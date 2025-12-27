#!/usr/bin/env node
"use strict";

/**
 * ONE v1.1 — Full “one for all” compiler (single-file, copy/paste)
 *
 * Modes:
 *   @backend (default)  -> compile + run on Bytecode VM, or build .onebc
 *   @frontend           -> compile to file.html + file.js (visible in browser)
 *
 * CLI:
 *   node one.js run   <file.one>
 *   node one.js build <file.one>
 *   node one.js fmt   <file.one>
 *   node one.js serve <file.one> [--port=3000]
 *   node one.js test
 *   node one.js init  [dir]
 *
 * Language core (v1.1):
 *   Types: number, string, bool, null, object, list
 *   let variables (block scoped, must declare before use)
 *   if/else, while, for(init;cond;update), break, continue, return
 *   fn declarations (hoisted), recursion + closures
 *   objects: { key: expr, ... }, obj.key, obj.key = expr
 *   lists: (a,b,c) syntax, methods: .get(i), .set(i,v), .len
 *   throw expr, try { ... } catch (e) { ... }
 *   modules: import "x" or let m = import("x"), export let / export fn
 *   optimizer: constant folding, branch pruning, dead code elimination (incl return-based)
 *   backend: bytecode VM
 *   frontend: <logic> full backend code (compiled + run in browser via embedded VM),
 *             <style> preserved, markup uses quoted text + {expr} interpolation,
 *             events via onClick="handler()" style attrs.
 *
 * Notes:
 *   - ONE does NOT use [] for indexing; lexer throws on '[' or ']'.
 *   - In @backend, '<' and '>' are comparison operators only. Markup-like usage
 *     becomes a syntax error naturally (no pre-regex false-positives).
 */

const fs = require("fs");
const path = require("path");
const http = require("http");
const url = require("url");

// ============================================================
// 0) Utilities + Errors
// ============================================================

function panic(msg, code = 1) {
  console.error(msg);
  process.exit(code);
}

function readFileText(p) {
  try {
    return fs.readFileSync(p, "utf8");
  } catch (e) {
    panic(`Could not read file: ${p}\n${String(e?.message || e)}`);
  }
}

function writeFileText(p, content) {
  try {
    fs.writeFileSync(p, content, "utf8");
  } catch (e) {
    panic(`Could not write file: ${p}\n${String(e?.message || e)}`);
  }
}

function fileExists(p) {
  try {
    fs.accessSync(p, fs.constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

function mkdirp(dir) {
  try {
    fs.mkdirSync(dir, { recursive: true });
  } catch { }
}

function indexToLineCol(src, idx) {
  let line = 1, col = 1;
  for (let i = 0; i < idx && i < src.length; i++) {
    const ch = src[i];
    if (ch === "\n") { line++; col = 1; }
    else col++;
  }
  return { line, col };
}

class OneError extends Error {
  constructor(message, fileName, line, col, kind = "error") {
    super(message);
    this.name = "OneError";
    this.fileName = fileName || "<unknown>";
    this.line = line || 1;
    this.col = col || 1;
    this.kind = kind;
  }
}

function throwErr(fileName, line, col, message, kind = "error") {
  throw new OneError(message, fileName, line, col, kind);
}

function formatError(err, src) {
  if (!(err instanceof OneError)) return String(err?.stack || err);
  const lines = String(src || "").split(/\r?\n/);
  const lineText = lines[err.line - 1] ?? "";
  const caretPad = " ".repeat(Math.max(0, err.col - 1));
  return [
    `${err.fileName}:${err.line}:${err.col} - ${err.kind}: ${err.message}`,
    lineText,
    `${caretPad}^`,
  ].join("\n");
}

function findHeaderMode(src) {
  const trimmed = String(src).trimStart();
  if (trimmed.startsWith("@frontend")) return "frontend";
  if (trimmed.startsWith("@backend")) return "backend";
  return "backend";
}

function stripHeaderLine(src) {
  return String(src).replace(/^\s*@(?:backend|frontend)[^\n]*\n/, "");
}

function isWhitespace(ch) { return ch === " " || ch === "\t" || ch === "\r"; }
function isNewline(ch) { return ch === "\n"; }
function isDigit(ch) { return ch >= "0" && ch <= "9"; }
function isAlpha(ch) { return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z"); }
function isIdentStart(ch) { return isAlpha(ch) || ch === "_"; }
function isIdentPart(ch) { return isIdentStart(ch) || isDigit(ch); }

// ============================================================
// 1) Tokens + Lexer
// ============================================================

const TokenKind = Object.freeze({
  EOF: "EOF",
  NEWLINE: "NEWLINE",
  SEMI: "SEMI",

  IDENT: "IDENT",
  NUMBER: "NUMBER",
  STRING: "STRING",

  // keywords
  KW_LET: "KW_LET",
  KW_FN: "KW_FN",
  KW_IF: "KW_IF",
  KW_ELSE: "KW_ELSE",
  KW_WHILE: "KW_WHILE",
  KW_FOR: "KW_FOR",
  KW_BREAK: "KW_BREAK",
  KW_CONTINUE: "KW_CONTINUE",
  KW_RETURN: "KW_RETURN",
  KW_TRUE: "KW_TRUE",
  KW_FALSE: "KW_FALSE",
  KW_NULL: "KW_NULL",
  KW_IMPORT: "KW_IMPORT",
  KW_EXPORT: "KW_EXPORT",
  KW_THROW: "KW_THROW",
  KW_TRY: "KW_TRY",
  KW_CATCH: "KW_CATCH",

  // symbols/operators
  LPAREN: "LPAREN",
  RPAREN: "RPAREN",
  LBRACE: "LBRACE",
  RBRACE: "RBRACE",
  COMMA: "COMMA",
  DOT: "DOT",
  COLON: "COLON",

  EQ: "EQ",
  EQEQ: "EQEQ",
  BANGEQ: "BANGEQ",
  LT: "LT",
  LTE: "LTE",
  GT: "GT",
  GTE: "GTE",

  PLUS: "PLUS",
  MINUS: "MINUS",
  STAR: "STAR",
  SLASH: "SLASH",
  BANG: "BANG",
});

const KEYWORDS = new Map([
  ["let", TokenKind.KW_LET],
  ["fn", TokenKind.KW_FN],
  ["if", TokenKind.KW_IF],
  ["else", TokenKind.KW_ELSE],
  ["while", TokenKind.KW_WHILE],
  ["for", TokenKind.KW_FOR],
  ["break", TokenKind.KW_BREAK],
  ["continue", TokenKind.KW_CONTINUE],
  ["return", TokenKind.KW_RETURN],
  ["true", TokenKind.KW_TRUE],
  ["false", TokenKind.KW_FALSE],
  ["null", TokenKind.KW_NULL],
  ["import", TokenKind.KW_IMPORT],
  ["export", TokenKind.KW_EXPORT],
  ["throw", TokenKind.KW_THROW],
  ["try", TokenKind.KW_TRY],
  ["catch", TokenKind.KW_CATCH],
]);

class Token {
  constructor(kind, value, line, col) {
    this.kind = kind;
    this.value = value;
    this.line = line;
    this.col = col;
  }
}

class Lexer {
  constructor(fileName, src) {
    this.fileName = fileName;
    this.src = String(src);
    this.i = 0;
    this.line = 1;
    this.col = 1;
  }

  peek(offset = 0) {
    const idx = this.i + offset;
    return idx >= 0 && idx < this.src.length ? this.src[idx] : "\0";
  }

  advance() {
    const ch = this.peek();
    this.i++;
    if (ch === "\n") { this.line++; this.col = 1; }
    else this.col++;
    return ch;
  }

  make(kind, value, line, col) {
    return new Token(kind, value, line, col);
  }

  lexString() {
    const startLine = this.line, startCol = this.col;
    this.advance(); // "
    let out = "";
    while (true) {
      const ch = this.peek();
      if (ch === "\0") throwErr(this.fileName, startLine, startCol, "Unterminated string literal");
      if (ch === "\"") { this.advance(); break; }
      if (ch === "\\") {
        this.advance();
        const esc = this.peek();
        if (esc === "\0") throwErr(this.fileName, startLine, startCol, "Unterminated escape sequence");
        this.advance();
        switch (esc) {
          case "n": out += "\n"; break;
          case "t": out += "\t"; break;
          case "\"": out += "\""; break;
          case "\\": out += "\\"; break;
          default: out += esc; break;
        }
      } else {
        out += this.advance();
      }
    }
    return this.make(TokenKind.STRING, out, startLine, startCol);
  }

  lexNumber() {
    const startLine = this.line, startCol = this.col;
    let s = "";
    while (isDigit(this.peek())) s += this.advance();
    if (this.peek() === "." && isDigit(this.peek(1))) {
      s += this.advance();
      while (isDigit(this.peek())) s += this.advance();
    }
    return this.make(TokenKind.NUMBER, Number(s), startLine, startCol);
  }

  lexIdentOrKeyword() {
    const startLine = this.line, startCol = this.col;
    let s = "";
    while (isIdentPart(this.peek())) s += this.advance();
    const kw = KEYWORDS.get(s);
    if (kw) return this.make(kw, s, startLine, startCol);
    return this.make(TokenKind.IDENT, s, startLine, startCol);
  }

  nextToken() {
    while (true) {
      const ch = this.peek();
      if (ch === "\0") return this.make(TokenKind.EOF, null, this.line, this.col);

      if (isWhitespace(ch)) { this.advance(); continue; }

      // comments
      if (ch === "/" && this.peek(1) === "/") {
        while (!isNewline(this.peek()) && this.peek() !== "\0") this.advance();
        continue;
      }

      if (isNewline(ch)) {
        const line = this.line, col = this.col;
        this.advance();
        return this.make(TokenKind.NEWLINE, "\n", line, col);
      }
      if (ch === ";") {
        const line = this.line, col = this.col;
        this.advance();
        return this.make(TokenKind.SEMI, ";", line, col);
      }

      if (ch === "\"") return this.lexString();
      if (isDigit(ch)) return this.lexNumber();
      if (isIdentStart(ch)) return this.lexIdentOrKeyword();

      // ONE does not use []
      if (ch === "[" || ch === "]") {
        const line = this.line, col = this.col;
        throwErr(this.fileName, line, col, "ONE does not use '[' or ']'. Use list syntax '(a, b, c)' and list methods.");
      }

      const line = this.line, col = this.col;

      if (ch === "=" && this.peek(1) === "=") { this.advance(); this.advance(); return this.make(TokenKind.EQEQ, "==", line, col); }
      if (ch === "!" && this.peek(1) === "=") { this.advance(); this.advance(); return this.make(TokenKind.BANGEQ, "!=", line, col); }
      if (ch === "<" && this.peek(1) === "=") { this.advance(); this.advance(); return this.make(TokenKind.LTE, "<=", line, col); }
      if (ch === ">" && this.peek(1) === "=") { this.advance(); this.advance(); return this.make(TokenKind.GTE, ">=", line, col); }

      switch (ch) {
        case "(": this.advance(); return this.make(TokenKind.LPAREN, "(", line, col);
        case ")": this.advance(); return this.make(TokenKind.RPAREN, ")", line, col);
        case "{": this.advance(); return this.make(TokenKind.LBRACE, "{", line, col);
        case "}": this.advance(); return this.make(TokenKind.RBRACE, "}", line, col);
        case ",": this.advance(); return this.make(TokenKind.COMMA, ",", line, col);
        case ".": this.advance(); return this.make(TokenKind.DOT, ".", line, col);
        case ":": this.advance(); return this.make(TokenKind.COLON, ":", line, col);
        case "=": this.advance(); return this.make(TokenKind.EQ, "=", line, col);
        case "<": this.advance(); return this.make(TokenKind.LT, "<", line, col);
        case ">": this.advance(); return this.make(TokenKind.GT, ">", line, col);
        case "+": this.advance(); return this.make(TokenKind.PLUS, "+", line, col);
        case "-": this.advance(); return this.make(TokenKind.MINUS, "-", line, col);
        case "*": this.advance(); return this.make(TokenKind.STAR, "*", line, col);
        case "/": this.advance(); return this.make(TokenKind.SLASH, "/", line, col);
        case "!": this.advance(); return this.make(TokenKind.BANG, "!", line, col);
        default:
          throwErr(this.fileName, line, col, `Unexpected character: '${ch}'`);
      }
    }
  }

  tokenizeAll() {
    const out = [];
    while (true) {
      const t = this.nextToken();
      out.push(t);
      if (t.kind === TokenKind.EOF) break;
    }
    return out;
  }
}

// ============================================================
// 2) AST helpers
// ============================================================

function node(kind, props) {
  return Object.freeze({ kind, ...props });
}

// ============================================================
// 3) Parser
// ============================================================

class Parser {
  constructor(fileName, tokens) {
    this.fileName = fileName;
    this.tokens = tokens;
    this.i = 0;
  }

  cur() { return this.tokens[this.i]; }
  at(kind) { return this.cur().kind === kind; }
  peekKind(offset = 1) { return (this.tokens[this.i + offset] || this.tokens[this.tokens.length - 1]).kind; }

  eat(kind, msg) {
    const t = this.cur();
    if (t.kind !== kind) {
      throwErr(this.fileName, t.line, t.col, msg || `Expected ${kind} but got ${t.kind}`);
    }
    this.i++;
    return t;
  }

  tryEat(kind) {
    if (this.at(kind)) return this.eat(kind);
    return null;
  }

  eatTerminators() {
    while (this.at(TokenKind.NEWLINE) || this.at(TokenKind.SEMI)) this.i++;
  }

  requireTerminator(msg) {
    if (this.at(TokenKind.SEMI) || this.at(TokenKind.NEWLINE)) { this.eatTerminators(); return; }
    if (this.at(TokenKind.RBRACE) || this.at(TokenKind.EOF)) return;
    const t = this.cur();
    throwErr(this.fileName, t.line, t.col, msg || "Expected end of statement (newline or ';')");
  }

  parseProgram() {
    const items = [];
    this.eatTerminators();
    while (!this.at(TokenKind.EOF)) {
      if (this.at(TokenKind.KW_EXPORT)) items.push(this.parseExportDecl());
      else if (this.at(TokenKind.KW_IMPORT)) items.push(this.parseImportDecl());
      else if (this.at(TokenKind.KW_FN)) items.push(this.parseFuncDecl(false));
      else items.push(this.parseStatement());
      this.eatTerminators();
    }
    return node("Program", { items });
  }

  parseExportDecl() {
    const kw = this.eat(TokenKind.KW_EXPORT);
    if (this.at(TokenKind.KW_LET)) return this.parseLet(true, kw.line, kw.col);
    if (this.at(TokenKind.KW_FN)) return this.parseFuncDecl(true, kw.line, kw.col);
    throwErr(this.fileName, kw.line, kw.col, "Expected 'let' or 'fn' after export");
  }

  parseImportDecl() {
    const kw = this.eat(TokenKind.KW_IMPORT);
    const p = this.eat(TokenKind.STRING, "Expected string after import");
    this.requireTerminator("Expected end of statement after import");
    return node("ImportDecl", { line: kw.line, col: kw.col, path: p.value });
  }

  parseFuncDecl(isExported, exportLine = null, exportCol = null) {
    const kw = this.eat(TokenKind.KW_FN);
    const nameTok = this.eat(TokenKind.IDENT, "Expected function name");
    const name = nameTok.value;

    this.eat(TokenKind.LPAREN, "Expected '(' after function name");
    const params = [];
    if (!this.at(TokenKind.RPAREN)) {
      while (true) {
        const pt = this.cur();
        const pName = this.eat(TokenKind.IDENT, "Expected parameter name").value;
        let pType = null;
        if (this.tryEat(TokenKind.COLON)) pType = this.parseTypeRef();
        params.push({ name: pName, typeRef: pType, line: pt.line, col: pt.col });
        if (this.tryEat(TokenKind.COMMA)) continue;
        break;
      }
    }
    this.eat(TokenKind.RPAREN, "Expected ')' after parameters");

    let returnType = null;
    if (this.tryEat(TokenKind.COLON)) returnType = this.parseTypeRef();

    const body = this.parseBlock();
    return node("FuncDecl", {
      line: kw.line, col: kw.col,
      name, params, returnType, body,
      exported: !!isExported,
      exportLine, exportCol
    });
  }

  parseTypeRef() {
    const t = this.cur();
    const nameTok = this.eat(TokenKind.IDENT, "Expected type name");
    return { name: nameTok.value, line: t.line, col: t.col };
  }

  parseStatement() {
    if (this.at(TokenKind.KW_LET)) return this.parseLet(false);
    if (this.at(TokenKind.KW_IF)) return this.parseIf();
    if (this.at(TokenKind.KW_WHILE)) return this.parseWhile();
    if (this.at(TokenKind.KW_FOR)) return this.parseFor();
    if (this.at(TokenKind.KW_BREAK)) return this.parseBreak();
    if (this.at(TokenKind.KW_CONTINUE)) return this.parseContinue();
    if (this.at(TokenKind.KW_RETURN)) return this.parseReturn();
    if (this.at(TokenKind.KW_THROW)) return this.parseThrow();
    if (this.at(TokenKind.KW_TRY)) return this.parseTryCatch();
    if (this.at(TokenKind.LBRACE)) return this.parseBlock();

    // assignment or expression statement:
    // allow: x = expr
    // allow: obj.key = expr   (member chain)
    if (this.at(TokenKind.IDENT)) {
      const save = this.i;
      const lv = this.parseAssignable();
      if (this.at(TokenKind.EQ)) {
        const eqTok = this.eat(TokenKind.EQ);
        const value = this.parseExpr();
        this.requireTerminator("Expected end of statement after assignment");
        return node("AssignStmt", { line: eqTok.line, col: eqTok.col, target: lv, value });
      }
      this.i = save;
    }

    const expr = this.parseExpr();
    this.requireTerminator("Expected end of statement after expression");
    return node("ExprStmt", { line: expr.line, col: expr.col, expr });
  }

  parseLet(isExported, exportLine = null, exportCol = null) {
    const kw = this.eat(TokenKind.KW_LET);
    const nameTok = this.eat(TokenKind.IDENT, "Expected identifier after let");
    let typeRef = null;
    if (this.tryEat(TokenKind.COLON)) typeRef = this.parseTypeRef();
    this.eat(TokenKind.EQ, "Expected '=' in let statement");
    const value = this.parseExpr();
    this.requireTerminator("Expected end of statement after let");
    return node("LetStmt", {
      line: kw.line, col: kw.col,
      name: nameTok.value,
      typeRef, value,
      exported: !!isExported,
      exportLine, exportCol
    });
  }

  parseIf() {
    const kw = this.eat(TokenKind.KW_IF);
    this.eat(TokenKind.LPAREN, "Expected '(' after if");
    const cond = this.parseExpr();
    this.eat(TokenKind.RPAREN, "Expected ')' after if condition");
    const thenBlock = this.parseBlock();
    let elseBlock = null;
    if (this.tryEat(TokenKind.KW_ELSE)) elseBlock = this.parseBlock();
    return node("IfStmt", { line: kw.line, col: kw.col, cond, thenBlock, elseBlock });
  }

  parseWhile() {
    const kw = this.eat(TokenKind.KW_WHILE);
    this.eat(TokenKind.LPAREN, "Expected '(' after while");
    const cond = this.parseExpr();
    this.eat(TokenKind.RPAREN, "Expected ')' after while condition");
    const body = this.parseBlock();
    return node("WhileStmt", { line: kw.line, col: kw.col, cond, body });
  }

  parseFor() {
    const kw = this.eat(TokenKind.KW_FOR);
    this.eat(TokenKind.LPAREN, "Expected '(' after for");
    // init: either empty, let, assignment, or expression-stmt-ish (no terminator; uses ';')
    let init = null;
    if (!this.at(TokenKind.SEMI)) {
      if (this.at(TokenKind.KW_LET)) init = this.parseLet(false);
      else {
        // parse assignment or expr as a "ForInit"
        if (this.at(TokenKind.IDENT)) {
          const save = this.i;
          const lv = this.parseAssignable();
          if (this.at(TokenKind.EQ)) {
            const eqTok = this.eat(TokenKind.EQ);
            const v = this.parseExpr();
            init = node("AssignStmt", { line: eqTok.line, col: eqTok.col, target: lv, value: v });
          } else {
            this.i = save;
            const ex = this.parseExpr();
            init = node("ExprStmt", { line: ex.line, col: ex.col, expr: ex });
          }
        } else {
          const ex = this.parseExpr();
          init = node("ExprStmt", { line: ex.line, col: ex.col, expr: ex });
        }
      }
    }
    this.eat(TokenKind.SEMI, "Expected ';' after for initializer");

    let cond = null;
    if (!this.at(TokenKind.SEMI)) cond = this.parseExpr();
    this.eat(TokenKind.SEMI, "Expected ';' after for condition");

    let update = null;
    if (!this.at(TokenKind.RPAREN)) {
      // update: assignment or expr
      if (this.at(TokenKind.IDENT)) {
        const save = this.i;
        const lv = this.parseAssignable();
        if (this.at(TokenKind.EQ)) {
          const eqTok = this.eat(TokenKind.EQ);
          const v = this.parseExpr();
          update = node("AssignStmt", { line: eqTok.line, col: eqTok.col, target: lv, value: v });
        } else {
          this.i = save;
          const ex = this.parseExpr();
          update = node("ExprStmt", { line: ex.line, col: ex.col, expr: ex });
        }
      } else {
        const ex = this.parseExpr();
        update = node("ExprStmt", { line: ex.line, col: ex.col, expr: ex });
      }
    }

    this.eat(TokenKind.RPAREN, "Expected ')' after for clauses");
    const body = this.parseBlock();
    return node("ForStmt", { line: kw.line, col: kw.col, init, cond, update, body });
  }

  parseBreak() {
    const kw = this.eat(TokenKind.KW_BREAK);
    this.requireTerminator("Expected end of statement after break");
    return node("BreakStmt", { line: kw.line, col: kw.col });
  }

  parseContinue() {
    const kw = this.eat(TokenKind.KW_CONTINUE);
    this.requireTerminator("Expected end of statement after continue");
    return node("ContinueStmt", { line: kw.line, col: kw.col });
  }

  parseReturn() {
    const kw = this.eat(TokenKind.KW_RETURN);
    if (this.at(TokenKind.NEWLINE) || this.at(TokenKind.SEMI) || this.at(TokenKind.RBRACE) || this.at(TokenKind.EOF)) {
      this.eatTerminators();
      return node("ReturnStmt", { line: kw.line, col: kw.col, expr: null });
    }
    const expr = this.parseExpr();
    this.requireTerminator("Expected end of statement after return");
    return node("ReturnStmt", { line: kw.line, col: kw.col, expr });
  }

  parseThrow() {
    const kw = this.eat(TokenKind.KW_THROW);
    const expr = this.parseExpr();
    this.requireTerminator("Expected end of statement after throw");
    return node("ThrowStmt", { line: kw.line, col: kw.col, expr });
  }

  parseTryCatch() {
    const kw = this.eat(TokenKind.KW_TRY);
    const tryBlock = this.parseBlock();
    this.eat(TokenKind.KW_CATCH, "Expected 'catch' after try block");
    this.eat(TokenKind.LPAREN, "Expected '(' after catch");
    const eTok = this.eat(TokenKind.IDENT, "Expected catch identifier");
    this.eat(TokenKind.RPAREN, "Expected ')' after catch identifier");
    const catchBlock = this.parseBlock();
    return node("TryCatchStmt", {
      line: kw.line, col: kw.col,
      tryBlock,
      catchName: eTok.value,
      catchLine: eTok.line,
      catchCol: eTok.col,
      catchBlock
    });
  }

  parseBlock() {
    const l = this.eat(TokenKind.LBRACE, "Expected '{' to start block");
    this.eatTerminators();
    const statements = [];
    while (!this.at(TokenKind.RBRACE)) {
      if (this.at(TokenKind.EOF)) throwErr(this.fileName, l.line, l.col, "Unterminated block (missing '}')");
      statements.push(this.parseStatement());
      this.eatTerminators();
    }
    this.eat(TokenKind.RBRACE, "Expected '}' to end block");
    return node("Block", { line: l.line, col: l.col, statements });
  }

  // ---- Expressions (precedence) ----
  parseExpr() { return this.parseEquality(); }

  parseEquality() {
    let expr = this.parseComparison();
    while (this.at(TokenKind.EQEQ) || this.at(TokenKind.BANGEQ)) {
      const opTok = this.cur(); this.i++;
      const right = this.parseComparison();
      expr = node("Binary", { line: opTok.line, col: opTok.col, op: opTok.kind, left: expr, right });
    }
    return expr;
  }

  parseComparison() {
    let expr = this.parseTerm();
    while (this.at(TokenKind.LT) || this.at(TokenKind.LTE) || this.at(TokenKind.GT) || this.at(TokenKind.GTE)) {
      const opTok = this.cur(); this.i++;
      const right = this.parseTerm();
      expr = node("Binary", { line: opTok.line, col: opTok.col, op: opTok.kind, left: expr, right });
    }
    return expr;
  }

  parseTerm() {
    let expr = this.parseFactor();
    while (this.at(TokenKind.PLUS) || this.at(TokenKind.MINUS)) {
      const opTok = this.cur(); this.i++;
      const right = this.parseFactor();
      expr = node("Binary", { line: opTok.line, col: opTok.col, op: opTok.kind, left: expr, right });
    }
    return expr;
  }

  parseFactor() {
    let expr = this.parseUnary();
    while (this.at(TokenKind.STAR) || this.at(TokenKind.SLASH)) {
      const opTok = this.cur(); this.i++;
      const right = this.parseUnary();
      expr = node("Binary", { line: opTok.line, col: opTok.col, op: opTok.kind, left: expr, right });
    }
    return expr;
  }

  parseUnary() {
    if (this.at(TokenKind.BANG) || this.at(TokenKind.MINUS)) {
      const opTok = this.cur(); this.i++;
      const expr = this.parseUnary();
      return node("Unary", { line: opTok.line, col: opTok.col, op: opTok.kind, expr });
    }
    return this.parseCallMember();
  }

  parseCallMember() {
    let expr = this.parsePrimary();
    while (true) {
      if (this.at(TokenKind.LPAREN)) {
        const lp = this.eat(TokenKind.LPAREN);
        const args = [];
        if (!this.at(TokenKind.RPAREN)) {
          while (true) {
            args.push(this.parseExpr());
            if (this.tryEat(TokenKind.COMMA)) continue;
            break;
          }
        }
        this.eat(TokenKind.RPAREN, "Expected ')' after arguments");
        expr = node("Call", { line: lp.line, col: lp.col, callee: expr, args });
        continue;
      }
      if (this.tryEat(TokenKind.DOT)) {
        const propTok = this.eat(TokenKind.IDENT, "Expected property after '.'");
        expr = node("Member", { line: propTok.line, col: propTok.col, object: expr, prop: propTok.value });
        continue;
      }
      break;
    }
    return expr;
  }

  parsePrimary() {
    const t = this.cur();

    if (this.at(TokenKind.NUMBER)) { this.i++; return node("Literal", { line: t.line, col: t.col, value: t.value, litType: "number" }); }
    if (this.at(TokenKind.STRING)) { this.i++; return node("Literal", { line: t.line, col: t.col, value: t.value, litType: "string" }); }
    if (this.at(TokenKind.KW_TRUE)) { this.i++; return node("Literal", { line: t.line, col: t.col, value: true, litType: "bool" }); }
    if (this.at(TokenKind.KW_FALSE)) { this.i++; return node("Literal", { line: t.line, col: t.col, value: false, litType: "bool" }); }
    if (this.at(TokenKind.KW_NULL)) { this.i++; return node("Literal", { line: t.line, col: t.col, value: null, litType: "null" }); }

    if (this.at(TokenKind.IDENT)) { this.i++; return node("Var", { line: t.line, col: t.col, name: t.value }); }

    // list literal: (a, b, c) OR () empty
    if (this.at(TokenKind.LPAREN)) {
      const lp = this.eat(TokenKind.LPAREN);
      if (this.at(TokenKind.RPAREN)) {
        this.eat(TokenKind.RPAREN);
        return node("List", { line: lp.line, col: lp.col, items: [] });
      }
      const first = this.parseExpr();
      if (this.tryEat(TokenKind.COMMA)) {
        const items = [first];
        while (true) {
          items.push(this.parseExpr());
          if (this.tryEat(TokenKind.COMMA)) continue;
          break;
        }
        this.eat(TokenKind.RPAREN, "Expected ')' after list");
        return node("List", { line: lp.line, col: lp.col, items });
      }
      this.eat(TokenKind.RPAREN, "Expected ')' after expression");
      return first;
    }

    // object literal: { key: expr }
    if (this.at(TokenKind.LBRACE)) {
      const lb = this.eat(TokenKind.LBRACE);

      this.eatTerminators(); // allow newline after {

      const props = [];

      while (!this.at(TokenKind.RBRACE)) {
        const kt = this.cur();
        let key;

        if (this.at(TokenKind.IDENT)) key = this.eat(TokenKind.IDENT).value;
        else if (this.at(TokenKind.STRING)) key = this.eat(TokenKind.STRING).value;
        else throwErr(
          this.fileName,
          kt.line,
          kt.col,
          "Expected object key (identifier or string)"
        );

        this.eat(TokenKind.COLON, "Expected ':' after object key");
        const val = this.parseExpr();
        props.push({ key, value: val, line: kt.line, col: kt.col });

        this.eatTerminators(); // allow newline after property

        // Optional comma support
        this.tryEat(TokenKind.COMMA);
        this.eatTerminators();
      }

      this.eat(TokenKind.RBRACE, "Expected '}' to close object literal");
      return node("Object", { line: lb.line, col: lb.col, props });
    }
    
    // Friendly message for backend accidental markup at statement start:
    if (this.at(TokenKind.LT)) {
      throwErr(this.fileName, t.line, t.col, "Unexpected '<'. In @backend, '<' starts a comparison inside an expression like '(a < b)'. Markup is only allowed in @frontend.");
    }

    if (this.at(TokenKind.EQ)) {
      throwErr(this.fileName, t.line, t.col, "Unexpected '=' in expression. Did you mean '==' for comparison?");
    }

    throwErr(this.fileName, t.line, t.col, `Unexpected token in expression: ${t.kind}`);
  }

  // Parse something assignable: Var or Member chain
  parseAssignable() {
    const t = this.cur();
    if (!this.at(TokenKind.IDENT)) throwErr(this.fileName, t.line, t.col, "Expected identifier");
    let expr = node("Var", { line: t.line, col: t.col, name: this.eat(TokenKind.IDENT).value });
    while (this.tryEat(TokenKind.DOT)) {
      const p = this.eat(TokenKind.IDENT, "Expected property after '.'");
      expr = node("Member", { line: p.line, col: p.col, object: expr, prop: p.value });
    }
    return expr;
  }
}

// ============================================================
// 4) Analyzer (decl-before-use, function hoisting, loop legality)
// ============================================================

function typeOfValue(v) {
  if (v === null) return "null";
  if (Array.isArray(v)) return "list";
  switch (typeof v) {
    case "number": return "number";
    case "string": return "string";
    case "boolean": return "bool";
    case "object": return "object";
    case "function": return "function";
    default: return "unknown";
  }
}

function isBuiltinName(name) {
  return name === "print" || name === "import" || name === "load";
}

class Scope {
  constructor(parent = null) {
    this.parent = parent;
    this.vars = new Map();  // name -> { declaredType? }
    this.funcs = new Map(); // name -> true
  }
  hasVarLocal(n) { return this.vars.has(n); }
  hasFuncLocal(n) { return this.funcs.has(n); }
  hasVar(n) { return this.vars.has(n) || (this.parent ? this.parent.hasVar(n) : false); }
  hasFunc(n) { return this.funcs.has(n) || (this.parent ? this.parent.hasFunc(n) : false); }
  getVarType(n) {
    if (this.vars.has(n)) return this.vars.get(n);
    return this.parent ? this.parent.getVarType(n) : null;
  }
}

function analyzeProgram(fileName, src, program) {
  // Build top scope with hoisted funcs
  const top = new Scope(null);

  // builtins
  top.funcs.set("print", true);
  top.funcs.set("import", true);
  top.funcs.set("load", true);

  // hoist top-level function declarations
  for (const it of program.items) {
    if (it.kind === "FuncDecl") top.funcs.set(it.name, true);
    if (it.kind === "FuncDecl" && it.exported) { } // export ok
  }

  // analyze items sequentially (lets must be declared before use)
  for (const it of program.items) {
    analyzeTopItem(fileName, src, it, top, { inLoop: false, inFunc: false });
  }

  return program;
}

function analyzeTopItem(fileName, src, it, scope, ctx) {
  switch (it.kind) {
    case "FuncDecl":
      analyzeFuncDecl(fileName, src, it, scope);
      return;
    case "ImportDecl":
      // ok anywhere top-level
      return;
    default:
      analyzeStmt(fileName, src, it, scope, ctx);
      return;
  }
}

function analyzeFuncDecl(fileName, src, fnDecl, parentScope) {
  // function body scope: params are declared at start
  const fScope = new Scope(parentScope);

  // hoist nested function declarations inside body blocks is handled per-block
  // Declare params
  for (const p of fnDecl.params) {
    if (fScope.hasVarLocal(p.name) || fScope.hasFuncLocal(p.name)) {
      throwErr(fileName, p.line, p.col, `Redeclaration error: '${p.name}' already defined in function scope`);
    }
    fScope.vars.set(p.name, p.typeRef ? { kind: p.typeRef.name } : null);
  }

  analyzeBlock(fileName, src, fnDecl.body, fScope, { inLoop: false, inFunc: true, returnType: fnDecl.returnType ? fnDecl.returnType.name : null });
}

function analyzeBlock(fileName, src, block, parentScope, ctx) {
  // block scope with hoisted funcs in that block
  const bScope = new Scope(parentScope);

  for (const st of block.statements) {
    if (st.kind === "FuncDecl") bScope.funcs.set(st.name, true); // (not typically in grammar as statement, but safe)
  }

  for (const st of block.statements) {
    analyzeStmt(fileName, src, st, bScope, ctx);
  }
}

function analyzeStmt(fileName, src, st, scope, ctx) {
  switch (st.kind) {
    case "LetStmt": {
      // value can reference prior vars, not itself
      analyzeExpr(fileName, src, st.value, scope);
      if (scope.hasVarLocal(st.name) || scope.hasFuncLocal(st.name)) {
        throwErr(fileName, st.line, st.col, `Redeclaration error: '${st.name}' is already defined in this scope`);
      }
      scope.vars.set(st.name, st.typeRef ? { kind: st.typeRef.name } : null);
      return;
    }

    case "AssignStmt":
      analyzeAssignable(fileName, src, st.target, scope);
      analyzeExpr(fileName, src, st.value, scope);
      return;

    case "ExprStmt":
      analyzeExpr(fileName, src, st.expr, scope);
      return;

    case "IfStmt":
      analyzeExpr(fileName, src, st.cond, scope);
      analyzeBlock(fileName, src, st.thenBlock, scope, ctx);
      if (st.elseBlock) analyzeBlock(fileName, src, st.elseBlock, scope, ctx);
      return;

    case "WhileStmt":
      analyzeExpr(fileName, src, st.cond, scope);
      analyzeBlock(fileName, src, st.body, scope, { ...ctx, inLoop: true });
      return;

    case "ForStmt":
      // init runs in for-scope, but init's lets must not escape; we model it as block scope:
      {
        const forScope = new Scope(scope);
        // hoist funcs inside for body handled by analyzeBlock
        if (st.init) analyzeStmtLikeForClause(fileName, src, st.init, forScope, ctx);
        if (st.cond) analyzeExpr(fileName, src, st.cond, forScope);
        if (st.update) analyzeStmtLikeForClause(fileName, src, st.update, forScope, ctx);
        analyzeBlock(fileName, src, st.body, forScope, { ...ctx, inLoop: true });
      }
      return;

    case "BreakStmt":
      if (!ctx.inLoop) throwErr(fileName, st.line, st.col, "'break' may only appear inside a loop");
      return;

    case "ContinueStmt":
      if (!ctx.inLoop) throwErr(fileName, st.line, st.col, "'continue' may only appear inside a loop");
      return;

    case "ReturnStmt":
      if (!ctx.inFunc) {
        // allow top-level returns? v1.1: disallow
        throwErr(fileName, st.line, st.col, "'return' may only appear inside a function");
      }
      if (st.expr) analyzeExpr(fileName, src, st.expr, scope);
      return;

    case "ThrowStmt":
      analyzeExpr(fileName, src, st.expr, scope);
      return;

    case "TryCatchStmt": {
      analyzeBlock(fileName, src, st.tryBlock, scope, ctx);
      // catch has its own scope with catch var declared at start
      const catchScope = new Scope(scope);
      if (catchScope.hasVarLocal(st.catchName) || catchScope.hasFuncLocal(st.catchName)) {
        throwErr(fileName, st.catchLine, st.catchCol, `Redeclaration error: '${st.catchName}' already defined`);
      }
      catchScope.vars.set(st.catchName, null);
      analyzeBlock(fileName, src, st.catchBlock, catchScope, ctx);
      return;
    }

    case "Block":
      analyzeBlock(fileName, src, st, scope, ctx);
      return;

    case "ImportDecl":
      // allowed in blocks too
      return;

    case "FuncDecl":
      // if ever appears as statement (future), handle
      analyzeFuncDecl(fileName, src, st, scope);
      return;

    default:
      throwErr(fileName, st.line || 1, st.col || 1, `Analyzer: unknown statement kind: ${st.kind}`);
  }
}

function analyzeStmtLikeForClause(fileName, src, st, scope, ctx) {
  // for clauses are parsed into LetStmt/AssignStmt/ExprStmt already
  if (st.kind === "LetStmt") {
    analyzeExpr(fileName, src, st.value, scope);
    if (scope.hasVarLocal(st.name) || scope.hasFuncLocal(st.name)) {
      throwErr(fileName, st.line, st.col, `Redeclaration error: '${st.name}' is already defined in this scope`);
    }
    scope.vars.set(st.name, st.typeRef ? { kind: st.typeRef.name } : null);
    return;
  }
  if (st.kind === "AssignStmt") {
    analyzeAssignable(fileName, src, st.target, scope);
    analyzeExpr(fileName, src, st.value, scope);
    return;
  }
  if (st.kind === "ExprStmt") {
    analyzeExpr(fileName, src, st.expr, scope);
    return;
  }
  throwErr(fileName, st.line || 1, st.col || 1, "Invalid for-clause statement");
}

function analyzeAssignable(fileName, src, target, scope) {
  // Var or Member chain, root must be declared var
  if (target.kind === "Var") {
    const name = target.name;
    if (!scope.hasVar(name) && !scope.hasFunc(name) && !isBuiltinName(name)) {
      throwErr(fileName, target.line, target.col, `Assignment to undefined variable: ${name}`);
    }
    if (!scope.hasVar(name) && (scope.hasFunc(name) || isBuiltinName(name))) {
      throwErr(fileName, target.line, target.col, `Cannot assign to function/builtin '${name}'`);
    }
    return;
  }
  if (target.kind === "Member") {
    // ensure base resolves
    analyzeExpr(fileName, src, target.object, scope);
    return;
  }
  throwErr(fileName, target.line, target.col, "Invalid assignment target");
}

function analyzeExpr(fileName, src, expr, scope) {
  switch (expr.kind) {
    case "Literal":
      return;

    case "Var": {
      const n = expr.name;
      if (!scope.hasVar(n) && !scope.hasFunc(n) && !isBuiltinName(n)) {
        throwErr(fileName, expr.line, expr.col, `Undefined identifier: ${n}`);
      }
      return;
    }

    case "Unary":
      analyzeExpr(fileName, src, expr.expr, scope);
      return;

    case "Binary":
      analyzeExpr(fileName, src, expr.left, scope);
      analyzeExpr(fileName, src, expr.right, scope);
      return;

    case "Call":
      analyzeExpr(fileName, src, expr.callee, scope);
      for (const a of expr.args) analyzeExpr(fileName, src, a, scope);
      return;

    case "Member":
      analyzeExpr(fileName, src, expr.object, scope);
      return;

    case "List":
      for (const it of expr.items) analyzeExpr(fileName, src, it, scope);
      return;

    case "Object":
      for (const p of expr.props) analyzeExpr(fileName, src, p.value, scope);
      return;

    default:
      throwErr(fileName, expr.line || 1, expr.col || 1, `Analyzer: unknown expr kind: ${expr.kind}`);
  }
}

// ============================================================
// 5) Optimizer (constant folding, branch pruning, DCE)
// ============================================================

function isPureExpr(e) {
  // Conservative purity: literals, vars, unary/binary of pure, list/object of pure.
  // Calls/member access are not pure (could throw).
  switch (e.kind) {
    case "Literal": return true;
    case "Var": return true;
    case "Unary": return isPureExpr(e.expr);
    case "Binary": return isPureExpr(e.left) && isPureExpr(e.right);
    case "List": return e.items.every(isPureExpr);
    case "Object": return e.props.every(p => isPureExpr(p.value));
    default: return false;
  }
}

function foldBinary(op, a, b) {
  // returns {ok, value, litType}
  const ta = typeOfValue(a), tb = typeOfValue(b);
  try {
    switch (op) {
      case TokenKind.PLUS:
        if (ta === "number" && tb === "number") return { ok: true, value: a + b, litType: "number" };
        if (ta === "string" && tb === "string") return { ok: true, value: a + b, litType: "string" };
        return { ok: false };
      case TokenKind.MINUS:
        if (ta === "number" && tb === "number") return { ok: true, value: a - b, litType: "number" };
        return { ok: false };
      case TokenKind.STAR:
        if (ta === "number" && tb === "number") return { ok: true, value: a * b, litType: "number" };
        return { ok: false };
      case TokenKind.SLASH:
        if (ta === "number" && tb === "number") return { ok: true, value: a / b, litType: "number" };
        return { ok: false };
      case TokenKind.EQEQ:
        if (ta !== tb) return { ok: true, value: false, litType: "bool" };
        return { ok: true, value: a === b, litType: "bool" };
      case TokenKind.BANGEQ:
        if (ta !== tb) return { ok: true, value: true, litType: "bool" };
        return { ok: true, value: a !== b, litType: "bool" };
      case TokenKind.LT:
        if ((ta === "number" && tb === "number") || (ta === "string" && tb === "string"))
          return { ok: true, value: a < b, litType: "bool" };
        return { ok: false };
      case TokenKind.LTE:
        if ((ta === "number" && tb === "number") || (ta === "string" && tb === "string"))
          return { ok: true, value: a <= b, litType: "bool" };
        return { ok: false };
      case TokenKind.GT:
        if ((ta === "number" && tb === "number") || (ta === "string" && tb === "string"))
          return { ok: true, value: a > b, litType: "bool" };
        return { ok: false };
      case TokenKind.GTE:
        if ((ta === "number" && tb === "number") || (ta === "string" && tb === "string"))
          return { ok: true, value: a >= b, litType: "bool" };
        return { ok: false };
      default:
        return { ok: false };
    }
  } catch {
    return { ok: false };
  }
}

function optimizeProgram(program) {
  const items = [];
  for (const it of program.items) {
    if (it.kind === "FuncDecl") {
      const body = optimizeBlock(it.body);
      items.push(node("FuncDecl", { ...it, body }));
    } else if (it.kind === "ImportDecl") {
      items.push(it);
    } else {
      const st = optimizeStmt(it);
      if (st) items.push(st);
    }
  }
  return node("Program", { items });
}

function optimizeBlock(block) {
  const out = [];
  let terminated = false;
  for (const st of block.statements) {
    if (terminated) {
      // return-based DCE
      continue;
    }
    const o = optimizeStmt(st);
    if (!o) continue;

    out.push(o);

    if (o.kind === "ReturnStmt" || o.kind === "ThrowStmt" || o.kind === "BreakStmt" || o.kind === "ContinueStmt") {
      terminated = true;
    }
  }
  return node("Block", { ...block, statements: out });
}

function optimizeStmt(st) {
  switch (st.kind) {
    case "LetStmt": {
      const v = optimizeExpr(st.value);
      // DCE: remove unused let only if initializer is pure AND literal-ish (very safe)
      // We don't have full liveness; keep lets. (Production-grade conservative)
      return node("LetStmt", { ...st, value: v });
    }

    case "AssignStmt":
      return node("AssignStmt", { ...st, target: st.target, value: optimizeExpr(st.value) });

    case "ExprStmt": {
      const e = optimizeExpr(st.expr);
      // DCE: drop pure expressions that have no effect
      if (isPureExpr(e)) return null;
      return node("ExprStmt", { ...st, expr: e });
    }

    case "IfStmt": {
      const cond = optimizeExpr(st.cond);
      const thenB = optimizeBlock(st.thenBlock);
      const elseB = st.elseBlock ? optimizeBlock(st.elseBlock) : null;
      // branch prune if cond is literal bool
      if (cond.kind === "Literal" && typeof cond.value === "boolean") {
        return cond.value ? thenB : (elseB ? elseB : null);
      }
      return node("IfStmt", { ...st, cond, thenBlock: thenB, elseBlock: elseB });
    }

    case "WhileStmt": {
      const cond = optimizeExpr(st.cond);
      const body = optimizeBlock(st.body);
      // remove while(false)
      if (cond.kind === "Literal" && cond.value === false) return null;
      return node("WhileStmt", { ...st, cond, body });
    }

    case "ForStmt": {
      const init = st.init ? optimizeStmt(st.init) : null;
      const cond = st.cond ? optimizeExpr(st.cond) : null;
      const update = st.update ? optimizeStmt(st.update) : null;
      const body = optimizeBlock(st.body);

      // prune for(;false;)
      if (cond && cond.kind === "Literal" && cond.value === false) {
        // still keep init if it has effects
        if (init && init.kind === "ExprStmt" && isPureExpr(init.expr)) return null;
        return init ? node("Block", { line: st.line, col: st.col, statements: [init] }) : null;
      }

      return node("ForStmt", { ...st, init, cond, update, body });
    }

    case "ReturnStmt":
      return node("ReturnStmt", { ...st, expr: st.expr ? optimizeExpr(st.expr) : null });

    case "ThrowStmt":
      return node("ThrowStmt", { ...st, expr: optimizeExpr(st.expr) });

    case "TryCatchStmt":
      return node("TryCatchStmt", {
        ...st,
        tryBlock: optimizeBlock(st.tryBlock),
        catchBlock: optimizeBlock(st.catchBlock),
      });

    case "Block":
      return optimizeBlock(st);

    case "ImportDecl":
    case "BreakStmt":
    case "ContinueStmt":
      return st;

    default:
      return st;
  }
}

function optimizeExpr(e) {
  switch (e.kind) {
    case "Literal":
    case "Var":
      return e;

    case "Unary": {
      const inner = optimizeExpr(e.expr);
      if (inner.kind === "Literal") {
        if (e.op === TokenKind.MINUS && typeof inner.value === "number") {
          return node("Literal", { line: e.line, col: e.col, value: -inner.value, litType: "number" });
        }
        if (e.op === TokenKind.BANG && typeof inner.value === "boolean") {
          return node("Literal", { line: e.line, col: e.col, value: !inner.value, litType: "bool" });
        }
      }
      return node("Unary", { ...e, expr: inner });
    }

    case "Binary": {
      const left = optimizeExpr(e.left);
      const right = optimizeExpr(e.right);
      if (left.kind === "Literal" && right.kind === "Literal") {
        const r = foldBinary(e.op, left.value, right.value);
        if (r.ok) return node("Literal", { line: e.line, col: e.col, value: r.value, litType: r.litType });
      }
      return node("Binary", { ...e, left, right });
    }

    case "Call":
      return node("Call", { ...e, callee: optimizeExpr(e.callee), args: e.args.map(optimizeExpr) });

    case "Member":
      return node("Member", { ...e, object: optimizeExpr(e.object) });

    case "List":
      return node("List", { ...e, items: e.items.map(optimizeExpr) });

    case "Object":
      return node("Object", { ...e, props: e.props.map(p => ({ ...p, value: optimizeExpr(p.value) })) });

    default:
      return e;
  }
}

// ============================================================
// 6) Bytecode + VM
// ============================================================

const Op = Object.freeze({
  NOP: 0,

  // stack / const
  CONST: 1,
  POP: 2,

  // variables
  LOAD: 3,   // name
  STORE: 4,  // name (defines if exists? store requires existing)
  DEF: 5,    // name (define new)

  // member ops
  GETPROP: 6,  // propName
  SETPROP: 7,  // propName

  // list ops
  MAKELIST: 8, // count
  LIST_GET: 9,
  LIST_SET: 10,
  LEN: 11,

  // object ops
  MAKEOBJ: 12, // count pairs: key(const string), value(expr)
  // arithmetic / compare
  ADD: 13,
  SUB: 14,
  MUL: 15,
  DIV: 16,
  NEG: 17,
  NOT: 18,
  EQ: 19,
  NEQ: 20,
  LT: 21,
  LTE: 22,
  GT: 23,
  GTE: 24,

  // control flow
  JMP: 25,
  JMPF: 26, // jump if false (expects bool)
  // calls
  CLOSURE: 27, // fnIndex
  CALL: 28,    // argc
  RET: 29,

  // exceptions
  TRY: 30,      // catchIp
  ENDTRY: 31,
  THROW: 32,

  // loop control
  BRK: 33,
  CONT: 34,
});

function opName(op) {
  for (const [k, v] of Object.entries(Op)) if (v === op) return k;
  return `OP_${op}`;
}

class Cell { constructor(v) { this.v = v; } }

class Env {
  constructor(parent = null) {
    this.parent = parent;
    this.map = Object.create(null); // name -> Cell
    this.types = Object.create(null); // name -> declaredType string or null
  }
  hasLocal(name) { return Object.prototype.hasOwnProperty.call(this.map, name); }
  getCell(name) {
    if (Object.prototype.hasOwnProperty.call(this.map, name)) return this.map[name];
    if (this.parent) return this.parent.getCell(name);
    return null;
  }
  define(name, value, declaredType = null) {
    this.map[name] = new Cell(value);
    this.types[name] = declaredType;
  }
  set(name, value) {
    const c = this.getCell(name);
    if (!c) return false;
    c.v = value;
    return true;
  }
  get(name) {
    const c = this.getCell(name);
    return c ? c.v : undefined;
  }
  getDeclaredType(name) {
    if (Object.prototype.hasOwnProperty.call(this.types, name)) return this.types[name];
    return this.parent ? this.parent.getDeclaredType(name) : null;
  }
}

class OneThrown {
  constructor(value) { this.value = value; }
}

class OneFunction {
  constructor(fnIndex, env, name = "<anon>") {
    this.fnIndex = fnIndex;
    this.env = env;
    this.name = name;
  }
}

function assertTypeRuntime(fileName, line, col, expected, value) {
  const actual = typeOfValue(value);
  if (expected === actual) return;
  // allow object to include null? No, strict.
  throwErr(fileName, line, col, `Type error: expected ${expected} but got ${actual}`);
}

function truthyBoolOnly(fileName, line, col, v) {
  if (typeof v !== "boolean") throwErr(fileName, line, col, `Type error: condition must be bool (got ${typeOfValue(v)})`);
  return v;
}

class VM {
  constructor(moduleRecord) {
    this.mod = moduleRecord; // { fileName, funcs, debug, consts, exportNames, mainIndex }
  }

  runFunction(fnIndex, args, envOverride = null) {
    const fn = this.mod.funcs[fnIndex];
    const code = fn.code;
    const debug = fn.debug; // { ip: {line,col} }
    const fileName = this.mod.fileName;

    const baseEnv = envOverride || new Env(fn.closureEnv || null);
    const env = new Env(baseEnv);

    // declare params
    for (let i = 0; i < fn.params.length; i++) {
      const p = fn.params[i];
      const v = i < args.length ? args[i] : null;
      if (p.type) assertTypeRuntime(fileName, p.line, p.col, p.type, v);
      env.define(p.name, v, p.type || null);
    }

    const stack = [];
    let ip = 0;

    // exception handlers: { catchIp, stackDepth, envSnap }
    const handlers = [];

    function dbgLoc() {
      const d = debug[ip] || debug[Math.max(0, ip - 1)] || { line: fn.line || 1, col: fn.col || 1 };
      return d;
    }

    while (ip < code.length) {
      const op = code[ip++];

      try {
        switch (op) {
          case Op.NOP: break;

          case Op.CONST: {
            const idx = code[ip++];
            stack.push(this.mod.consts[idx]);
            break;
          }

          case Op.POP:
            stack.pop();
            break;

          case Op.DEF: {
            const nameIdx = code[ip++];
            const name = this.mod.consts[nameIdx];
            const value = stack.pop();
            const declared = fn.localTypes[name] || null;
            if (declared) assertTypeRuntime(fileName, dbgLoc().line, dbgLoc().col, declared, value);
            env.define(name, value, declared);
            break;
          }

          case Op.LOAD: {
            const nameIdx = code[ip++];
            const name = this.mod.consts[nameIdx];
            const v = env.get(name);
            if (v === undefined) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Undefined identifier: ${name}`);
            }
            stack.push(v);
            break;
          }

          case Op.STORE: {
            const nameIdx = code[ip++];
            const name = this.mod.consts[nameIdx];
            const value = stack.pop();
            const declared = env.getDeclaredType(name);
            if (declared) assertTypeRuntime(fileName, dbgLoc().line, dbgLoc().col, declared, value);
            const ok = env.set(name, value);
            if (!ok) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Assignment to undefined variable: ${name}`);
            }
            break;
          }

          case Op.GETPROP: {
            const propIdx = code[ip++];
            const prop = this.mod.consts[propIdx];
            const obj = stack.pop();

            if (Array.isArray(obj)) {
              if (prop === "len") { stack.push(obj.length); break; }
              if (prop === "get") { stack.push(makeListGetBuiltin(obj, fileName, dbgLoc())); break; }
              if (prop === "set") { stack.push(makeListSetBuiltin(obj, fileName, dbgLoc())); break; }
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Unknown list property: ${prop}`);
            }

            if (obj === null) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, "Cannot access property of null");
            }
            if (typeof obj !== "object") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Cannot access member '${prop}' on ${typeOfValue(obj)}`);
            }
            if (!(prop in obj)) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Property not found: ${prop}`);
            }
            stack.push(obj[prop]);
            break;
          }

          case Op.SETPROP: {
            const propIdx = code[ip++];
            const prop = this.mod.consts[propIdx];
            const value = stack.pop();
            const obj = stack.pop();

            if (obj === null) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, "Cannot set property on null");
            }
            if (typeof obj !== "object" || Array.isArray(obj)) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Cannot set member '${prop}' on ${typeOfValue(obj)}`);
            }
            obj[prop] = value;
            stack.push(value);
            break;
          }

          case Op.MAKELIST: {
            const n = code[ip++];
            const items = [];
            for (let i = 0; i < n; i++) items.push(stack.pop());
            items.reverse();
            stack.push(items);
            break;
          }

          case Op.LIST_GET: {
            const idx = stack.pop();
            const list = stack.pop();
            if (!Array.isArray(list)) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: .get() expects list (got ${typeOfValue(list)})`);
            }
            if (typeof idx !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: list index must be number (got ${typeOfValue(idx)})`);
            }
            const i = idx | 0;
            if (i < 0 || i >= list.length) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Index out of range: ${i}`);
            }
            stack.push(list[i]);
            break;
          }

          case Op.LIST_SET: {
            const value = stack.pop();
            const idx = stack.pop();
            const list = stack.pop();
            if (!Array.isArray(list)) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: .set() expects list (got ${typeOfValue(list)})`);
            }
            if (typeof idx !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: list index must be number (got ${typeOfValue(idx)})`);
            }
            const i = idx | 0;
            if (i < 0 || i >= list.length) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Index out of range: ${i}`);
            }
            list[i] = value;
            stack.push(value);
            break;
          }

          case Op.LEN: {
            const v = stack.pop();
            if (Array.isArray(v)) stack.push(v.length);
            else if (typeof v === "string") stack.push(v.length);
            else {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: .len expects list or string (got ${typeOfValue(v)})`);
            }
            break;
          }

          case Op.MAKEOBJ: {
            const pairCount = code[ip++];
            const obj = Object.create(null);
            for (let i = 0; i < pairCount; i++) {
              const value = stack.pop();
              const key = stack.pop();
              if (typeof key !== "string") {
                const d = dbgLoc();
                throwErr(fileName, d.line, d.col, `Object key must be string (got ${typeOfValue(key)})`);
              }
              obj[key] = value;
            }
            stack.push(obj);
            break;
          }

          case Op.ADD: {
            const b = stack.pop(), a = stack.pop();
            const ta = typeOfValue(a), tb = typeOfValue(b);
            if (ta === "number" && tb === "number") stack.push(a + b);
            else if (ta === "string" && tb === "string") stack.push(a + b);
            else {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: '+' expects number+number or string+string (got ${ta}+${tb})`);
            }
            break;
          }

          case Op.SUB: {
            const b = stack.pop(), a = stack.pop();
            if (typeof a !== "number" || typeof b !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: '-' expects number-number`);
            }
            stack.push(a - b);
            break;
          }

          case Op.MUL: {
            const b = stack.pop(), a = stack.pop();
            if (typeof a !== "number" || typeof b !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: '*' expects number*number`);
            }
            stack.push(a * b);
            break;
          }

          case Op.DIV: {
            const b = stack.pop(), a = stack.pop();
            if (typeof a !== "number" || typeof b !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: '/' expects number/number`);
            }
            stack.push(a / b);
            break;
          }

          case Op.NEG: {
            const a = stack.pop();
            if (typeof a !== "number") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: unary '-' expects number`);
            }
            stack.push(-a);
            break;
          }

          case Op.NOT: {
            const a = stack.pop();
            if (typeof a !== "boolean") {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: '!' expects bool`);
            }
            stack.push(!a);
            break;
          }

          case Op.EQ: {
            const b = stack.pop(), a = stack.pop();
            if (typeOfValue(a) !== typeOfValue(b)) stack.push(false);
            else stack.push(a === b);
            break;
          }

          case Op.NEQ: {
            const b = stack.pop(), a = stack.pop();
            if (typeOfValue(a) !== typeOfValue(b)) stack.push(true);
            else stack.push(a !== b);
            break;
          }

          case Op.LT:
          case Op.LTE:
          case Op.GT:
          case Op.GTE: {
            const b = stack.pop(), a = stack.pop();
            const ta = typeOfValue(a), tb = typeOfValue(b);
            if (!((ta === "number" && tb === "number") || (ta === "string" && tb === "string"))) {
              const d = dbgLoc();
              throwErr(fileName, d.line, d.col, `Type error: comparison expects number/number or string/string (got ${ta}/${tb})`);
            }
            if (op === Op.LT) stack.push(a < b);
            else if (op === Op.LTE) stack.push(a <= b);
            else if (op === Op.GT) stack.push(a > b);
            else stack.push(a >= b);
            break;
          }

          case Op.JMP: {
            const target = code[ip++];
            ip = target;
            break;
          }

          case Op.JMPF: {
            const target = code[ip++];
            const cond = stack.pop();
            const d = dbgLoc();
            if (!truthyBoolOnly(fileName, d.line, d.col, cond)) {
              ip = target;
            }
            break;
          }

          case Op.CLOSURE: {
            const fidx = code[ip++];
            const closure = new OneFunction(fidx, env, this.mod.funcs[fidx].name);
            stack.push(closure);
            break;
          }

          case Op.CALL: {
            const argc = code[ip++];
            const args2 = [];
            for (let i = 0; i < argc; i++) args2.push(stack.pop());
            args2.reverse();
            const callee = stack.pop();

            if (typeof callee === "function") {
              const res = callee(...args2);
              stack.push(res);
              break;
            }

            if (callee instanceof OneFunction) {
              // IMPORTANT: allow OneThrown to bubble up to THIS frame
              const res = this.runFunction(callee.fnIndex, args2, callee.env);
              stack.push(res);
              break;
            }

            const d = dbgLoc();
            throwErr(fileName, d.line, d.col, `Attempted to call non-function (${typeOfValue(callee)})`);
          }

          case Op.RET: {
            const ret = stack.pop();
            return ret;
          }

          case Op.TRY: {
            const catchIp = code[ip++];
            handlers.push({ catchIp, stackDepth: stack.length, envSnap: env });
            break;
          }

          case Op.ENDTRY:
            handlers.pop();
            break;

          case Op.THROW: {
            const value = stack.pop();
            throw new OneThrown(value);
          }

          default: {
            const d = dbgLoc();
            throwErr(fileName, d.line, d.col, `VM: Unknown opcode ${op} (${opName(op)})`);
          }
        }
      } catch (e) {
        if (e instanceof OneThrown) {
          // KEY CHANGE:
          // If this frame has no handlers, DO NOT convert to fatal error.
          // Let it bubble upward so an outer try/catch can catch it.
          if (handlers.length === 0) {
            throw e;
          }

          const h = handlers.pop();
          while (stack.length > h.stackDepth) stack.pop();
          stack.push(e.value);
          ip = h.catchIp;
          continue;
        }

        // rethrow OneError or unknown
        throw e;
      }
    }

    return null; // implicit return null
  }    
}

function makeListGetBuiltin(listRef, fileName, loc) {
  return (i) => {
    if (typeof i !== "number") throwErr(fileName, loc.line, loc.col, `Type error: .get(i) expects number index`);
    const idx = i | 0;
    if (idx < 0 || idx >= listRef.length) throwErr(fileName, loc.line, loc.col, `Index out of range: ${idx}`);
    return listRef[idx];
  };
}

function makeListSetBuiltin(listRef, fileName, loc) {
  return (i, v) => {
    if (typeof i !== "number") throwErr(fileName, loc.line, loc.col, `Type error: .set(i,v) expects number index`);
    const idx = i | 0;
    if (idx < 0 || idx >= listRef.length) throwErr(fileName, loc.line, loc.col, `Index out of range: ${idx}`);
    listRef[idx] = v;
    return v;
  };
}

function stringifyValue(v) {
  if (v === null) return "null";
  if (Array.isArray(v)) return "(" + v.map(stringifyValue).join(", ") + ")";
  if (typeof v === "object") return JSON.stringify(v);
  return String(v);
}

// ============================================================
// 7) Bytecode Compiler (AST -> module record)
// ============================================================

function mkDebugTable() {
  return Object.create(null); // ip -> {line,col}
}

function addDebug(debug, ip, line, col) {
  debug[ip] = { line, col };
}

function compileModuleFromAST(fileName, program, options) {
  // options: { moduleAbsPath, loader, isModule }
  const consts = [];
  const constIndex = new Map(); // JSON-ish key -> idx

  function cconst(v) {
    // keys for objects can't be used; store as JSON for primitives + strings
    const key = typeof v === "string" ? "s:" + v : typeof v === "number" ? "n:" + String(v)
      : typeof v === "boolean" ? "b:" + String(v) : v === null ? "null" : "o:" + JSON.stringify(v);
    if (constIndex.has(key)) return constIndex.get(key);
    const idx = consts.length;
    consts.push(v);
    constIndex.set(key, idx);
    return idx;
  }

  const funcs = [];

  function emitFn(fnName, params, bodyBlock, closureEnv, declaredReturnType, localTypes, fnLine, fnCol) {
    const code = [];
    const debug = mkDebugTable();

    const loopStack = []; // { breakJumps: [], contJumps: [], contTargetIp: number, breakTargetIp: number }

    function emit(op, ...args) {
      const ip = code.length;
      code.push(op, ...args);
      return ip;
    }

    function mark(nodeLike) {
      if (nodeLike && typeof nodeLike.line === "number") addDebug(debug, code.length, nodeLike.line, nodeLike.col || 1);
    }

    function compileExpr(e) {
      switch (e.kind) {
        case "Literal":
          emit(Op.CONST, cconst(e.value));
          return;

        case "Var":
          emit(Op.LOAD, cconst(e.name));
          return;

        case "Unary":
          compileExpr(e.expr);
          if (e.op === TokenKind.MINUS) emit(Op.NEG);
          else if (e.op === TokenKind.BANG) emit(Op.NOT);
          else throwErr(fileName, e.line, e.col, "Unknown unary operator");
          return;

        case "Binary":
          compileExpr(e.left);
          compileExpr(e.right);
          switch (e.op) {
            case TokenKind.PLUS: emit(Op.ADD); break;
            case TokenKind.MINUS: emit(Op.SUB); break;
            case TokenKind.STAR: emit(Op.MUL); break;
            case TokenKind.SLASH: emit(Op.DIV); break;
            case TokenKind.EQEQ: emit(Op.EQ); break;
            case TokenKind.BANGEQ: emit(Op.NEQ); break;
            case TokenKind.LT: emit(Op.LT); break;
            case TokenKind.LTE: emit(Op.LTE); break;
            case TokenKind.GT: emit(Op.GT); break;
            case TokenKind.GTE: emit(Op.GTE); break;
            default:
              throwErr(fileName, e.line, e.col, "Unknown binary operator");
          }
          return;

        case "List": {
          // push items then MAKELIST count
          for (const it of e.items) compileExpr(it);
          emit(Op.MAKELIST, e.items.length);
          return;
        }

        case "Object": {
          // compile as stack pushes: key(const string), value, ... then MAKEOBJ count
          // We'll push key THEN value for each pair.
          for (const p of e.props) {
            emit(Op.CONST, cconst(String(p.key)));
            compileExpr(p.value);
          }
          emit(Op.MAKEOBJ, e.props.length);
          return;
        }

        case "Member":
          compileExpr(e.object);
          emit(Op.GETPROP, cconst(e.prop));
          return;

        case "Call":
          compileExpr(e.callee);
          for (const a of e.args) compileExpr(a);
          emit(Op.CALL, e.args.length);
          return;

        default:
          throwErr(fileName, e.line, e.col, `Compiler: unknown expr kind: ${e.kind}`);
      }
    }

    function compileAssignableTarget(t, isStoreValueOnStackAfter = false) {
      // for assignment:
      // Var: STORE name
      // Member: compile object, compile value, SETPROP prop (leaves value on stack)
      if (t.kind === "Var") {
        emit(Op.STORE, cconst(t.name));
        if (isStoreValueOnStackAfter) emit(Op.LOAD, cconst(t.name));
        return;
      }
      if (t.kind === "Member") {
        // SETPROP expects object then value on stack; our caller typically has value computed, so we need to re-order:
        // We'll handle in assignment compile: compile object, compile value, SETPROP prop
        throw new Error("compileAssignableTarget: Member should be compiled by assignment handler");
      }
      throwErr(fileName, t.line, t.col, "Invalid assignment target");
    }

    function compileStmt(st) {
      mark(st);

      switch (st.kind) {
        case "ImportDecl": {
          // statement import: call builtin import("path") and pop
          emit(Op.LOAD, cconst("import"));
          emit(Op.CONST, cconst(st.path));
          emit(Op.CALL, 1);
          emit(Op.POP);
          return;
        }

        case "LetStmt": {
          compileExpr(st.value);
          // DEF defines var; also attaches declared type in fn.localTypes
          if (st.typeRef && st.typeRef.name) localTypes[st.name] = st.typeRef.name;
          emit(Op.DEF, cconst(st.name));
          return;
        }

        case "AssignStmt": {
          // if target is member, compile object, value, setprop
          if (st.target.kind === "Member") {
            // compile object
            compileExpr(st.target.object);
            // compile value
            compileExpr(st.value);
            // set prop
            emit(Op.SETPROP, cconst(st.target.prop));
            // assignment statement discards result
            emit(Op.POP);
            return;
          }
          // else Var
          compileExpr(st.value);
          compileAssignableTarget(st.target);
          return;
        }

        case "ExprStmt":
          compileExpr(st.expr);
          emit(Op.POP);
          return;

        case "Block":
          for (const s of st.statements) {
            compileStmt(s);
            // DCE already done, but keep safe
            if (s.kind === "ReturnStmt" || s.kind === "ThrowStmt" || s.kind === "BreakStmt" || s.kind === "ContinueStmt") break;
          }
          return;

        case "IfStmt": {
          compileExpr(st.cond);
          const jmpFalseIp = emit(Op.JMPF, 0);
          compileStmt(st.thenBlock);
          const jmpEndIp = emit(Op.JMP, 0);
          // patch false
          code[jmpFalseIp + 1] = code.length;
          if (st.elseBlock) compileStmt(st.elseBlock);
          // patch end
          code[jmpEndIp + 1] = code.length;
          return;
        }

        case "WhileStmt": {
          const start = code.length;
          compileExpr(st.cond);
          const jmpFalseIp = emit(Op.JMPF, 0);

          const loopRec = { breakJumps: [], contJumps: [], contTargetIp: start, breakTargetIp: -1 };
          loopStack.push(loopRec);

          compileStmt(st.body);

          // continue target = re-check cond
          emit(Op.JMP, start);

          // patch jmpFalse to end
          const end = code.length;
          code[jmpFalseIp + 1] = end;

          loopRec.breakTargetIp = end;

          // patch breaks/continues
          for (const bi of loopRec.breakJumps) code[bi + 1] = end;
          for (const ci of loopRec.contJumps) code[ci + 1] = start;

          loopStack.pop();
          return;
        }

        case "ForStmt": {
          // for creates its own "block" in semantics; we don't model env frames in bytecode,
          // but runtime Env is nested by function only; so scoping is analyzer-level.
          // We'll still compile in order: init; loop: cond; body; update; jmp loop.
          if (st.init) compileStmt(st.init);

          const loopStart = code.length;

          if (st.cond) compileExpr(st.cond);
          else { emit(Op.CONST, cconst(true)); } // for(;;) => true
          const jmpFalseIp = emit(Op.JMPF, 0);

          const loopRec = { breakJumps: [], contJumps: [], contTargetIp: -1, breakTargetIp: -1 };
          loopStack.push(loopRec);

          compileStmt(st.body);

          // continue should jump to update
          const updateStart = code.length;
          loopRec.contTargetIp = updateStart;

          if (st.update) compileStmt(st.update);

          emit(Op.JMP, loopStart);

          const end = code.length;
          code[jmpFalseIp + 1] = end;

          loopRec.breakTargetIp = end;
          for (const bi of loopRec.breakJumps) code[bi + 1] = end;
          for (const ci of loopRec.contJumps) code[ci + 1] = updateStart;

          loopStack.pop();
          return;
        }

        case "BreakStmt": {
          if (loopStack.length === 0) throwErr(fileName, st.line, st.col, "Compiler: break outside loop");
          const rec = loopStack[loopStack.length - 1];
          const j = emit(Op.JMP, 0);
          rec.breakJumps.push(j);
          return;
        }

        case "ContinueStmt": {
          if (loopStack.length === 0) throwErr(fileName, st.line, st.col, "Compiler: continue outside loop");
          const rec = loopStack[loopStack.length - 1];
          const j = emit(Op.JMP, 0);
          rec.contJumps.push(j);
          return;
        }

        case "ReturnStmt": {
          if (st.expr) compileExpr(st.expr);
          else emit(Op.CONST, cconst(null));
          emit(Op.RET);
          return;
        }

        case "ThrowStmt": {
          compileExpr(st.expr);
          emit(Op.THROW);
          return;
        }

        case "TryCatchStmt": {
          // TRY catchIp
          const tryIp = emit(Op.TRY, 0);
          compileStmt(st.tryBlock);
          emit(Op.ENDTRY);
          const jmpAfterCatch = emit(Op.JMP, 0);

          // catch starts here
          const catchIp = code.length;
          code[tryIp + 1] = catchIp;

          // stack top has thrown value (pushed by VM)
          // define catch variable
          // This is a "define" in catch-scope; we model it as DEF in current env.
          // (Analyzer guarantees unique name in catch scope; runtime env doesn't create new frame)
          // We'll DEF it:
          // value already on stack
          emit(Op.DEF, cconst(st.catchName));

          compileStmt(st.catchBlock);

          // after catch ends, expression returns null by default for statement; no action.

          // patch jump
          code[jmpAfterCatch + 1] = code.length;
          return;
        }

        default:
          throwErr(fileName, st.line, st.col, `Compiler: unknown statement kind: ${st.kind}`);
      }
    }

    // compile function body statements
    compileStmt(bodyBlock);

    // implicit return null
    emit(Op.CONST, cconst(null));
    emit(Op.RET);

    return {
      name: fnName,
      line: fnLine || 1,
      col: fnCol || 1,
      params,
      returnType: declaredReturnType || null,
      localTypes,
      code,
      debug,
      closureEnv: closureEnv || null,
    };
  }

  // Build top-level "main" function body: hoisted function declarations produce closures and defs.
  // We treat functions as values bound in env under their name.
  // Implementation: compile top-level into a main function that:
  //   - defines builtin functions in env (print/import/load) via host injection before running.
  //   - binds all function decls by defining closures.
  //   - runs top-level non-fn statements.
  //
  // Similarly for module: export collection is done after run by reading env for exported names.
  const exportNames = [];
  const topFnDecls = [];
  const topStmts = [];

  for (const it of program.items) {
    if (it.kind === "FuncDecl") topFnDecls.push(it);
    else topStmts.push(it);

    if (it.kind === "LetStmt" && it.exported) exportNames.push(it.name);
    if (it.kind === "FuncDecl" && it.exported) exportNames.push(it.name);
  }

  // First, compile function decls into separate bytecode funcs.
  // We'll compile each function body into its own function record; closures created at runtime capture env.
  const fnIndexByName = new Map();

  // Reserve slot 0 for main
  funcs.push(null);

  function compileDeclaredFunction(fnDecl) {
    const params = fnDecl.params.map(p => ({ name: p.name, type: p.typeRef ? p.typeRef.name : null, line: p.line, col: p.col }));
    const localTypes = Object.create(null);

    // in function, params are typed separately; lets will populate localTypes during compilation
    const fnRec = emitFn(fnDecl.name, params, fnDecl.body, null, fnDecl.returnType ? fnDecl.returnType.name : null, localTypes, fnDecl.line, fnDecl.col);
    const idx = funcs.length;
    funcs.push(fnRec);
    fnIndexByName.set(fnDecl.name, idx);
    return idx;
  }

  for (const fd of topFnDecls) compileDeclaredFunction(fd);

  // Now compile main. It needs to:
  // - define closures for all top functions as variables (hoisted)
  // - execute imports/lets/assign/expr etc at top-level
  const mainBody = node("Block", {
    line: 1, col: 1,
    statements: [
      // pseudo: let __dummy = null (not needed)
    ]
  });

  // We'll generate main by reusing emitFn but we need special statement compilation
  // for function decls: define closure and DEF.
  const mainLocalTypes = Object.create(null);

  const mainFn = (function compileMain() {
    const params = [];
    const code = [];
    const debug = mkDebugTable();

    function emit(op, ...args) {
      const ip = code.length;
      code.push(op, ...args);
      return ip;
    }
    function mark(line, col) { addDebug(debug, code.length, line, col); }

    // Bind top functions
    for (const fd of topFnDecls) {
      mark(fd.line, fd.col);
      const idx = fnIndexByName.get(fd.name);
      emit(Op.CLOSURE, idx);
      emit(Op.DEF, cconst(fd.name));
    }

    // Compile top-level statements
    // We'll reuse a mini compiler for statements/expr but without nested function declarations at top-level.
    const loopStack = [];

    function compileExpr(e) {
      switch (e.kind) {
        case "Literal": emit(Op.CONST, cconst(e.value)); return;
        case "Var": emit(Op.LOAD, cconst(e.name)); return;
        case "Unary":
          compileExpr(e.expr);
          if (e.op === TokenKind.MINUS) emit(Op.NEG);
          else if (e.op === TokenKind.BANG) emit(Op.NOT);
          else throwErr(fileName, e.line, e.col, "Unknown unary operator");
          return;
        case "Binary":
          compileExpr(e.left); compileExpr(e.right);
          switch (e.op) {
            case TokenKind.PLUS: emit(Op.ADD); break;
            case TokenKind.MINUS: emit(Op.SUB); break;
            case TokenKind.STAR: emit(Op.MUL); break;
            case TokenKind.SLASH: emit(Op.DIV); break;
            case TokenKind.EQEQ: emit(Op.EQ); break;
            case TokenKind.BANGEQ: emit(Op.NEQ); break;
            case TokenKind.LT: emit(Op.LT); break;
            case TokenKind.LTE: emit(Op.LTE); break;
            case TokenKind.GT: emit(Op.GT); break;
            case TokenKind.GTE: emit(Op.GTE); break;
            default: throwErr(fileName, e.line, e.col, "Unknown binary operator");
          }
          return;
        case "List":
          for (const it of e.items) compileExpr(it);
          emit(Op.MAKELIST, e.items.length);
          return;
        case "Object":
          for (const p of e.props) { emit(Op.CONST, cconst(String(p.key))); compileExpr(p.value); }
          emit(Op.MAKEOBJ, e.props.length);
          return;
        case "Member":
          compileExpr(e.object);
          emit(Op.GETPROP, cconst(e.prop));
          return;
        case "Call":
          compileExpr(e.callee);
          for (const a of e.args) compileExpr(a);
          emit(Op.CALL, e.args.length);
          return;
        default:
          throwErr(fileName, e.line, e.col, `Compiler(main): unknown expr kind: ${e.kind}`);
      }
    }

    function compileStmt(st) {
      mark(st.line || 1, st.col || 1);
      switch (st.kind) {
        case "ImportDecl":
          emit(Op.LOAD, cconst("import"));
          emit(Op.CONST, cconst(st.path));
          emit(Op.CALL, 1);
          emit(Op.POP);
          return;

        case "LetStmt":
          compileExpr(st.value);
          if (st.typeRef && st.typeRef.name) mainLocalTypes[st.name] = st.typeRef.name;
          emit(Op.DEF, cconst(st.name));
          return;

        case "AssignStmt":
          if (st.target.kind === "Member") {
            compileExpr(st.target.object);
            compileExpr(st.value);
            emit(Op.SETPROP, cconst(st.target.prop));
            emit(Op.POP);
            return;
          } else {
            compileExpr(st.value);
            emit(Op.STORE, cconst(st.target.name));
            return;
          }

        case "ExprStmt":
          compileExpr(st.expr);
          emit(Op.POP);
          return;

        case "Block":
          for (const s of st.statements) {
            compileStmt(s);
            if (s.kind === "ReturnStmt" || s.kind === "ThrowStmt" || s.kind === "BreakStmt" || s.kind === "ContinueStmt") break;
          }
          return;

        case "IfStmt": {
          compileExpr(st.cond);
          const jf = emit(Op.JMPF, 0);
          compileStmt(st.thenBlock);
          const je = emit(Op.JMP, 0);
          code[jf + 1] = code.length;
          if (st.elseBlock) compileStmt(st.elseBlock);
          code[je + 1] = code.length;
          return;
        }

        case "WhileStmt": {
          const start = code.length;
          compileExpr(st.cond);
          const jf = emit(Op.JMPF, 0);

          const rec = { breakJumps: [], contJumps: [], contTargetIp: start, breakTargetIp: -1 };
          loopStack.push(rec);

          compileStmt(st.body);
          emit(Op.JMP, start);

          const end = code.length;
          code[jf + 1] = end;

          rec.breakTargetIp = end;
          for (const bi of rec.breakJumps) code[bi + 1] = end;
          for (const ci of rec.contJumps) code[ci + 1] = start;

          loopStack.pop();
          return;
        }

        case "ForStmt": {
          if (st.init) compileStmt(st.init);
          const loopStart = code.length;

          if (st.cond) compileExpr(st.cond);
          else emit(Op.CONST, cconst(true));

          const jf = emit(Op.JMPF, 0);

          const rec = { breakJumps: [], contJumps: [], contTargetIp: -1, breakTargetIp: -1 };
          loopStack.push(rec);

          compileStmt(st.body);

          const updateStart = code.length;
          rec.contTargetIp = updateStart;

          if (st.update) compileStmt(st.update);

          emit(Op.JMP, loopStart);

          const end = code.length;
          code[jf + 1] = end;

          rec.breakTargetIp = end;
          for (const bi of rec.breakJumps) code[bi + 1] = end;
          for (const ci of rec.contJumps) code[ci + 1] = updateStart;

          loopStack.pop();
          return;
        }

        case "BreakStmt": {
          if (loopStack.length === 0) throwErr(fileName, st.line, st.col, "Compiler(main): break outside loop");
          const rec = loopStack[loopStack.length - 1];
          const j = emit(Op.JMP, 0);
          rec.breakJumps.push(j);
          return;
        }

        case "ContinueStmt": {
          if (loopStack.length === 0) throwErr(fileName, st.line, st.col, "Compiler(main): continue outside loop");
          const rec = loopStack[loopStack.length - 1];
          const j = emit(Op.JMP, 0);
          rec.contJumps.push(j);
          return;
        }

        case "ThrowStmt":
          compileExpr(st.expr);
          emit(Op.THROW);
          return;

        case "TryCatchStmt": {
          const tryIp = emit(Op.TRY, 0);
          compileStmt(st.tryBlock);
          emit(Op.ENDTRY);
          const jAfter = emit(Op.JMP, 0);
          const catchIp = code.length;
          code[tryIp + 1] = catchIp;
          // thrown value is on stack
          emit(Op.DEF, cconst(st.catchName));
          compileStmt(st.catchBlock);
          code[jAfter + 1] = code.length;
          return;
        }

        case "ReturnStmt":
          // Top-level return disallowed by analyzer, but compile anyway:
          if (st.expr) compileExpr(st.expr);
          else emit(Op.CONST, cconst(null));
          emit(Op.RET);
          return;

        default:
          throwErr(fileName, st.line || 1, st.col || 1, `Compiler(main): unknown stmt kind: ${st.kind}`);
      }
    }

    for (const st of topStmts) {
      if (st.kind === "FuncDecl") continue;
      compileStmt(st);
    }

    // implicit return null
    emit(Op.CONST, cconst(null));
    emit(Op.RET);

    return {
      name: "<main>",
      line: 1,
      col: 1,
      params,
      returnType: null,
      localTypes: mainLocalTypes,
      code,
      debug,
      closureEnv: null,
    };
  })();

  funcs[0] = mainFn;

  return {
    version: "1.1",
    fileName,
    consts,
    funcs,
    exportNames,
    mainIndex: 0,
  };
}

// ============================================================
// 8) Module Loader (bytecode caching, import semantics)
// ============================================================

class ModuleLoader {
  constructor() {
    this.cache = new Map();   // absPath -> { module, exportsObj, ran }
    this.loading = new Set(); // absPath currently loading
    this.stack = [];
  }

  resolve(specifier, fromDir) {
    if (typeof specifier !== "string") return null;
    let p = specifier;
    if (!path.extname(p)) p += ".one";
    return path.isAbsolute(p) ? p : path.resolve(fromDir, p);
  }

  loadAndRunModule(absPath) {
    if (this.cache.has(absPath)) return this.cache.get(absPath);

    if (this.loading.has(absPath)) {
      const chain = [...this.stack, absPath].map(x => path.basename(x)).join(" -> ");
      throw new Error(`Circular import detected: ${chain}`);
    }

    this.loading.add(absPath);
    this.stack.push(absPath);

    const srcRaw = readFileText(absPath);
    const mode = findHeaderMode(srcRaw);
    if (mode !== "backend") {
      // v1.1 module import supports backend modules only
      throw new OneError("Only @backend modules can be imported", path.basename(absPath), 1, 1);
    }

    const src = stripHeaderLine(srcRaw);
    const fileName = path.basename(absPath);

    const lex = new Lexer(fileName, src);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(fileName, tokens);
    let program = parser.parseProgram();

    analyzeProgram(fileName, src, program);
    program = optimizeProgram(program);

    const mod = compileModuleFromAST(fileName, program, { moduleAbsPath: absPath, loader: this, isModule: true });

    const record = {
      absPath,
      module: mod,
      exportsObj: Object.create(null),
      ran: false,
    };
    this.cache.set(absPath, record);

    // run the module once
    const vm = new VM(mod);
    const env = makeRootEnvForBackend(fileName, path.dirname(absPath), this);
    // Bind top-level execution: run main which defines top-level vars in its own env.
    // Our VM creates a new Env each function call; to persist module state, we do:
    // run main with envOverride = env so definitions happen in that env.
    vm.runFunction(mod.mainIndex, [], env);

    // snapshot exports
    for (const name of mod.exportNames) {
      const v = env.get(name);
      if (v === undefined) throwErr(fileName, 1, 1, `Export '${name}' not found at runtime`);
      record.exportsObj[name] = v;
    }

    record.ran = true;

    this.stack.pop();
    this.loading.delete(absPath);

    return record;
  }
}

function makeRootEnvForBackend(fileName, baseDir, loader) {
  const env = new Env(null);

  // print builtin
  env.define("print", (...args) => {
    console.log(args.map(stringifyValue).join(" "));
    return null;
  });

  // load builtin (read file as string)
  env.define("load", (p) => {
    if (typeof p !== "string") throwErr(fileName, 1, 1, "load(path) expects a string path");
    const abs = path.isAbsolute(p) ? p : path.resolve(baseDir, p);
    return readFileText(abs);
  });

  // import builtin (expression import)
  env.define("import", (spec) => {
    if (typeof spec !== "string") throwErr(fileName, 1, 1, "import(path) expects a string path");
    const resolved = loader.resolve(spec, baseDir);
    const rec = loader.loadAndRunModule(resolved);
    return rec.exportsObj;
  });

  return env;
}

// ============================================================
// 9) Backend entry: compile + run / build .onebc
// ============================================================

function compileBackendFromSource(absPath, srcRaw, loader) {
  const fileName = path.basename(absPath);
  const src = stripHeaderLine(srcRaw);

  const lex = new Lexer(fileName, src);
  const tokens = lex.tokenizeAll();
  const parser = new Parser(fileName, tokens);
  let program = parser.parseProgram();

  analyzeProgram(fileName, src, program);
  program = optimizeProgram(program);

  const mod = compileModuleFromAST(fileName, program, { moduleAbsPath: absPath, loader, isModule: false });
  return { fileName, src, program, module: mod };
}

function runBackendFile(absPath, srcRaw, loader) {
  const fileName = path.basename(absPath);
  const baseDir = path.dirname(absPath);

  const { module: mod } = compileBackendFromSource(absPath, srcRaw, loader);
  const vm = new VM(mod);

  const env = makeRootEnvForBackend(fileName, baseDir, loader);
  vm.runFunction(mod.mainIndex, [], env);

  return env;
}

function buildBackendFile(absPath, srcRaw, loader) {
  const { module: mod } = compileBackendFromSource(absPath, srcRaw, loader);
  const outPath = absPath.replace(/\.one$/i, "") + ".onebc";
  // Persist as JSON (portable + stable for v1.x in this implementation)
  // NOTE: This is a simple artifact; a future version could store binary.
  writeFileText(outPath, JSON.stringify(mod, null, 2));
  return outPath;
}

// ============================================================
// 10) Frontend (parse + compile <logic> backend into embedded VM JS)
// ============================================================

class FScanner {
  constructor(fileName, src) {
    this.fileName = fileName;
    this.src = String(src);
    this.i = 0;
    this.line = 1;
    this.col = 1;
  }
  peek(n = 0) {
    const idx = this.i + n;
    return idx >= 0 && idx < this.src.length ? this.src[idx] : "\0";
  }
  advance() {
    const ch = this.peek();
    this.i++;
    if (ch === "\n") { this.line++; this.col = 1; }
    else this.col++;
    return ch;
  }
  startsWith(s) { return this.src.slice(this.i, this.i + s.length) === s; }
  skipWs() {
    while (true) {
      const ch = this.peek();
      if (ch === "\0") break;
      if (ch === " " || ch === "\t" || ch === "\r" || ch === "\n") this.advance();
      else break;
    }
  }
}

function parseFrontend(fileName, srcRaw) {
  let markupSrc = String(srcRaw);

  function extractBlock(tag) {
    const open = `<${tag}>`;
    const close = `</${tag}>`;
    const oi = markupSrc.indexOf(open);
    if (oi === -1) return null;
    const ci = markupSrc.indexOf(close, oi + open.length);
    if (ci === -1) {
      const { line, col } = indexToLineCol(markupSrc, oi);
      throwErr(fileName, line, col, `Unterminated <${tag}> block (missing ${close})`);
    }
    const inner = markupSrc.slice(oi + open.length, ci);
    markupSrc = markupSrc.slice(0, oi) + markupSrc.slice(ci + close.length);
    return inner;
  }

  const logicInner = extractBlock("logic");
  const styleInner = extractBlock("style");

  markupSrc = markupSrc.replace(/^\s*@frontend[^\n]*\n/, "");

  const s = new FScanner(fileName, markupSrc);

  function parseStringLiteral() {
    const startLine = s.line, startCol = s.col;
    if (s.peek() !== "\"") throwErr(fileName, startLine, startCol, "Expected string literal");
    s.advance();
    let out = "";
    while (true) {
      const c = s.peek();
      if (c === "\0") throwErr(fileName, startLine, startCol, "Unterminated string");
      if (c === "\"") { s.advance(); break; }
      if (c === "\\") {
        s.advance();
        const esc = s.peek();
        if (esc === "\0") throwErr(fileName, startLine, startCol, "Unterminated escape");
        s.advance();
        if (esc === "n") out += "\n";
        else if (esc === "t") out += "\t";
        else out += esc;
      } else out += s.advance();
    }
    return node("Text", { line: startLine, col: startCol, value: out });
  }

  function parseInterpolation() {
    const startLine = s.line, startCol = s.col;
    if (s.peek() !== "{") return null;
    s.advance(); // {

    // capture until matching }
    let inner = "";
    while (true) {
      const c = s.peek();
      if (c === "\0") throwErr(fileName, startLine, startCol, "Unterminated interpolation (missing '}')");
      if (c === "}") { s.advance(); break; }
      inner += s.advance();
    }

    const innerTrim = inner.trim();
    if (!innerTrim.length) throwErr(fileName, startLine, startCol, "Empty interpolation '{}' is not allowed");

    // Parse full backend expression from innerTrim
    const lex = new Lexer(fileName, innerTrim);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(fileName, tokens);
    const expr = parser.parseExpr();
    // Must be EOF next
    if (!parser.at(TokenKind.EOF)) {
      const t = parser.cur();
      throwErr(fileName, t.line, t.col, "Unexpected tokens in interpolation expression");
    }

    return node("Interp", { line: startLine, col: startCol, expr });
  }

  function parseTextOrInterp() {
    s.skipWs();
    const ch = s.peek();
    if (ch === "\"") return parseStringLiteral();
    if (ch === "{") return parseInterpolation();
    return null;
  }

  function parseAttrs() {
    const attrs = [];
    while (true) {
      s.skipWs();
      const ch = s.peek();
      if (ch === ">" || ch === "\0") break;

      if (!isIdentStart(ch)) throwErr(fileName, s.line, s.col, "Expected attribute name");
      let name = "";
      const startLine = s.line, startCol = s.col;
      while (isIdentPart(s.peek())) name += s.advance();

      s.skipWs();
      if (s.peek() !== "=") throwErr(fileName, s.line, s.col, "Expected '=' after attribute name");
      s.advance();
      s.skipWs();
      if (s.peek() !== "\"") throwErr(fileName, s.line, s.col, 'Expected attribute value like "..."');

      const valNode = parseStringLiteral();
      attrs.push({ name, value: valNode.value, line: startLine, col: startCol });
    }
    return attrs;
  }

  function parseElement() {
    const startLine = s.line, startCol = s.col;
    if (s.peek() !== "<") return null;
    if (s.startsWith("</")) return null;

    s.advance(); // <
    s.skipWs();
    if (!isIdentStart(s.peek())) throwErr(fileName, s.line, s.col, "Expected tag name");
    let tag = "";
    while (isIdentPart(s.peek())) tag += s.advance();

    const attrs = parseAttrs();
    if (s.peek() !== ">") throwErr(fileName, s.line, s.col, "Expected '>'");
    s.advance();

    const children = [];
    while (true) {
      s.skipWs();
      if (s.startsWith(`</${tag}`)) break;
      if (s.peek() === "\0") throwErr(fileName, startLine, startCol, `Unterminated <${tag}> (missing </${tag}>)`);

      const el = parseElement();
      if (el) { children.push(el); continue; }

      const t = parseTextOrInterp();
      if (t) { children.push(t); continue; }

      throwErr(fileName, s.line, s.col, 'Frontend markup text must be quoted like "Hello" or use {expr}');
    }

    // closing tag
    s.advance(); // <
    s.advance(); // /
    for (const c of tag) {
      if (s.peek() !== c) throwErr(fileName, s.line, s.col, "Mismatched closing tag");
      s.advance();
    }
    s.skipWs();
    if (s.peek() !== ">") throwErr(fileName, s.line, s.col, "Expected '>' at end of closing tag");
    s.advance();

    return node("Element", { line: startLine, col: startCol, tag, attrs, children });
  }

  function parseRootNodes() {
    const nodes = [];
    while (true) {
      s.skipWs();
      if (s.peek() === "\0") break;

      const el = parseElement();
      if (el) { nodes.push(el); continue; }

      const t = parseTextOrInterp();
      if (t) { nodes.push(t); continue; }

      throwErr(fileName, s.line, s.col, "Unexpected content in frontend file");
    }
    return nodes;
  }

  const nodes = parseRootNodes();
  return {
    logicRaw: logicInner || "",
    styleRaw: styleInner || "",
    nodes,
  };
}

function escapeJsString(s) {
  return String(s)
    .replace(/\\/g, "\\\\")
    .replace(/`/g, "\\`")
    .replace(/\$/g, "\\$")
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n");
}

function emitFrontendArtifacts(absPath, srcRaw, loader) {
  const fileName = path.basename(absPath);
  const fe = parseFrontend(fileName, srcRaw);

  // Compile logic (full backend code)
  const logicName = fileName + "::logic";
  const logicSrc = fe.logicRaw || "";
  const lex = new Lexer(logicName, logicSrc);
  const tokens = lex.tokenizeAll();
  const parser = new Parser(logicName, tokens);
  let program = parser.parseProgram();
  analyzeProgram(logicName, logicSrc, program);
  program = optimizeProgram(program);

  // Compile to a "module" bytecode that will be embedded in JS
  const mod = compileModuleFromAST(logicName, program, { moduleAbsPath: absPath, loader, isModule: false });

  // Emit HTML + JS
  const base = absPath.replace(/\.one$/i, "");
  const outHtml = base + ".html";
  const outJs = base + ".js";

  const html = [
    "<!doctype html>",
    "<html>",
    "<head>",
    "  <meta charset=\"utf-8\">",
    `  <title>${escapeHtml(path.basename(base))}</title>`,
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    fe.styleRaw && fe.styleRaw.trim().length ? "  <style>\n" + fe.styleRaw + "\n  </style>" : "",
    "</head>",
    "<body>",
    "  <script src=\"" + path.basename(outJs) + "\"></script>",
    "</body>",
    "</html>",
  ].filter(Boolean).join("\n");

  const js = emitFrontendJSBundle(fileName, fe.nodes, mod);

  writeFileText(outHtml, html + "\n");
  writeFileText(outJs, js + "\n");

  return { outHtml, outJs };
}

function escapeHtml(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function emitFrontendJSBundle(sourceFileName, nodes, mod) {
  // Bundle includes: minimal VM (browser-safe), bytecode module JSON, DOM builder, event binding.
  // No Node FS, no backend import. Frontend import is not supported in-browser in v1.1.
  // (Future: bundler resolves and embeds modules.)
  const modJson = JSON.stringify(mod);

  // Emit code that:
  // 1) Installs builtins: print -> console.log
  // 2) Runs mod.main, producing env with functions/vars
  // 3) Builds DOM from nodes, where interpolation evals expressions by re-running a tiny evaluator:
  //    Instead of re-parsing expressions in browser, we compile interpolations as inline evaluators:
  //    Here, we re-emit a small AST evaluator in JS for interpolation nodes only.
  //
  // For simplicity + correctness: we evaluate interpolation expressions by executing a small “expression VM”
  // over the same bytecode compiler? That’s heavy. Instead, we will:
  // - Reuse the same bytecode module env, but interpolation expressions are AST nodes (from frontend parse).
  // - Emit those AST nodes as JSON and interpret them directly (pure evaluator with strict semantics).
  //
  // This keeps frontend visible and correct without needing to recompile expressions to bytecode in browser.

  const nodesJson = JSON.stringify(nodes);

  return [
    `// Generated by ONE v1.1 frontend compiler`,
    `// Source: ${escapeJsString(sourceFileName)}`,
    `(function(){`,
    `"use strict";`,
    ``,
    `// ----------------- Embedded bytecode module -----------------`,
    `const __MOD = ${modJson};`,
    ``,
    `// ----------------- Minimal runtime helpers -----------------`,
    `function __typeOf(v){`,
    `  if (v === null) return "null";`,
    `  if (Array.isArray(v)) return "list";`,
    `  const t = typeof v;`,
    `  if (t === "number") return "number";`,
    `  if (t === "string") return "string";`,
    `  if (t === "boolean") return "bool";`,
    `  if (t === "function") return "function";`,
    `  if (t === "object") return "object";`,
    `  return "unknown";`,
    `}`,
    `function __stringify(v){`,
    `  if (v === null) return "null";`,
    `  if (Array.isArray(v)) return "(" + v.map(__stringify).join(", ") + ")";`,
    `  if (typeof v === "object") { try { return JSON.stringify(v); } catch { return "[object]"; } }`,
    `  return String(v);`,
    `}`,
    `function __assertBool(v, where){`,
    `  if (typeof v !== "boolean") throw new Error(where + ": condition must be bool (got " + __typeOf(v) + ")");`,
    `  return v;`,
    `}`,
    `function __assertType(expected, v, where){`,
    `  const a = __typeOf(v);`,
    `  if (a !== expected) throw new Error(where + ": expected " + expected + " but got " + a);`,
    `}`,
    ``,
    `// ----------------- Env / Cells (closures) -----------------`,
    `class __Cell{ constructor(v){ this.v = v; } }`,
    `class __Env{`,
    `  constructor(parent){ this.parent = parent || null; this.map = Object.create(null); this.types = Object.create(null); }`,
    `  getCell(n){ if (Object.prototype.hasOwnProperty.call(this.map,n)) return this.map[n]; return this.parent?this.parent.getCell(n):null; }`,
    `  get(n){ const c = this.getCell(n); return c?c.v:undefined; }`,
    `  set(n,v){ const c = this.getCell(n); if(!c) return false; c.v=v; return true; }`,
    `  define(n,v,t){ this.map[n]=new __Cell(v); this.types[n]=t||null; }`,
    `  getType(n){ if(Object.prototype.hasOwnProperty.call(this.types,n)) return this.types[n]; return this.parent?this.parent.getType(n):null; }`,
    `}`,
    `class __Thrown{ constructor(v){ this.value=v; } }`,
    `class __Fn{ constructor(fnIndex, env, name){ this.fnIndex=fnIndex; this.env=env; this.name=name||"<fn>"; } }`,
    ``,
    `// ----------------- Bytecode VM (browser) -----------------`,
    `const __Op = ${JSON.stringify(Op)};`,
    `function __opName(o){ for(const k in __Op){ if(__Op[k]===o) return k; } return "OP_"+o; }`,
    ``,
    `function __makeListGet(listRef){`,
    `  return function(i){`,
    `    if(typeof i!=="number") throw new Error(".get(i): index must be number");`,
    `    const idx = i|0;`,
    `    if(idx<0||idx>=listRef.length) throw new Error("Index out of range: "+idx);`,
    `    return listRef[idx];`,
    `  };`,
    `}`,
    `function __makeListSet(listRef){`,
    `  return function(i,v){`,
    `    if(typeof i!=="number") throw new Error(".set(i,v): index must be number");`,
    `    const idx = i|0;`,
    `    if(idx<0||idx>=listRef.length) throw new Error("Index out of range: "+idx);`,
    `    listRef[idx]=v;`,
    `    return v;`,
    `  };`,
    `}`,
    ``,
    `function __runFn(mod, fnIndex, args, envOverride){`,
    `  const fn = mod.funcs[fnIndex];`,
    `  const code = fn.code;`,
    `  const debug = fn.debug || {};`,
    `  const fileName = mod.fileName || "<logic>";`,
    `  const baseEnv = envOverride || new __Env(fn.closureEnv||null);`,
    `  const env = new __Env(baseEnv);`,
    `  for(let i=0;i<fn.params.length;i++){`,
    `    const p = fn.params[i];`,
    `    const v = (i<args.length)?args[i]:null;`,
    `    if(p.type) __assertType(p.type, v, fileName+":"+p.line+":"+p.col);`,
    `    env.define(p.name, v, p.type||null);`,
    `  }`,
    `  const stack=[];`,
    `  let ip=0;`,
    `  const handlers=[];`,
    `  function loc(){`,
    `    const d = debug[ip] || debug[Math.max(0,ip-1)] || {line:fn.line||1,col:fn.col||1};`,
    `    return d;`,
    `  }`,
    `  while(ip < code.length){`,
    `    const op = code[ip++];`,
    `    try {`,
    `      switch(op){`,
    `        case __Op.NOP: break;`,
    `        case __Op.CONST: stack.push(mod.consts[code[ip++]]); break;`,
    `        case __Op.POP: stack.pop(); break;`,
    `        case __Op.DEF: {`,
    `          const name = mod.consts[code[ip++]];`,
    `          const v = stack.pop();`,
    `          const t = fn.localTypes?fn.localTypes[name]:null;`,
    `          if(t) __assertType(t, v, fileName+":"+loc().line+":"+loc().col);`,
    `          env.define(name, v, t||null);`,
    `          break;`,
    `        }`,
    `        case __Op.LOAD: {`,
    `          const name = mod.consts[code[ip++]];`,
    `          const v = env.get(name);`,
    `          if(v===undefined) throw new Error(fileName+":"+loc().line+":"+loc().col+": Undefined identifier: "+name);`,
    `          stack.push(v);`,
    `          break;`,
    `        }`,
    `        case __Op.STORE: {`,
    `          const name = mod.consts[code[ip++]];`,
    `          const v = stack.pop();`,
    `          const t = env.getType(name);`,
    `          if(t) __assertType(t, v, fileName+":"+loc().line+":"+loc().col);`,
    `          if(!env.set(name,v)) throw new Error(fileName+":"+loc().line+":"+loc().col+": Assignment to undefined variable: "+name);`,
    `          break;`,
    `        }`,
    `        case __Op.GETPROP: {`,
    `          const prop = mod.consts[code[ip++]];`,
    `          const obj = stack.pop();`,
    `          if(Array.isArray(obj)){`,
    `            if(prop==="len"){ stack.push(obj.length); break; }`,
    `            if(prop==="get"){ stack.push(__makeListGet(obj)); break; }`,
    `            if(prop==="set"){ stack.push(__makeListSet(obj)); break; }`,
    `            throw new Error("Unknown list property: "+prop);`,
    `          }`,
    `          if(obj===null) throw new Error("Cannot access property of null");`,
    `          if(typeof obj!=="object") throw new Error("Cannot access member '"+prop+"' on "+__typeOf(obj));`,
    `          if(!(prop in obj)) throw new Error("Property not found: "+prop);`,
    `          stack.push(obj[prop]);`,
    `          break;`,
    `        }`,
    `        case __Op.SETPROP: {`,
    `          const prop = mod.consts[code[ip++]];`,
    `          const value = stack.pop();`,
    `          const obj = stack.pop();`,
    `          if(obj===null) throw new Error("Cannot set property on null");`,
    `          if(typeof obj!=="object" || Array.isArray(obj)) throw new Error("Cannot set member '"+prop+"' on "+__typeOf(obj));`,
    `          obj[prop]=value;`,
    `          stack.push(value);`,
    `          break;`,
    `        }`,
    `        case __Op.MAKELIST: {`,
    `          const n = code[ip++];`,
    `          const items=[];`,
    `          for(let i=0;i<n;i++) items.push(stack.pop());`,
    `          items.reverse();`,
    `          stack.push(items);`,
    `          break;`,
    `        }`,
    `        case __Op.MAKEOBJ: {`,
    `          const n = code[ip++];`,
    `          const o = Object.create(null);`,
    `          for(let i=0;i<n;i++){`,
    `            const v = stack.pop();`,
    `            const k = stack.pop();`,
    `            if(typeof k!=="string") throw new Error("Object key must be string");`,
    `            o[k]=v;`,
    `          }`,
    `          stack.push(o);`,
    `          break;`,
    `        }`,
    `        case __Op.ADD: {`,
    `          const b=stack.pop(), a=stack.pop();`,
    `          const ta=__typeOf(a), tb=__typeOf(b);`,
    `          if(ta==="number"&&tb==="number") stack.push(a+b);`,
    `          else if(ta==="string"&&tb==="string") stack.push(a+b);`,
    `          else throw new Error("Type error: '+' expects number+number or string+string");`,
    `          break;`,
    `        }`,
    `        case __Op.SUB: { const b=stack.pop(), a=stack.pop(); if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '-' expects number"); stack.push(a-b); break; }`,
    `        case __Op.MUL: { const b=stack.pop(), a=stack.pop(); if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '*' expects number"); stack.push(a*b); break; }`,
    `        case __Op.DIV: { const b=stack.pop(), a=stack.pop(); if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '/' expects number"); stack.push(a/b); break; }`,
    `        case __Op.NEG: { const a=stack.pop(); if(typeof a!=="number") throw new Error("Type error: unary '-' expects number"); stack.push(-a); break; }`,
    `        case __Op.NOT: { const a=stack.pop(); if(typeof a!=="boolean") throw new Error("Type error: '!' expects bool"); stack.push(!a); break; }`,
    `        case __Op.EQ: { const b=stack.pop(), a=stack.pop(); stack.push(__typeOf(a)!==__typeOf(b)?false:(a===b)); break; }`,
    `        case __Op.NEQ:{ const b=stack.pop(), a=stack.pop(); stack.push(__typeOf(a)!==__typeOf(b)?true:(a!==b)); break; }`,
    `        case __Op.LT:`,
    `        case __Op.LTE:`,
    `        case __Op.GT:`,
    `        case __Op.GTE: {`,
    `          const b=stack.pop(), a=stack.pop();`,
    `          const ta=__typeOf(a), tb=__typeOf(b);`,
    `          if(!((ta==="number"&&tb==="number")||(ta==="string"&&tb==="string"))) throw new Error("Type error: comparison expects number/number or string/string");`,
    `          if(op===__Op.LT) stack.push(a<b);`,
    `          else if(op===__Op.LTE) stack.push(a<=b);`,
    `          else if(op===__Op.GT) stack.push(a>b);`,
    `          else stack.push(a>=b);`,
    `          break;`,
    `        }`,
    `        case __Op.JMP: ip = code[ip++]; break;`,
    `        case __Op.JMPF: {`,
    `          const t = code[ip++];`,
    `          const c = stack.pop();`,
    `          if(!__assertBool(c, "if/while/for")) ip = t;`,
    `          break;`,
    `        }`,
    `        case __Op.CLOSURE: { const idx=code[ip++]; stack.push(new __Fn(idx, env, mod.funcs[idx].name)); break; }`,
    `        case __Op.CALL: {`,
    `          const argc=code[ip++];`,
    `          const a=[]; for(let i=0;i<argc;i++) a.push(stack.pop()); a.reverse();`,
    `          const cal=stack.pop();`,
    `          if(typeof cal==="function"){ stack.push(cal.apply(null,a)); break; }`,
    `          if(cal instanceof __Fn){ stack.push(__runFn(mod, cal.fnIndex, a, cal.env)); break; }`,
    `          throw new Error("Attempted to call non-function ("+__typeOf(cal)+")");`,
    `        }`,
    `        case __Op.RET: return stack.pop();`,
    `        case __Op.TRY: { const catchIp=code[ip++]; handlers.push({catchIp, stackDepth:stack.length}); break; }`,
    `        case __Op.ENDTRY: handlers.pop(); break;`,
    `        case __Op.THROW: throw new __Thrown(stack.pop());`,
    `        default: throw new Error("Unknown opcode: "+op+" ("+__opName(op)+")");`,
    `      }`,
    `    } catch(e){`,
    `      if(e instanceof __Thrown){`,
    `        if(handlers.length===0) throw new Error("Uncaught throw: "+__stringify(e.value));`,
    `        const h=handlers.pop();`,
    `        while(stack.length>h.stackDepth) stack.pop();`,
    `        stack.push(e.value);`,
    `        ip = h.catchIp;`,
    `        continue;`,
    `      }`,
    `      throw e;`,
    `    }`,
    `  }`,
    `  return null;`,
    `}`,
    ``,
    `// ----------------- Run logic module -----------------`,
    `const __root = new __Env(null);`,
    `__root.define("print", function(){ console.log([].slice.call(arguments).map(__stringify).join(" ")); return null; });`,
    `__root.define("load", function(){ throw new Error("load() is not available in browser"); });`,
    `__root.define("import", function(){ throw new Error("import() is not supported in browser bundle v1.1"); });`,
    `__runFn(__MOD, __MOD.mainIndex, [], __root);`,
    ``,
    `// ----------------- Interpolation evaluator (AST) -----------------`,
    `function __evalExpr(e, env){`,
    `  switch(e.kind){`,
    `    case "Literal": return e.value;`,
    `    case "Var": { const v = env.get(e.name); if(v===undefined) throw new Error("Undefined identifier: "+e.name); return v; }`,
    `    case "Unary": { const v=__evalExpr(e.expr,env); if(e.op==="MINUS"){ if(typeof v!=="number") throw new Error("Type error: unary '-' expects number"); return -v; } if(e.op==="BANG"){ if(typeof v!=="boolean") throw new Error("Type error: '!' expects bool"); return !v; } throw new Error("Unknown unary"); }`,
    `    case "Binary": {`,
    `      const a=__evalExpr(e.left,env), b=__evalExpr(e.right,env);`,
    `      if(e.op==="PLUS"){ const ta=__typeOf(a), tb=__typeOf(b); if(ta==="number"&&tb==="number") return a+b; if(ta==="string"&&tb==="string") return a+b; throw new Error("Type error: '+' expects number+number or string+string"); }`,
    `      if(e.op==="MINUS"){ if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '-' expects number"); return a-b; }`,
    `      if(e.op==="STAR"){ if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '*' expects number"); return a*b; }`,
    `      if(e.op==="SLASH"){ if(typeof a!=="number"||typeof b!=="number") throw new Error("Type error: '/' expects number"); return a/b; }`,
    `      if(e.op==="EQEQ"){ return __typeOf(a)!==__typeOf(b)?false:(a===b); }`,
    `      if(e.op==="BANGEQ"){ return __typeOf(a)!==__typeOf(b)?true:(a!==b); }`,
    `      if(e.op==="LT"||e.op==="LTE"||e.op==="GT"||e.op==="GTE"){`,
    `        const ta=__typeOf(a), tb=__typeOf(b);`,
    `        if(!((ta==="number"&&tb==="number")||(ta==="string"&&tb==="string"))) throw new Error("Type error: comparison expects number/number or string/string");`,
    `        if(e.op==="LT") return a<b; if(e.op==="LTE") return a<=b; if(e.op==="GT") return a>b; return a>=b;`,
    `      }`,
    `      throw new Error("Unknown binary op");`,
    `    }`,
    `    case "List": return e.items.map(x=>__evalExpr(x,env));`,
    `    case "Object": { const o=Object.create(null); for(const p of e.props){ o[p.key]=__evalExpr(p.value,env); } return o; }`,
    `    case "Member": {`,
    `      const obj=__evalExpr(e.object,env);`,
    `      if(Array.isArray(obj)){`,
    `        if(e.prop==="len") return obj.length;`,
    `        if(e.prop==="get") return __makeListGet(obj);`,
    `        if(e.prop==="set") return __makeListSet(obj);`,
    `        throw new Error("Unknown list property: "+e.prop);`,
    `      }`,
    `      if(obj===null) throw new Error("Cannot access property of null");`,
    `      if(typeof obj!=="object") throw new Error("Cannot access member '"+e.prop+"' on "+__typeOf(obj));`,
    `      if(!(e.prop in obj)) throw new Error("Property not found: "+e.prop);`,
    `      return obj[e.prop];`,
    `    }`,
    `    case "Call": {`,
    `      const cal=__evalExpr(e.callee,env);`,
    `      const args=e.args.map(a=>__evalExpr(a,env));`,
    `      if(typeof cal==="function") return cal.apply(null,args);`,
    `      if(cal instanceof __Fn) return __runFn(__MOD, cal.fnIndex, args, cal.env);`,
    `      throw new Error("Attempted to call non-function ("+__typeOf(cal)+")");`,
    `    }`,
    `    default: throw new Error("Unknown expr kind: "+e.kind);`,
    `  }`,
    `}`,
    ``,
    `// ----------------- DOM builder -----------------`,
    `const __NODES = ${nodesJson};`,
    `function __text(v){ return document.createTextNode(String(v)); }`,
    `function __el(tag){ return document.createElement(tag); }`,
    `function __setAttr(el,k,v){ el.setAttribute(k,String(v)); }`,
    ``,
    `function __bindEvent(el, attrName, attrValue){`,
    `  // attrName like onClick, onInput, etc. -> event 'click','input'`,
    `  const m = /^on([A-Z].*)$/.exec(attrName);`,
    `  if(!m) return false;`,
    `  const evt = m[1].toLowerCase();`,
    `  // attrValue is string like "handler()" or "handler(1)"`,
    `  el.addEventListener(evt, function(ev){`,
    `    try{`,
    `      // expose 'event' in env temporarily`,
    `      const old = __root.get("event");`,
    `      if(old===undefined) __root.define("event", ev, null); else __root.set("event", ev);`,
    `      // Evaluate as a tiny "call expression" by parsing just a function call pattern is out of scope; we do a safe eval-like mapping:`,
    `      // - If it ends with ')', we treat it as a JS call against exported env values (functions).`,
    `      // - We support: name() and name(arg,...) where args are numbers/strings/bools/null only.`,
    `      const s = String(attrValue).trim();`,
    `      const mm = /^([A-Za-z_][A-Za-z0-9_]*)\\s*\\((.*)\\)\\s*$/.exec(s);`,
    `      if(!mm) throw new Error("Invalid event handler expression: "+s);`,
    `      const fnName = mm[1];`,
    `      const fn = __root.get(fnName);`,
    `      if(typeof fn!=="function" && !(fn instanceof __Fn)) throw new Error("Handler is not a function: "+fnName);`,
    `      const argText = mm[2].trim();`,
    `      const args = [];`,
    `      if(argText.length){`,
    `        // very small literal arg parser: splits by comma, supports numbers, "strings", true/false/null`,
    `        const parts = argText.split(",").map(x=>x.trim()).filter(Boolean);`,
    `        for(const p of parts){`,
    `          if(/^\\d+(?:\\.\\d+)?$/.test(p)) args.push(Number(p));`,
    `          else if(p==="true") args.push(true);`,
    `          else if(p==="false") args.push(false);`,
    `          else if(p==="null") args.push(null);`,
    `          else if(/^".*"$/.test(p)) args.push(p.slice(1,-1));`,
    `          else throw new Error("Only literal args supported in event handler: "+p);`,
    `        }`,
    `      }`,
    `      if(typeof fn==="function") fn.apply(null,args);`,
    `      else __runFn(__MOD, fn.fnIndex, args, fn.env);`,
    `    }catch(e){ console.error(e); }`,
    `  });`,
    `  return true;`,
    `}`,
    ``,
    `function __emitNode(n, parent){`,
    `  if(n.kind==="Text"){ parent.appendChild(__text(n.value)); return; }`,
    `  if(n.kind==="Interp"){`,
    `    const v = __evalExpr(n.expr, __root);`,
    `    parent.appendChild(__text(v===null?"":v));`,
    `    return;`,
    `  }`,
    `  if(n.kind==="Element"){`,
    `    const el = __el(n.tag);`,
    `    for(const a of (n.attrs||[])){`,
    `      if(!__bindEvent(el, a.name, a.value)){`,
    `        __setAttr(el, a.name, a.value);`,
    `      }`,
    `    }`,
    `    parent.appendChild(el);`,
    `    for(const c of (n.children||[])) __emitNode(c, el);`,
    `    return;`,
    `  }`,
    `}`,
    ``,
    `const __frag = document.createDocumentFragment();`,
    `for(const n of __NODES) __emitNode(n, __frag);`,
    `document.body.appendChild(__frag);`,
    `})();`,
  ].join("\n");
}

// ============================================================
// 11) Formatter (backend + frontend)
// ============================================================

function indent(n) { return "  ".repeat(n); }

function opToStr(op) {
  return {
    [TokenKind.PLUS]: "+",
    [TokenKind.MINUS]: "-",
    [TokenKind.STAR]: "*",
    [TokenKind.SLASH]: "/",
    [TokenKind.EQEQ]: "==",
    [TokenKind.BANGEQ]: "!=",
    [TokenKind.LT]: "<",
    [TokenKind.LTE]: "<=",
    [TokenKind.GT]: ">",
    [TokenKind.GTE]: ">=",
  }[op] || "?";
}

function fmtExpr(e) {
  switch (e.kind) {
    case "Literal":
      if (typeof e.value === "string") return `"${e.value.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
      if (e.value === null) return "null";
      return String(e.value);
    case "Var":
      return e.name;
    case "Unary":
      return (e.op === TokenKind.MINUS ? "-" : "!") + fmtExpr(e.expr);
    case "Binary":
      return `${fmtExpr(e.left)} ${opToStr(e.op)} ${fmtExpr(e.right)}`;
    case "Call":
      return `${fmtExpr(e.callee)}(${e.args.map(fmtExpr).join(", ")})`;
    case "Member":
      return `${fmtExpr(e.object)}.${e.prop}`;
    case "List":
      return `(${e.items.map(fmtExpr).join(", ")})`;
    case "Object":
      return `{ ${e.props.map(p => `${p.key}: ${fmtExpr(p.value)}`).join(", ")} }`;
    default:
      return "<expr>";
  }
}

function fmtStmt(s, depth) {
  switch (s.kind) {
    case "ImportDecl":
      return indent(depth) + `import "${s.path}"`;

    case "LetStmt": {
      const head = (s.exported ? "export " : "") + "let";
      return indent(depth) + `${head} ${s.name}` + (s.typeRef ? `: ${s.typeRef.name}` : "") + ` = ${fmtExpr(s.value)}`;
    }

    case "AssignStmt":
      return indent(depth) + `${fmtExpr(s.target)} = ${fmtExpr(s.value)}`;

    case "ExprStmt":
      return indent(depth) + fmtExpr(s.expr);

    case "ReturnStmt":
      return indent(depth) + (s.expr ? `return ${fmtExpr(s.expr)}` : "return");

    case "ThrowStmt":
      return indent(depth) + `throw ${fmtExpr(s.expr)}`;

    case "BreakStmt":
      return indent(depth) + "break";

    case "ContinueStmt":
      return indent(depth) + "continue";

    case "IfStmt": {
      const thenBody = s.thenBlock.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      let out = indent(depth) + `if (${fmtExpr(s.cond)}) {\n` + thenBody + `\n${indent(depth)}}`;
      if (s.elseBlock) {
        const elseBody = s.elseBlock.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
        out += ` else {\n` + elseBody + `\n${indent(depth)}}`;
      }
      return out;
    }

    case "WhileStmt": {
      const body = s.body.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      return indent(depth) + `while (${fmtExpr(s.cond)}) {\n` + body + `\n${indent(depth)}}`;
    }

    case "ForStmt": {
      const init = s.init ? stripTerminatorsForFmt(fmtStmt(s.init, 0)) : "";
      const cond = s.cond ? fmtExpr(s.cond) : "";
      const upd = s.update ? stripTerminatorsForFmt(fmtStmt(s.update, 0)) : "";
      const body = s.body.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      return indent(depth) + `for (${init}; ${cond}; ${upd}) {\n` + body + `\n${indent(depth)}}`;
    }

    case "TryCatchStmt": {
      const tb = s.tryBlock.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      const cb = s.catchBlock.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      return indent(depth) + `try {\n${tb}\n${indent(depth)}} catch (${s.catchName}) {\n${cb}\n${indent(depth)}}`;
    }

    case "Block": {
      const body = s.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      return indent(depth) + "{\n" + body + `\n${indent(depth)}}`;
    }

    default:
      return indent(depth) + "<stmt>";
  }
}

function stripTerminatorsForFmt(s) {
  return String(s).replace(/[\r\n]+/g, " ").trim();
}

function fmtProgram(program) {
  const parts = [];
  for (const item of program.items) {
    if (item.kind === "FuncDecl") {
      const head = (item.exported ? "export " : "") + "fn";
      const sig =
        `${head} ${item.name}(` +
        item.params.map(p => p.name + (p.typeRef ? `: ${p.typeRef.name}` : "")).join(", ") +
        `)` +
        (item.returnType ? `: ${item.returnType.name}` : "");
      const body = item.body.statements.map(s => fmtStmt(s, 1)).join("\n");
      parts.push(sig + ` {\n` + body + `\n}`);
    } else {
      parts.push(fmtStmt(item, 0));
    }
  }
  return parts.filter(Boolean).join("\n\n") + "\n";
}

function fmtFrontendNode(n, depth) {
  if (n.kind === "Text") return indent(depth) + `"${n.value.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
  if (n.kind === "Interp") {
// best-effort
    if (n.kind === "Interp") {
      // best-effort: reuse backend formatter on the embedded expression
      return indent(depth) + `{ ${fmtExpr(n.expr)} }`;
    }
    if (n.kind === "Element") {
      const attrs = (n.attrs || [])
        .map(a => `${a.name}="${String(a.value).replace(/"/g, "&quot;")}"`)
        .join(" ");
      const open = attrs.length ? `<${n.tag} ${attrs}>` : `<${n.tag}>`;
      if (!n.children || n.children.length === 0) {
        return indent(depth) + open + `</${n.tag}>`;
      }
      const kids = n.children.map(c => fmtFrontendNode(c, depth + 1)).join("\n");
      return indent(depth) + open + "\n" + kids + "\n" + indent(depth) + `</${n.tag}>`;
    }
    return indent(depth) + "<unknown>";
  }
}

function fmtFrontendFile(parsedFrontend, logicProgramOrNull) {
  const parts = [];
  parts.push("@frontend");
  parts.push("");

  // logic
  parts.push("<logic>");
  if (logicProgramOrNull) parts.push(fmtProgram(logicProgramOrNull).trimEnd());
  else if (parsedFrontend.logicRaw && parsedFrontend.logicRaw.trim().length) parts.push(String(parsedFrontend.logicRaw).trimEnd());
  parts.push("</logic>");
  parts.push("");

  // style
  if (parsedFrontend.styleRaw && parsedFrontend.styleRaw.trim().length) {
    parts.push("<style>");
    parts.push(String(parsedFrontend.styleRaw).trimEnd());
    parts.push("</style>");
    parts.push("");
  }

  // markup
  const body = (parsedFrontend.nodes || []).map(n => fmtFrontendNode(n, 0)).join("\n");
  if (body.trim().length) parts.push(body.trimEnd());
  parts.push("");

  return parts.join("\n");
}

function formatFile(absPath, srcRaw) {
  const mode = findHeaderMode(srcRaw);
  const fileName = path.basename(absPath);

  if (mode === "backend") {
    const src = stripHeaderLine(srcRaw);
    const lex = new Lexer(fileName, src);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(fileName, tokens);
    let program = parser.parseProgram();
    // Analyzer is optional for formatting; but running it helps catch bugs early.
    analyzeProgram(fileName, src, program);
    program = optimizeProgram(program);
    return "@backend\n\n" + fmtProgram(program);
  }

  // frontend
  const fe = parseFrontend(fileName, srcRaw);
  let logicProgram = null;
  if (fe.logicRaw && fe.logicRaw.trim().length) {
    const logicName = fileName + "::logic";
    const lex = new Lexer(logicName, fe.logicRaw);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(logicName, tokens);
    logicProgram = parser.parseProgram();
    analyzeProgram(logicName, fe.logicRaw, logicProgram);
    logicProgram = optimizeProgram(logicProgram);
  }
  return fmtFrontendFile(fe, logicProgram);
}

// ============================================================
// 12) CLI Commands
// ============================================================

function parseArgs(argv) {
  // argv: process.argv.slice(2)
  const out = { cmd: null, file: null, flags: Object.create(null), rest: [] };
  if (!argv.length) return out;
  out.cmd = argv[0];
  for (let i = 1; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) {
      const eq = a.indexOf("=");
      if (eq !== -1) out.flags[a.slice(2, eq)] = a.slice(eq + 1);
      else out.flags[a.slice(2)] = true;
    } else if (!out.file && a.endsWith(".one")) {
      out.file = a;
    } else {
      out.rest.push(a);
    }
  }
  return out;
}

function usage() {
  console.log([
    "ONE v1.1 (single-file compiler)",
    "",
    "Usage:",
    "  node one.js run   <file.one>",
    "  node one.js build <file.one>",
    "  node one.js fmt   <file.one>",
    "  node one.js serve <file.one> [--port=3000]",
    "  node one.js test",
    "  node one.js init  [dir]",
    "",
    "Notes:",
    "  - Mode is determined by @backend / @frontend header (backend default).",
    "  - @frontend produces <file>.html and <file>.js in the same directory.",
  ].join("\n"));
}

  // ============================================================
  // 13) Server (serve frontend output)
  // ============================================================

  function contentTypeFor(p) {
    const ext = path.extname(p).toLowerCase();
    if (ext === ".html") return "text/html; charset=utf-8";
    if (ext === ".js") return "application/javascript; charset=utf-8";
    if (ext === ".css") return "text/css; charset=utf-8";
    if (ext === ".json") return "application/json; charset=utf-8";
    if (ext === ".png") return "image/png";
    if (ext === ".jpg" || ext === ".jpeg") return "image/jpeg";
    if (ext === ".svg") return "image/svg+xml; charset=utf-8";
    return "application/octet-stream";
  }

  function serveFrontendProject(absOnePath, port) {
    const absPath = path.resolve(absOnePath);
    const srcRaw = readFileText(absPath);
    const mode = findHeaderMode(srcRaw);
    if (mode !== "frontend") panic("serve expects a @frontend file.");

    const loader = new ModuleLoader();
    const { outHtml, outJs } = emitFrontendArtifacts(absPath, srcRaw, loader);

    const baseDir = path.dirname(absPath);
    const htmlName = path.basename(outHtml);
    const jsName = path.basename(outJs);

    const server = http.createServer((req, res) => {
      try {
        const u = url.parse(req.url || "/");
        let reqPath = decodeURIComponent(u.pathname || "/");
        if (reqPath === "/" || reqPath === "") reqPath = "/" + htmlName;

        // lock to baseDir
        const filePath = path.resolve(baseDir, "." + reqPath);
        if (!filePath.startsWith(baseDir)) {
          res.statusCode = 403;
          res.end("Forbidden");
          return;
        }
        if (!fileExists(filePath)) {
          res.statusCode = 404;
          res.end("Not found");
          return;
        }
        const data = fs.readFileSync(filePath);
        res.statusCode = 200;
        res.setHeader("Content-Type", contentTypeFor(filePath));
        res.end(data);
      } catch (e) {
        res.statusCode = 500;
        res.setHeader("Content-Type", "text/plain; charset=utf-8");
        res.end(String(e?.stack || e));
      }
    });

    server.listen(port, () => {
      console.log(`ONE serve: http://localhost:${port}/`);
      console.log(`Serving: ${htmlName} (and ${jsName})`);
    });
  }

  // ============================================================
  // 14) Tests (self-contained)
  // ============================================================

  function assertEq(actual, expected, msg) {
    const ok =
      (Number.isNaN(actual) && Number.isNaN(expected)) ||
      (actual === expected);
    if (!ok) throw new Error(`Assertion failed: ${msg || ""}\n  expected: ${expected}\n  actual:   ${actual}`);
  }

  function runAllTests() {
    // Core smoke tests using in-memory compilation and VM.
    const loader = new ModuleLoader();

    function runBackendString(virtualName, source) {
      const absPath = path.resolve(process.cwd(), virtualName);
      const fileName = path.basename(absPath);
      const mode = findHeaderMode(source);
      if (mode !== "backend") throw new Error("test helper expects @backend");
      const src = stripHeaderLine(source);

      const lex = new Lexer(fileName, src);
      const tokens = lex.tokenizeAll();
      const parser = new Parser(fileName, tokens);
      let program = parser.parseProgram();
      analyzeProgram(fileName, src, program);
      program = optimizeProgram(program);
      const mod = compileModuleFromAST(fileName, program, { moduleAbsPath: absPath, loader, isModule: false });
      const vm = new VM(mod);

      const env = makeRootEnvForBackend(fileName, process.cwd(), loader);
      // capture prints
      const printed = [];
      env.set("print", (...args) => { printed.push(args.map(stringifyValue).join(" ")); return null; });

      vm.runFunction(mod.mainIndex, [], env);
      return { env, printed };
    }

    // 1) let + arithmetic + while
    {
      const { env } = runBackendString("test1.one", `@backend
let x = 0
let i = 1
while (i <= 5) {
x = x + i
i = i + 1
}
`);
      assertEq(env.get("x"), 15, "sum 1..5");
    }

    // 2) functions + recursion + closures
    {
      const { env } = runBackendString("test2.one", `@backend
fn fact(n: number): number {
if (n == 0) { return 1 }
return n * fact(n - 1)
}

fn makeAdder(a: number) {
fn add(b: number) { return a + b }
return add
}

let f = fact(6)
let add10 = makeAdder(10)
let y = add10(32)
`);
      assertEq(env.get("f"), 720, "fact(6)");
      assertEq(env.get("y"), 42, "closure adder");
    }

    // 3) objects + member set/get
    {
      const { env } = runBackendString("test3.one", `@backend
let o = { a: 1, b: 2 }
o.a = o.a + 9
let z = o.a
`);
      assertEq(env.get("z"), 10, "object member assignment");
    }

    // 4) lists + .get .set .len
    {
      const { env } = runBackendString("test4.one", `@backend
let xs = (1, 2, 3)
let a = xs.get(0)
xs.set(1, 9)
let b = xs.get(1)
let n = xs.len
`);
      assertEq(env.get("a"), 1, "list get");
      assertEq(env.get("b"), 9, "list set");
      assertEq(env.get("n"), 3, "list len");
    }

    // 5) for loop + break/continue
    {
      const { env } = runBackendString("test5.one", `@backend
let s = 0
for (let i = 0; i < 10; i = i + 1) {
if (i == 5) { continue }
if (i == 8) { break }
s = s + i
}
`);
      // sum 0..7 excluding 5 => 0+1+2+3+4+6+7 = 23
      assertEq(env.get("s"), 23, "for loop break/continue");
    }

    // 6) try/catch + throw
    {
      const { env } = runBackendString("test6.one", `@backend
let ok = false
try {
throw "boom"
} catch (e) {
ok = (e == "boom")
}
`);
      assertEq(env.get("ok"), true, "try/catch throw");
    }

    // 7) strict bool in condition
    {
      let threw = false;
      try {
        runBackendString("test7.one", `@backend
let x = 1
if (x) { }
`);
      } catch (e) { threw = true; }
      assertEq(threw, true, "non-bool condition throws");
    }

    console.log("ONE tests: OK");
  }

  // ============================================================
  // 15) Init (scaffold)
  // ============================================================

  function initProject(dirArg) {
    const dir = dirArg ? path.resolve(dirArg) : process.cwd();
    mkdirp(dir);

    const mainBackend = path.join(dir, "main.one");
    const mainFrontend = path.join(dir, "app.one");

    if (!fileExists(mainBackend)) {
      writeFileText(mainBackend, `@backend
// ONE v1.1 example
fn main() {
print("Hello from ONE backend!")
}
main()
`);
    }

    if (!fileExists(mainFrontend)) {
      writeFileText(mainFrontend, `@frontend

<logic>
fn handler() {
print("clicked!")
}
</logic>

<style>
body { font-family: system-ui, Arial; padding: 24px; }
button { padding: 10px 14px; font-size: 16px; cursor: pointer; }
</style>

<div>
"ONE v1.1 Frontend Demo"
<button onClick="handler()">"Click me"</button>
</div>
`);
    }

    console.log("Initialized ONE project:");
    console.log(" - " + mainBackend);
    console.log(" - " + mainFrontend);
  }

  // ============================================================
  // 16) Main dispatcher
  // ============================================================

  function main() {
    const args = parseArgs(process.argv.slice(2));
    if (!args.cmd) { usage(); process.exit(0); }

    try {
      if (args.cmd === "test") {
        runAllTests();
        return;
      }

      if (args.cmd === "init") {
        const dir = args.rest[0] || null;
        initProject(dir);
        return;
      }

      if (args.cmd === "run") {
        if (!args.file) panic("run: missing <file.one>");
        const abs = path.resolve(args.file);
        const srcRaw = readFileText(abs);
        const mode = findHeaderMode(srcRaw);

        const loader = new ModuleLoader();

        if (mode === "backend") {
          runBackendFile(abs, srcRaw, loader);
          return;
        }

        // frontend: build artifacts then serve a quick file:// friendly message
        const { outHtml, outJs } = emitFrontendArtifacts(abs, srcRaw, loader);
        console.log("Built frontend:");
        console.log(" - " + outHtml);
        console.log(" - " + outJs);
        console.log("Open the .html in a browser, or use: node one.js serve " + args.file);
        return;
      }

      if (args.cmd === "build") {
        if (!args.file) panic("build: missing <file.one>");
        const abs = path.resolve(args.file);
        const srcRaw = readFileText(abs);
        const mode = findHeaderMode(srcRaw);

        const loader = new ModuleLoader();

        if (mode === "backend") {
          const out = buildBackendFile(abs, srcRaw, loader);
          console.log("Built backend bytecode:");
          console.log(" - " + out);
          return;
        }

        const { outHtml, outJs } = emitFrontendArtifacts(abs, srcRaw, loader);
        console.log("Built frontend:");
        console.log(" - " + outHtml);
        console.log(" - " + outJs);
        return;
      }

      if (args.cmd === "fmt") {
        if (!args.file) panic("fmt: missing <file.one>");
        const abs = path.resolve(args.file);
        const srcRaw = readFileText(abs);
        const formatted = formatFile(abs, srcRaw);
        writeFileText(abs, formatted);
        console.log("Formatted: " + abs);
        return;
      }

      if (args.cmd === "serve") {
        if (!args.file) panic("serve: missing <file.one>");
        const port = Number(args.flags.port || 3000) || 3000;
        serveFrontendProject(args.file, port);
        return;
      }

      usage();
      panic(`Unknown command: ${args.cmd}`);
    } catch (e) {
      // Try to print nice OneError formatting if possible
      if (e instanceof OneError) {
        // We may not have src handy here; best-effort:
        console.error(`${e.fileName}:${e.line}:${e.col} - ${e.kind}: ${e.message}`);
        process.exit(1);
      }
      console.error(String(e?.stack || e));
      process.exit(1);
    }
  }

  main();
