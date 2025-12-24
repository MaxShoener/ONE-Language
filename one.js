#!/usr/bin/env node
"use strict";

/**
 * ONE v0.1 (Balanced) — Compiler + Formatter + Module System (Step 3+)
 *
 * Commands:
 *   node one.js run <file.one>
 *   node one.js fmt <file.one>
 *   node one.js <file.one>        (same as run)
 *
 * Modes:
 *   @backend   (default)
 *   @frontend
 *
 * Module system (v0.1 step 3):
 *   - Supports BOTH:
 *       import "relative/path.one"          // statement form (side-effects + cache)
 *       let m = import("relative/path.one") // expression form (returns module exports object)
 *   - Adds exports:
 *       export let x = 1
 *       export fn add(a: number, b: number): number { return a + b }
 *
 * NEW in this build:
 *   - Constant folding
 *   - Dead code elimination (if(true/false), while(false), after-return)
 *   - Bytecode VM backend (default)
 */

const fs = require("fs");
const path = require("path");

// ----------------------------- Utilities -----------------------------

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

function isWhitespace(ch) {
  return ch === " " || ch === "\t" || ch === "\r";
}
function isNewline(ch) {
  return ch === "\n";
}
function isDigit(ch) {
  return ch >= "0" && ch <= "9";
}
function isAlpha(ch) {
  return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z");
}
function isIdentStart(ch) {
  return isAlpha(ch) || ch === "_";
}
function isIdentPart(ch) {
  return isIdentStart(ch) || isDigit(ch);
}

function indexToLineCol(src, idx) {
  let line = 1;
  let col = 1;
  for (let i = 0; i < idx && i < src.length; i++) {
    const ch = src[i];
    if (ch === "\n") {
      line++;
      col = 1;
    } else {
      col++;
    }
  }
  return { line, col };
}

function findHeaderMode(src) {
  const trimmed = src.trimStart();
  if (trimmed.startsWith("@frontend")) return "frontend";
  if (trimmed.startsWith("@backend")) return "backend";
  return "backend";
}

function stripHeaderLine(src) {
  return src.replace(/^\s*@(?:backend|frontend)[^\n]*\n/, "");
}

function enforceBackendNoFrontendTags(fileName, srcRaw) {
  // Disallow frontend tags like <div>, </div>, <logic>, <style> in backend mode.
  // Allow comparisons x < y because those do not match "<[A-Za-z]" pattern.
  const re = /<\s*\/?\s*[A-Za-z][A-Za-z0-9_-]*/g;
  const m = re.exec(srcRaw);
  if (!m) return;
  const idx = m.index;
  const { line, col } = indexToLineCol(srcRaw, idx);
  throwErr(
    fileName,
    line,
    col,
    `Backend mode does not allow frontend tags. Found '${m[0]}...'. Use @frontend for markup files.`
  );
}

// ----------------------------- Error Model -----------------------------

class OneError extends Error {
  constructor(message, fileName, line, col) {
    super(message);
    this.name = "OneError";
    this.fileName = fileName;
    this.line = line;
    this.col = col;
  }
}

function throwErr(fileName, line, col, message) {
  throw new OneError(message, fileName, line, col);
}

function formatError(err, src) {
  if (!(err instanceof OneError)) return String(err?.stack || err);
  const lines = src.split(/\r?\n/);
  const lineText = lines[err.line - 1] ?? "";
  const caretPad = " ".repeat(Math.max(0, err.col - 1));
  return [
    `${err.fileName}:${err.line}:${err.col} - error: ${err.message}`,
    lineText,
    `${caretPad}^`,
  ].join("\n");
}

// ----------------------------- Tokens -----------------------------

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
  KW_TYPE: "KW_TYPE",
  KW_IF: "KW_IF",
  KW_ELSE: "KW_ELSE",
  KW_WHILE: "KW_WHILE",
  KW_RETURN: "KW_RETURN",
  KW_TRUE: "KW_TRUE",
  KW_FALSE: "KW_FALSE",
  KW_NULL: "KW_NULL",
  KW_IMPORT: "KW_IMPORT",
  KW_EXPORT: "KW_EXPORT",

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
  ["type", TokenKind.KW_TYPE],
  ["if", TokenKind.KW_IF],
  ["else", TokenKind.KW_ELSE],
  ["while", TokenKind.KW_WHILE],
  ["return", TokenKind.KW_RETURN],
  ["true", TokenKind.KW_TRUE],
  ["false", TokenKind.KW_FALSE],
  ["null", TokenKind.KW_NULL],
  ["import", TokenKind.KW_IMPORT],
  ["export", TokenKind.KW_EXPORT],
]);

class Token {
  constructor(kind, value, line, col) {
    this.kind = kind;
    this.value = value;
    this.line = line;
    this.col = col;
  }
}

// ----------------------------- Backend Lexer -----------------------------

class Lexer {
  constructor(fileName, src) {
    this.fileName = fileName;
    this.src = src;
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
    if (ch === "\n") {
      this.line++;
      this.col = 1;
    } else {
      this.col++;
    }
    return ch;
  }

  make(kind, value, line, col) {
    return new Token(kind, value, line, col);
  }

  lexString() {
    const startLine = this.line;
    const startCol = this.col;
    this.advance(); // consume "
    let out = "";
    while (true) {
      const ch = this.peek();
      if (ch === "\0") throwErr(this.fileName, startLine, startCol, "Unterminated string literal");
      if (ch === "\"") {
        this.advance();
        break;
      }
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
    const startLine = this.line;
    const startCol = this.col;
    let s = "";
    while (isDigit(this.peek())) s += this.advance();
    if (this.peek() === "." && isDigit(this.peek(1))) {
      s += this.advance(); // .
      while (isDigit(this.peek())) s += this.advance();
    }
    return this.make(TokenKind.NUMBER, Number(s), startLine, startCol);
  }

  lexIdentOrKeyword() {
    const startLine = this.line;
    const startCol = this.col;
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

      if (isWhitespace(ch)) {
        this.advance();
        continue;
      }

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

// ----------------------------- AST helpers -----------------------------

function node(kind, props) {
  return Object.freeze({ kind, ...props });
}

// ----------------------------- Backend Parser -----------------------------

class Parser {
  constructor(fileName, tokens) {
    this.fileName = fileName;
    this.tokens = tokens;
    this.i = 0;
  }

  cur() { return this.tokens[this.i]; }
  at(kind) { return this.cur().kind === kind; }

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

  // Terminator required unless next is '}' or EOF
  requireTerminator(msg) {
    if (this.at(TokenKind.SEMI) || this.at(TokenKind.NEWLINE)) {
      this.eatTerminators();
      return;
    }
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
      else if (this.at(TokenKind.KW_TYPE)) items.push(this.parseTypeDecl());
      else if (this.at(TokenKind.KW_FN)) items.push(this.parseFuncDecl(false));
      else items.push(this.parseStatement(false));
      this.eatTerminators();
    }
    return node("Program", { items });
  }

  parseExportDecl() {
    const kw = this.eat(TokenKind.KW_EXPORT);
    if (this.at(TokenKind.KW_LET)) {
      const st = this.parseLet(true, kw.line, kw.col);
      return st;
    }
    if (this.at(TokenKind.KW_FN)) {
      const fn = this.parseFuncDecl(true, kw.line, kw.col);
      return fn;
    }
    throwErr(this.fileName, kw.line, kw.col, "Expected 'let' or 'fn' after export");
  }

  parseImportDecl() {
    const kw = this.eat(TokenKind.KW_IMPORT);
    const p = this.eat(TokenKind.STRING, "Expected string after import");
    this.requireTerminator("Expected end of statement after import");
    return node("ImportDecl", { line: kw.line, col: kw.col, path: p.value });
  }

  parseTypeDecl() {
    const kw = this.eat(TokenKind.KW_TYPE);
    const name = this.eat(TokenKind.IDENT, "Expected type name").value;
    this.eat(TokenKind.LBRACE, "Expected '{' after type name");
    this.eatTerminators();

    const fields = [];
    while (!this.at(TokenKind.RBRACE)) {
      if (this.at(TokenKind.EOF)) throwErr(this.fileName, kw.line, kw.col, "Unterminated type decl (missing '}')");
      const t = this.cur();
      const fieldName = this.eat(TokenKind.IDENT, "Expected field name").value;
      this.eat(TokenKind.COLON, "Expected ':' after field name");
      const typeRef = this.parseTypeRef();
      this.eatTerminators();
      fields.push({ name: fieldName, typeRef, line: t.line, col: t.col });
    }
    this.eat(TokenKind.RBRACE, "Expected '}' to close type");
    return node("TypeDecl", { line: kw.line, col: kw.col, name, fields });
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
      line: kw.line,
      col: kw.col,
      name,
      params,
      returnType,
      body,
      exported: !!isExported,
      exportLine,
      exportCol
    });
  }

  parseStatement() {
    if (this.at(TokenKind.KW_LET)) return this.parseLet(false);
    if (this.at(TokenKind.KW_IF)) return this.parseIf();
    if (this.at(TokenKind.KW_WHILE)) return this.parseWhile();
    if (this.at(TokenKind.KW_RETURN)) return this.parseReturn();
    if (this.at(TokenKind.LBRACE)) return this.parseBlock();

    // assignment or expr stmt
    if (this.at(TokenKind.IDENT)) {
      const save = this.i;
      const lv = this.parseLValue();
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
      line: kw.line,
      col: kw.col,
      name: nameTok.value,
      typeRef,
      value,
      exported: !!isExported,
      exportLine,
      exportCol
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

  parseTypeRef() {
    const t = this.cur();
    const nameTok = this.eat(TokenKind.IDENT, "Expected type name");
    return { name: nameTok.value, nullable: false, line: t.line, col: t.col };
  }

  parseLValue() {
    const t = this.cur();
    const base = this.eat(TokenKind.IDENT, "Expected identifier").value;
    const parts = [base];
    while (this.tryEat(TokenKind.DOT)) {
      parts.push(this.eat(TokenKind.IDENT, "Expected identifier after '.'").value);
    }
    return node("LValue", { line: t.line, col: t.col, parts });
  }

  // Expressions with precedence
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
    return this.parseCall();
  }

  parseCall() {
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

    if (this.at(TokenKind.LPAREN)) {
      const lp = this.eat(TokenKind.LPAREN);
      if (this.at(TokenKind.RPAREN)) {
        this.eat(TokenKind.RPAREN);
        return node("Tuple", { line: lp.line, col: lp.col, items: [] });
      }
      const first = this.parseExpr();
      if (this.tryEat(TokenKind.COMMA)) {
        const items = [first];
        while (true) {
          items.push(this.parseExpr());
          if (this.tryEat(TokenKind.COMMA)) continue;
          break;
        }
        this.eat(TokenKind.RPAREN, "Expected ')' after tuple/list");
        return node("Tuple", { line: lp.line, col: lp.col, items });
      }
      this.eat(TokenKind.RPAREN, "Expected ')' after expression");
      return first;
    }

    if (this.at(TokenKind.EQ)) {
      throwErr(this.fileName, t.line, t.col, "Unexpected '=' in expression. Did you mean '==' for comparison?");
    }

    throwErr(this.fileName, t.line, t.col, `Unexpected token in expression: ${t.kind}`);
  }
}

// ----------------------------- Types (minimal v0.1) -----------------------------

function typeOfValue(v) {
  if (v === null) return "null";
  if (Array.isArray(v)) return "tuple";
  switch (typeof v) {
    case "number": return "number";
    case "string": return "string";
    case "boolean": return "bool";
    case "object": return "object";
    case "function": return "function";
    default: return "unknown";
  }
}

function assertType(fileName, line, col, expectedKind, value) {
  const actualKind = typeOfValue(value);
  if (expectedKind !== actualKind) {
    throwErr(fileName, line, col, `Type error: expected ${expectedKind} but got ${actualKind}`);
  }
}

// ----------------------------- Runtime env + exports -----------------------------

class ReturnSignal {
  constructor(value) { this.value = value; }
}

class Env {
  constructor(parent = null) {
    this.parent = parent;
    this.values = Object.create(null);
    this.types = Object.create(null);
    this.funcs = Object.create(null);
    this.typesDecl = Object.create(null);
  }

  hasLocalVar(name) { return Object.prototype.hasOwnProperty.call(this.values, name); }
  hasLocalFunc(name) { return Object.prototype.hasOwnProperty.call(this.funcs, name); }

  defineVar(fileName, line, col, name, value, declaredType = null) {
    if (this.hasLocalVar(name) || this.hasLocalFunc(name)) {
      throwErr(fileName, line, col, `Redeclaration error: '${name}' is already defined in this scope`);
    }
    this.values[name] = value;
    if (declaredType) this.types[name] = declaredType;
  }

  defineFunc(fileName, line, col, name, fnValue) {
    if (this.hasLocalVar(name) || this.hasLocalFunc(name)) {
      throwErr(fileName, line, col, `Redeclaration error: '${name}' is already defined in this scope`);
    }
    this.funcs[name] = fnValue;
  }

  // NOTE: VM needs "defineOrSet" at global scope; keep setVar semantics same.
  setVar(name, value) {
    if (Object.prototype.hasOwnProperty.call(this.values, name)) {
      this.values[name] = value;
      return true;
    }
    if (this.parent) return this.parent.setVar(name, value);
    return false;
  }

  // Used by VM for top-level lets (global env only)
  setOrDefineGlobal(name, value) {
    if (!this.parent) {
      this.values[name] = value;
      return true;
    }
    return this.parent.setOrDefineGlobal(name, value);
  }

  get(name) {
    if (Object.prototype.hasOwnProperty.call(this.values, name)) return this.values[name];
    if (Object.prototype.hasOwnProperty.call(this.funcs, name)) return this.funcs[name];
    if (this.parent) return this.parent.get(name);
    return undefined;
  }

  defineType(name, decl) { this.typesDecl[name] = decl; }

  getDeclaredType(name) {
    if (Object.prototype.hasOwnProperty.call(this.types, name)) return this.types[name];
    if (this.parent) return this.parent.getDeclaredType(name);
    return null;
  }
}

function makeBuiltinPrint() {
  const f = (...args) => {
    console.log(
      args.map(a => (a === null ? "null" : Array.isArray(a) ? `(${a.map(String).join(", ")})` : String(a))).join(" ")
    );
    return null;
  };
  return f;
}

// ----------------------------- Optimizer Passes (NEW) -----------------------------

function isLiteral(node) {
  return node && node.kind === "Literal";
}

function foldExpr(fileName, expr) {
  if (!expr || typeof expr !== "object") return expr;

  switch (expr.kind) {
    case "Literal":
    case "Var":
      return expr;

    case "Tuple": {
      const items = expr.items.map(x => foldExpr(fileName, x));
      return node("Tuple", { ...expr, items });
    }

    case "Member": {
      const object = foldExpr(fileName, expr.object);
      return node("Member", { ...expr, object });
    }

    case "Call": {
      const callee = foldExpr(fileName, expr.callee);
      const args = expr.args.map(a => foldExpr(fileName, a));
      return node("Call", { ...expr, callee, args });
    }

    case "Unary": {
      const inner = foldExpr(fileName, expr.expr);
      if (inner.kind === "Literal") {
        if (expr.op === TokenKind.MINUS && typeof inner.value === "number") {
          return node("Literal", { line: expr.line, col: expr.col, value: -inner.value, litType: "number" });
        }
        if (expr.op === TokenKind.BANG && typeof inner.value === "boolean") {
          return node("Literal", { line: expr.line, col: expr.col, value: !inner.value, litType: "bool" });
        }
      }
      return node("Unary", { ...expr, expr: inner });
    }

    case "Binary": {
      const left = foldExpr(fileName, expr.left);
      const right = foldExpr(fileName, expr.right);

      if (left.kind === "Literal" && right.kind === "Literal") {
        const a = left.value;
        const b = right.value;

        // Keep folding rules minimal & safe for v0.1
        switch (expr.op) {
          case TokenKind.PLUS:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a + b, litType: "number" });
            if (typeof a === "string" && typeof b === "string") return node("Literal", { line: expr.line, col: expr.col, value: a + b, litType: "string" });
            break;
          case TokenKind.MINUS:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a - b, litType: "number" });
            break;
          case TokenKind.STAR:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a * b, litType: "number" });
            break;
          case TokenKind.SLASH:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a / b, litType: "number" });
            break;
          case TokenKind.EQEQ:
            return node("Literal", { line: expr.line, col: expr.col, value: a === b, litType: "bool" });
          case TokenKind.BANGEQ:
            return node("Literal", { line: expr.line, col: expr.col, value: a !== b, litType: "bool" });
          case TokenKind.LT:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a < b, litType: "bool" });
            if (typeof a === "string" && typeof b === "string") return node("Literal", { line: expr.line, col: expr.col, value: a < b, litType: "bool" });
            break;
          case TokenKind.LTE:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a <= b, litType: "bool" });
            if (typeof a === "string" && typeof b === "string") return node("Literal", { line: expr.line, col: expr.col, value: a <= b, litType: "bool" });
            break;
          case TokenKind.GT:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a > b, litType: "bool" });
            if (typeof a === "string" && typeof b === "string") return node("Literal", { line: expr.line, col: expr.col, value: a > b, litType: "bool" });
            break;
          case TokenKind.GTE:
            if (typeof a === "number" && typeof b === "number") return node("Literal", { line: expr.line, col: expr.col, value: a >= b, litType: "bool" });
            if (typeof a === "string" && typeof b === "string") return node("Literal", { line: expr.line, col: expr.col, value: a >= b, litType: "bool" });
            break;
          default:
            break;
        }
      }

      return node("Binary", { ...expr, left, right });
    }

    default:
      return expr;
  }
}

function foldInStmt(fileName, st) {
  switch (st.kind) {
    case "LetStmt":
      return node("LetStmt", { ...st, value: foldExpr(fileName, st.value) });
    case "AssignStmt":
      return node("AssignStmt", { ...st, value: foldExpr(fileName, st.value) });
    case "ExprStmt":
      return node("ExprStmt", { ...st, expr: foldExpr(fileName, st.expr) });
    case "ReturnStmt":
      return node("ReturnStmt", { ...st, expr: st.expr ? foldExpr(fileName, st.expr) : null });
    case "IfStmt":
      return node("IfStmt", {
        ...st,
        cond: foldExpr(fileName, st.cond),
        thenBlock: foldInBlock(fileName, st.thenBlock),
        elseBlock: st.elseBlock ? foldInBlock(fileName, st.elseBlock) : null
      });
    case "WhileStmt":
      return node("WhileStmt", { ...st, cond: foldExpr(fileName, st.cond), body: foldInBlock(fileName, st.body) });
    case "Block":
      return foldInBlock(fileName, st);
    default:
      return st;
  }
}

function foldInBlock(fileName, block) {
  const statements = block.statements.map(s => foldInStmt(fileName, s));
  return node("Block", { ...block, statements });
}

function dceBlock(fileName, block) {
  const out = [];
  for (const st of block.statements) {
    // Return-based DCE: remove anything after return
    if (out.length > 0 && out[out.length - 1].kind === "ReturnStmt") break;

    if (st.kind === "IfStmt" && isLiteral(st.cond) && typeof st.cond.value === "boolean") {
      const chosen = st.cond.value ? st.thenBlock : st.elseBlock;
      if (chosen) {
        const inlined = dceBlock(fileName, chosen).statements;
        out.push(...inlined);
      }
      continue;
    }

    if (st.kind === "WhileStmt" && isLiteral(st.cond) && st.cond.value === false) {
      // while(false) never runs
      continue;
    }

    if (st.kind === "IfStmt") {
      out.push(node("IfStmt", {
        ...st,
        thenBlock: dceBlock(fileName, st.thenBlock),
        elseBlock: st.elseBlock ? dceBlock(fileName, st.elseBlock) : null
      }));
      continue;
    }

    if (st.kind === "WhileStmt") {
      out.push(node("WhileStmt", { ...st, body: dceBlock(fileName, st.body) }));
      continue;
    }

    if (st.kind === "Block") {
      out.push(dceBlock(fileName, st));
      continue;
    }

    out.push(st);
  }
  return node("Block", { ...block, statements: out });
}

function optimizeProgram(fileName, program) {
  // 1) constant fold expressions everywhere
  const items1 = program.items.map(it => {
    if (it.kind === "FuncDecl") {
      return node("FuncDecl", { ...it, body: foldInBlock(fileName, it.body) });
    }
    if (it.kind === "TypeDecl") return it;
    return foldInStmt(fileName, it);
  });

  // 2) DCE on blocks (functions + inline blocks)
  const items2 = items1.map(it => {
    if (it.kind === "FuncDecl") {
      return node("FuncDecl", { ...it, body: dceBlock(fileName, it.body) });
    }
    if (it.kind === "Block") return dceBlock(fileName, it);
    // top-level if/while can be simplified by wrapping in a synthetic block
    if (it.kind === "IfStmt" || it.kind === "WhileStmt") {
      const b = dceBlock(fileName, node("Block", { line: it.line, col: it.col, statements: [it] }));
      return b.statements[0] || null;
    }
    return it;
  }).filter(Boolean);

  return node("Program", { items: items2 });
}

// ----------------------------- Module Loader (Step 3) -----------------------------

class ModuleLoader {
  constructor() {
    this.cache = new Map();      // absPath -> { exports }
    this.loading = new Set();    // absPath currently being loaded
    this.stack = [];             // for error messages
  }

  resolve(specifier, fromDir) {
    if (typeof specifier !== "string") return null;
    let p = specifier;
    if (!path.extname(p)) p += ".one";
    const abs = path.isAbsolute(p) ? p : path.resolve(fromDir, p);
    return abs;
  }

  loadModule(absPath) {
    if (this.cache.has(absPath)) return this.cache.get(absPath);

    if (this.loading.has(absPath)) {
      const chain = [...this.stack, absPath].map(x => path.basename(x)).join(" -> ");
      throw new Error(`Circular import detected: ${chain}`);
    }

    this.loading.add(absPath);
    this.stack.push(absPath);

    const srcRaw = readFileText(absPath);
    const fileName = path.basename(absPath);
    const mode = findHeaderMode(srcRaw);

    if (mode !== "backend") {
      throw new OneError("Only @backend modules can be imported in v0.1", fileName, 1, 1);
    }

    enforceBackendNoFrontendTags(fileName, srcRaw);
    const src = stripHeaderLine(srcRaw);

    const lex = new Lexer(fileName, src);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(fileName, tokens);
    let program = parser.parseProgram();

    // NEW: optimize module program too
    program = optimizeProgram(fileName, program);

    const exportNames = [];
    for (const it of program.items) {
      if (it.kind === "LetStmt" && it.exported) exportNames.push(it.name);
      if (it.kind === "FuncDecl" && it.exported) exportNames.push(it.name);
    }

    const env = new Env(null);
    env.defineFunc(fileName, 1, 1, "print", makeBuiltinPrint());

    const importer = (spec) => {
      if (typeof spec !== "string") throw new OneError("import() expects a string path", fileName, 1, 1);
      const resolved = this.resolve(spec, path.dirname(absPath));
      const rec = this.loadModule(resolved);
      return rec.exports;
    };
    env.defineFunc(fileName, 1, 1, "import", importer);

    // install functions/types first
    installTopLevelDecls(fileName, src, env, program, this, absPath);

    // execute top-level with VM by default
    runProgramBackend(fileName, src, env, program, this, absPath);

    const exportsObj = Object.create(null);
    for (const name of exportNames) {
      const v = env.get(name);
      if (v === undefined) throw new OneError(`Export '${name}' not found at runtime`, fileName, 1, 1);
      exportsObj[name] = v;
    }

    const record = { exports: exportsObj };
    this.cache.set(absPath, record);

    this.stack.pop();
    this.loading.delete(absPath);

    return record;
  }
}

// ----------------------------- Interpreter (Backend) -----------------------------

function evalExpr(fileName, src, env, expr, loader, currentAbsPath) {
  switch (expr.kind) {
    case "Literal":
      return expr.value;

    case "Var": {
      const v = env.get(expr.name);
      if (v === undefined) throwErr(fileName, expr.line, expr.col, `Undefined identifier: ${expr.name}`);
      return v;
    }

    case "Tuple":
      return expr.items.map(e => evalExpr(fileName, src, env, e, loader, currentAbsPath));

    case "Unary": {
      const v = evalExpr(fileName, src, env, expr.expr, loader, currentAbsPath);
      if (expr.op === TokenKind.MINUS) {
        assertType(fileName, expr.line, expr.col, "number", v);
        return -v;
      }
      if (expr.op === TokenKind.BANG) {
        assertType(fileName, expr.line, expr.col, "bool", v);
        return !v;
      }
      throwErr(fileName, expr.line, expr.col, "Unknown unary operator");
    }

    case "Binary": {
      const left = evalExpr(fileName, src, env, expr.left, loader, currentAbsPath);
      const right = evalExpr(fileName, src, env, expr.right, loader, currentAbsPath);

      const lt = typeOfValue(left);
      const rt = typeOfValue(right);

      switch (expr.op) {
        case TokenKind.PLUS:
          if (lt === "number" && rt === "number") return left + right;
          if (lt === "string" && rt === "string") return left + right;
          throwErr(fileName, expr.line, expr.col, `Type error: '+' expects number+number or string+string (got ${lt}+${rt})`);

        case TokenKind.MINUS:
          assertType(fileName, expr.line, expr.col, "number", left);
          assertType(fileName, expr.line, expr.col, "number", right);
          return left - right;

        case TokenKind.STAR:
          assertType(fileName, expr.line, expr.col, "number", left);
          assertType(fileName, expr.line, expr.col, "number", right);
          return left * right;

        case TokenKind.SLASH:
          assertType(fileName, expr.line, expr.col, "number", left);
          assertType(fileName, expr.line, expr.col, "number", right);
          return left / right;

        case TokenKind.EQEQ:
          if (lt !== rt) return false;
          return left === right;

        case TokenKind.BANGEQ:
          if (lt !== rt) return true;
          return left !== right;

        case TokenKind.LT:
        case TokenKind.LTE:
        case TokenKind.GT:
        case TokenKind.GTE: {
          if (lt === "number" && rt === "number") {
            if (expr.op === TokenKind.LT) return left < right;
            if (expr.op === TokenKind.LTE) return left <= right;
            if (expr.op === TokenKind.GT) return left > right;
            return left >= right;
          }
          if (lt === "string" && rt === "string") {
            if (expr.op === TokenKind.LT) return left < right;
            if (expr.op === TokenKind.LTE) return left <= right;
            if (expr.op === TokenKind.GT) return left > right;
            return left >= right;
          }
          throwErr(fileName, expr.line, expr.col, `Type error: comparison expects number/number or string/string (got ${lt}/${rt})`);
        }

        default:
          throwErr(fileName, expr.line, expr.col, "Unknown binary operator");
      }
    }

    case "Member": {
      const obj = evalExpr(fileName, src, env, expr.object, loader, currentAbsPath);
      if (obj === null) throwErr(fileName, expr.line, expr.col, "Cannot access property of null");

      if (Array.isArray(obj)) {
        if (expr.prop === "len") return obj.length;
      }

      if (typeof obj === "object" && obj !== null) {
        if (!(expr.prop in obj)) throwErr(fileName, expr.line, expr.col, `Property not found: ${expr.prop}`);
        return obj[expr.prop];
      }

      throwErr(fileName, expr.line, expr.col, `Cannot access member '${expr.prop}' on ${typeOfValue(obj)}`);
    }

    case "Call": {
      const callee = evalExpr(fileName, src, env, expr.callee, loader, currentAbsPath);
      const args = expr.args.map(a => evalExpr(fileName, src, env, a, loader, currentAbsPath));
      if (typeof callee !== "function") {
        throwErr(fileName, expr.line, expr.col, `Attempted to call non-function (${typeOfValue(callee)})`);
      }
      return callee(...args);
    }

    default:
      throwErr(fileName, expr.line, expr.col, `Unknown expression kind: ${expr.kind}`);
  }
}

function execBlock(fileName, src, env, blockNode, loader, currentAbsPath) {
  for (const st of blockNode.statements) {
    execStatement(fileName, src, env, st, loader, currentAbsPath);
  }
}

function execStatement(fileName, src, env, stmt, loader, currentAbsPath) {
  switch (stmt.kind) {
    case "ImportDecl": {
      const resolved = loader.resolve(stmt.path, path.dirname(currentAbsPath));
      loader.loadModule(resolved);
      return;
    }

    case "LetStmt": {
      const value = evalExpr(fileName, src, env, stmt.value, loader, currentAbsPath);

      if (stmt.typeRef) {
        const declared = stmt.typeRef.name;
        const actual = typeOfValue(value);
        if ((declared === "number" || declared === "string" || declared === "bool" || declared === "null") && declared !== actual) {
          throwErr(fileName, stmt.line, stmt.col, `Type error: ${stmt.name}: ${declared} but got ${actual}`);
        }
      }

      env.defineVar(fileName, stmt.line, stmt.col, stmt.name, value, stmt.typeRef ? { kind: stmt.typeRef.name } : null);
      return;
    }

    case "AssignStmt": {
      const parts = stmt.target.parts;
      if (parts.length !== 1) throwErr(fileName, stmt.line, stmt.col, "v0.1: assignment supports only simple variables (no obj.prop yet)");
      const name = parts[0];
      const value = evalExpr(fileName, src, env, stmt.value, loader, currentAbsPath);

      const declared = env.getDeclaredType(name);
      if (declared && declared.kind) {
        const actual = typeOfValue(value);
        if (declared.kind !== actual) throwErr(fileName, stmt.line, stmt.col, `Type error: ${name}: ${declared.kind} but got ${actual}`);
      }

      const ok = env.setVar(name, value);
      if (!ok) throwErr(fileName, stmt.line, stmt.col, `Assignment to undefined variable: ${name}`);
      return;
    }

    case "IfStmt": {
      const cond = evalExpr(fileName, src, env, stmt.cond, loader, currentAbsPath);
      assertType(fileName, stmt.line, stmt.col, "bool", cond);
      if (cond) execBlock(fileName, src, new Env(env), stmt.thenBlock, loader, currentAbsPath);
      else if (stmt.elseBlock) execBlock(fileName, src, new Env(env), stmt.elseBlock, loader, currentAbsPath);
      return;
    }

    case "WhileStmt": {
      while (true) {
        const cond = evalExpr(fileName, src, env, stmt.cond, loader, currentAbsPath);
        assertType(fileName, stmt.line, stmt.col, "bool", cond);
        if (!cond) break;
        execBlock(fileName, src, new Env(env), stmt.body, loader, currentAbsPath);
      }
      return;
    }

    case "ReturnStmt": {
      const value = stmt.expr ? evalExpr(fileName, src, env, stmt.expr, loader, currentAbsPath) : null;
      throw new ReturnSignal(value);
    }

    case "ExprStmt":
      evalExpr(fileName, src, env, stmt.expr, loader, currentAbsPath);
      return;

    case "Block":
      execBlock(fileName, src, new Env(env), stmt, loader, currentAbsPath);
      return;

    default:
      throwErr(fileName, stmt.line, stmt.col, `Unknown statement kind: ${stmt.kind}`);
  }
}

function installTopLevelDecls(fileName, src, env, program, loader, currentAbsPath) {
  for (const item of program.items) {
    if (item.kind === "TypeDecl") env.defineType(item.name, item);
  }

  for (const item of program.items) {
    if (item.kind !== "FuncDecl") continue;

    const fnDecl = item;
    const fn = (...args) => {
      const local = new Env(env);

      for (let i = 0; i < fnDecl.params.length; i++) {
        const p = fnDecl.params[i];
        const v = args[i] ?? null;

        if (p.typeRef) {
          const expected = p.typeRef.name;
          const actual = typeOfValue(v);
          if ((expected === "number" || expected === "string" || expected === "bool" || expected === "null") && expected !== actual) {
            throwErr(fileName, p.line, p.col, `Type error: param ${p.name}: ${expected} but got ${actual}`);
          }
        }

        local.defineVar(fileName, p.line, p.col, p.name, v, p.typeRef ? { kind: p.typeRef.name } : null);
      }

      try {
        // function body executed with VM too (below). Fall back to interpreter for safety if needed.
        const rv = runFunctionBodyBackend(fileName, src, local, fnDecl.body, loader, currentAbsPath);
        return rv;
      } catch (e) {
        if (e instanceof ReturnSignal) return e.value;
        throw e;
      }
    };

    env.defineFunc(fileName, fnDecl.line, fnDecl.col, fnDecl.name, fn);
  }
}

// ----------------------------- Bytecode VM (NEW) -----------------------------

const Op = Object.freeze({
  CONST: 0,
  LOAD: 1,
  STORE: 2,
  POP: 3,

  ADD: 10,
  SUB: 11,
  MUL: 12,
  DIV: 13,

  EQ: 20,
  NE: 21,
  LT: 22,
  LTE: 23,
  GT: 24,
  GTE: 25,

  NOT: 30,
  NEG: 31,

  JMP: 40,
  JMPF: 41, // jump if false

  CALL: 50,
  RETURN: 51,

  IMPORT_STMT: 60, // import "x"
});

function bcEmit(arr, op, a = null, b = null) {
  arr.push([op, a, b]);
  return arr.length - 1;
}

function bcPatch(arr, at, a = null, b = null) {
  const ins = arr[at];
  arr[at] = [ins[0], a !== null ? a : ins[1], b !== null ? b : ins[2]];
}

function compileProgramToBytecode(fileName, program) {
  const code = [];
  const consts = [];

  function cconst(v) {
    const idx = consts.length;
    consts.push(v);
    return idx;
  }

  function emitExpr(e) {
    switch (e.kind) {
      case "Literal":
        bcEmit(code, Op.CONST, cconst(e.value));
        return;

      case "Var":
        bcEmit(code, Op.LOAD, e.name);
        return;

      case "Tuple": {
        // v0.1 VM: keep tuples in interpreter path if they appear
        // (We still allow them, but compile them as runtime-built array via const list)
        const items = e.items.map(x => (x.kind === "Literal" ? x.value : null));
        const allLiteral = items.every(v => v !== null);
        if (!allLiteral) {
          // fallback by storing a thunk const that interpreter can evaluate isn't available;
          // so keep it simple: forbid non-literal tuples in VM for v0.1
          throwErr(fileName, e.line, e.col, "v0.1 VM: tuple items must be literals");
        }
        bcEmit(code, Op.CONST, cconst(items)); // arrays are objects; OK
        return;
      }

      case "Unary":
        emitExpr(e.expr);
        bcEmit(code, e.op === TokenKind.MINUS ? Op.NEG : Op.NOT);
        return;

      case "Binary":
        emitExpr(e.left);
        emitExpr(e.right);
        switch (e.op) {
          case TokenKind.PLUS: bcEmit(code, Op.ADD); return;
          case TokenKind.MINUS: bcEmit(code, Op.SUB); return;
          case TokenKind.STAR: bcEmit(code, Op.MUL); return;
          case TokenKind.SLASH: bcEmit(code, Op.DIV); return;

          case TokenKind.EQEQ: bcEmit(code, Op.EQ); return;
          case TokenKind.BANGEQ: bcEmit(code, Op.NE); return;
          case TokenKind.LT: bcEmit(code, Op.LT); return;
          case TokenKind.LTE: bcEmit(code, Op.LTE); return;
          case TokenKind.GT: bcEmit(code, Op.GT); return;
          case TokenKind.GTE: bcEmit(code, Op.GTE); return;

          default:
            throwErr(fileName, e.line, e.col, "v0.1 VM: unsupported binary op");
        }

      case "Member":
        // v0.1 VM doesn't support member access directly.
        // This keeps behavior consistent: if user uses member, interpreter path is needed.
        throwErr(fileName, e.line, e.col, "v0.1 VM: member access not supported yet (use interpreter fallback later)");

      case "Call": {
        // Only support calling a global function by name or a value on stack that is function.
        // We compile callee then args then CALL argc
        emitExpr(e.callee);
        for (const a of e.args) emitExpr(a);
        bcEmit(code, Op.CALL, e.args.length);
        return;
      }

      default:
        throwErr(fileName, e.line, e.col, `v0.1 VM: unsupported expr kind ${e.kind}`);
    }
  }

  function emitStmt(s) {
    switch (s.kind) {
      case "ImportDecl":
        bcEmit(code, Op.IMPORT_STMT, s.path);
        return;

      case "LetStmt":
        emitExpr(s.value);
        bcEmit(code, Op.STORE, s.name);
        return;

      case "AssignStmt":
        if (s.target.parts.length !== 1) throwErr(fileName, s.line, s.col, "v0.1 VM: assignment supports only simple variables");
        emitExpr(s.value);
        bcEmit(code, Op.STORE, s.target.parts[0]);
        return;

      case "ExprStmt":
        emitExpr(s.expr);
        bcEmit(code, Op.POP);
        return;

      case "ReturnStmt":
        if (s.expr) emitExpr(s.expr);
        else bcEmit(code, Op.CONST, cconst(null));
        bcEmit(code, Op.RETURN);
        return;

      case "Block":
        for (const st of s.statements) emitStmt(st);
        return;

      case "IfStmt": {
        emitExpr(s.cond);
        const jmpFalseAt = bcEmit(code, Op.JMPF, null);
        emitStmt(s.thenBlock);
        const jmpEndAt = bcEmit(code, Op.JMP, null);
        const elseStart = code.length;
        if (s.elseBlock) emitStmt(s.elseBlock);
        const end = code.length;
        bcPatch(code, jmpFalseAt, elseStart);
        bcPatch(code, jmpEndAt, end);
        return;
      }

      case "WhileStmt": {
        const loopStart = code.length;
        emitExpr(s.cond);
        const jmpFalseAt = bcEmit(code, Op.JMPF, null);
        emitStmt(s.body);
        bcEmit(code, Op.JMP, loopStart);
        const end = code.length;
        bcPatch(code, jmpFalseAt, end);
        return;
      }

      default:
        throwErr(fileName, s.line, s.col, `v0.1 VM: unsupported stmt kind ${s.kind}`);
    }
  }

  // compile only executable top-level (types/funcs are handled elsewhere)
  for (const item of program.items) {
    if (item.kind === "TypeDecl" || item.kind === "FuncDecl") continue;
    emitStmt(item);
  }

  // implicit return
  bcEmit(code, Op.CONST, consts.push(null) - 1);
  bcEmit(code, Op.RETURN);

  return { code, consts };
}

function runBytecodeVM(fileName, env, loader, currentAbsPath, bytecode) {
  const { code, consts } = bytecode;
  const stack = [];
  let ip = 0;

  const pop = () => {
    if (stack.length === 0) throwErr(fileName, 1, 1, "VM stack underflow");
    return stack.pop();
  };

  while (ip < code.length) {
    const [op, a] = code[ip++];

    switch (op) {
      case Op.CONST:
        stack.push(consts[a]);
        break;

      case Op.LOAD: {
        const v = env.get(a);
        if (v === undefined) throwErr(fileName, 1, 1, `Undefined identifier: ${a}`);
        stack.push(v);
        break;
      }

      case Op.STORE: {
        const v = pop();
        // at top-level and in function bodies we want assignment to behave the same:
        // - if variable exists in any scope, setVar
        // - if not, define at current env scope (v0.1 does that for lets; here we store for lets and assigns)
        if (!env.setVar(a, v)) {
          // define in current env scope
          env.defineVar(fileName, 1, 1, a, v, null);
        }
        break;
      }

      case Op.POP:
        pop();
        break;

      case Op.ADD: {
        const b = pop(), a1 = pop();
        stack.push(a1 + b);
        break;
      }
      case Op.SUB: {
        const b = pop(), a1 = pop();
        stack.push(a1 - b);
        break;
      }
      case Op.MUL: {
        const b = pop(), a1 = pop();
        stack.push(a1 * b);
        break;
      }
      case Op.DIV: {
        const b = pop(), a1 = pop();
        stack.push(a1 / b);
        break;
      }

      case Op.EQ: {
        const b = pop(), a1 = pop();
        stack.push(a1 === b);
        break;
      }
      case Op.NE: {
        const b = pop(), a1 = pop();
        stack.push(a1 !== b);
        break;
      }
      case Op.LT: {
        const b = pop(), a1 = pop();
        stack.push(a1 < b);
        break;
      }
      case Op.LTE: {
        const b = pop(), a1 = pop();
        stack.push(a1 <= b);
        break;
      }
      case Op.GT: {
        const b = pop(), a1 = pop();
        stack.push(a1 > b);
        break;
      }
      case Op.GTE: {
        const b = pop(), a1 = pop();
        stack.push(a1 >= b);
        break;
      }

      case Op.NOT:
        stack.push(!pop());
        break;

      case Op.NEG:
        stack.push(-pop());
        break;

      case Op.JMP:
        ip = a;
        break;

      case Op.JMPF: {
        const cond = pop();
        if (!cond) ip = a;
        break;
      }

      case Op.CALL: {
        const argc = a;
        const args = [];
        for (let i = 0; i < argc; i++) args.unshift(pop());
        const callee = pop();
        if (typeof callee !== "function") throwErr(fileName, 1, 1, `Attempted to call non-function (${typeOfValue(callee)})`);
        const rv = callee(...args);
        stack.push(rv);
        break;
      }

      case Op.IMPORT_STMT: {
        const resolved = loader.resolve(a, path.dirname(currentAbsPath));
        loader.loadModule(resolved);
        break;
      }

      case Op.RETURN:
        return pop();

      default:
        throwErr(fileName, 1, 1, `Unknown VM opcode: ${op}`);
    }
  }
  return null;
}

function runProgramBackend(fileName, src, env, program, loader, absPath) {
  // VM by default; fallback to interpreter if VM hits unsupported nodes.
  const USE_VM = true;

  if (USE_VM) {
    // compile + run VM
    const bc = compileProgramToBytecode(fileName, program);
    runBytecodeVM(fileName, env, loader, absPath, bc);
    return;
  }

  // interpreter fallback
  for (const item of program.items) {
    if (item.kind === "TypeDecl" || item.kind === "FuncDecl") continue;
    execStatement(fileName, src, env, item, loader, absPath);
  }
}

function runFunctionBodyBackend(fileName, src, env, blockNode, loader, absPath) {
  // Minimal: compile block to bytecode and run it.
  const fakeProgram = node("Program", { items: [blockNode] });
  const bc = compileProgramToBytecode(fileName, fakeProgram);
  return runBytecodeVM(fileName, env, loader, absPath, bc);
}

// ----------------------------- Backend Runner (updated) -----------------------------

function runBackend(absPath, fileName, srcRaw, loader) {
  enforceBackendNoFrontendTags(fileName, srcRaw);

  const src = stripHeaderLine(srcRaw);

  const lex = new Lexer(fileName, src);
  const tokens = lex.tokenizeAll();
  const parser = new Parser(fileName, tokens);
  let program = parser.parseProgram();

  // NEW: Optimize before execution
  program = optimizeProgram(fileName, program);

  const env = new Env(null);

  // builtins
  env.defineFunc(fileName, 1, 1, "print", makeBuiltinPrint());

  // import() expression builtin
  const importer = (spec) => {
    if (typeof spec !== "string") throwErr(fileName, 1, 1, "import() expects a string path");
    const resolved = loader.resolve(spec, path.dirname(absPath));
    const rec = loader.loadModule(resolved);
    return rec.exports;
  };
  env.defineFunc(fileName, 1, 1, "import", importer);

  // install types/functions first
  installTopLevelDecls(fileName, src, env, program, loader, absPath);

  // execute top-level via VM
  runProgramBackend(fileName, src, env, program, loader, absPath);

  return { env, program };
}

// ----------------------------- Frontend v0.1 -----------------------------
// (unchanged semantics; used by `run` and `fmt`)

class FScanner {
  constructor(fileName, src) {
    this.fileName = fileName;
    this.src = src;
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
  let markupSrc = srcRaw;

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

  const ctx = Object.create(null);

  if (logicInner != null) {
    const logic = logicInner;
    const lex = new Lexer(fileName, logic);
    const tokens = lex.tokenizeAll();
    const parser = new Parser(fileName, tokens);
    const program = parser.parseProgram();

    for (const item of program.items) {
      if (item.kind !== "LetStmt") throwErr(fileName, item.line, item.col, "v0.1 <logic> supports only 'let name = literal'");
      if (item.value.kind !== "Literal") throwErr(fileName, item.line, item.col, "v0.1 <logic> lets must assign a literal");
      ctx[item.name] = item.value.value;
    }
  }

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

  function parseTextOrInterp() {
    s.skipWs();
    const ch = s.peek();
    if (ch === "\"") return parseStringLiteral();

    if (ch === "{") {
      const startLine = s.line, startCol = s.col;
      s.advance();
      s.skipWs();

      let expr;
      if (s.peek() === "\"") {
        const t = parseStringLiteral();
        expr = node("Literal", { line: t.line, col: t.col, value: t.value, litType: "string" });
      } else {
        if (!isIdentStart(s.peek())) throwErr(fileName, s.line, s.col, "Expected identifier in interpolation");
        let name = "";
        while (isIdentPart(s.peek())) name += s.advance();
        expr = node("Var", { line: startLine, col: startCol, name });
      }

      s.skipWs();
      if (s.peek() !== "}") throwErr(fileName, s.line, s.col, "Expected '}' to close interpolation");
      s.advance();
      return node("Interp", { line: startLine, col: startCol, expr });
    }

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

    s.advance();
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

      throwErr(fileName, s.line, s.col, 'v0.1 markup text must be quoted like "Hello" or use {interpolation}');
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
  return { ctx, styleText: styleInner ? styleInner : "", nodes, logicRaw: logicInner, styleRaw: styleInner, markupRaw: markupSrc };
}

function escapeJsString(s) {
  return String(s)
    .replace(/\\/g, "\\\\")
    .replace(/`/g, "\\`")
    .replace(/\$/g, "\\$")
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n");
}

function emitFrontendJS(fileName, frontendAst) {
  const { ctx, styleText, nodes } = frontendAst;
  const lines = [];
  lines.push(`// Generated by ONE v0.1 frontend compiler`);
  lines.push(`// Source: ${fileName}`);
  lines.push(`(function(){`);
  lines.push(`  "use strict";`);
  lines.push(`  const __ctx = ${JSON.stringify(ctx, null, 2)};`);
  lines.push(`  function __text(v){ return document.createTextNode(String(v)); }`);
  lines.push(`  function __el(tag){ return document.createElement(tag); }`);
  lines.push(`  function __setAttr(el, k, v){ el.setAttribute(k, String(v)); }`);

  if (styleText && styleText.trim().length) {
    lines.push(`  (function(){`);
    lines.push(`    const st = document.createElement("style");`);
    lines.push(`    st.textContent = \`${escapeJsString(styleText)}\`;`);
    lines.push(`    document.head.appendChild(st);`);
    lines.push(`  })();`);
  }

  lines.push(`  const __root = document.createDocumentFragment();`);

  function emitNode(n, parentVar, idx) {
    if (n.kind === "Text") {
      lines.push(`  ${parentVar}.appendChild(__text(\`${escapeJsString(n.value)}\`));`);
      return;
    }
    if (n.kind === "Interp") {
      if (n.expr.kind === "Var") {
        const nm = n.expr.name;
        lines.push(`  ${parentVar}.appendChild(__text(Object.prototype.hasOwnProperty.call(__ctx, ${JSON.stringify(nm)}) ? __ctx[${JSON.stringify(nm)}] : ""));`);
        return;
      }
      if (n.expr.kind === "Literal") {
        lines.push(`  ${parentVar}.appendChild(__text(\`${escapeJsString(n.expr.value)}\`));`);
        return;
      }
    }
    if (n.kind === "Element") {
      const v = `__n${idx}`;
      lines.push(`  const ${v} = __el(${JSON.stringify(n.tag)});`);
      for (const a of n.attrs) {
        lines.push(`  __setAttr(${v}, ${JSON.stringify(a.name)}, ${JSON.stringify(a.value)});`);
      }
      lines.push(`  ${parentVar}.appendChild(${v});`);
      let childIdx = idx * 1000 + 1;
      for (const c of n.children) emitNode(c, v, childIdx++);
      return;
    }
    lines.push(`  // Unknown node kind: ${n.kind}`);
  }

  let idx = 1;
  for (const n of nodes) emitNode(n, "__root", idx++);

  lines.push(`  document.body.appendChild(__root);`);
  lines.push(`})();`);
  return lines.join("\n");
}

function runFrontend(absPath, fileName, srcRaw) {
  const fe = parseFrontend(fileName, srcRaw);
  const js = emitFrontendJS(fileName, fe);
  const outPath = absPath.replace(/\.one$/i, "") + ".js";
  writeFileText(outPath, js);
  console.log(`ONE v0.1: frontend compiled -> ${outPath}`);
  console.log(`Open the generated .js in a browser (or bundle it).`);
}

// ----------------------------- Formatter (fmt) -----------------------------

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
    case "Tuple":
      return `(${e.items.map(fmtExpr).join(", ")})`;
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
      return (
        indent(depth) +
        `${head} ${s.name}` +
        (s.typeRef ? `: ${s.typeRef.name}` : "") +
        ` = ${fmtExpr(s.value)}`
      );
    }

    case "AssignStmt":
      return indent(depth) + `${s.target.parts.join(".")} = ${fmtExpr(s.value)}`;

    case "ExprStmt":
      return indent(depth) + fmtExpr(s.expr);

    case "ReturnStmt":
      return indent(depth) + (s.expr ? `return ${fmtExpr(s.expr)}` : "return");

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

    case "Block": {
      const body = s.statements.map(st => fmtStmt(st, depth + 1)).join("\n");
      return indent(depth) + "{\n" + body + `\n${indent(depth)}}`;
    }

    default:
      return indent(depth) + "<stmt>";
  }
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
    } else if (item.kind === "TypeDecl") {
      const fields = item.fields.map(f => indent(1) + `${f.name}: ${f.typeRef.name}`).join("\n");
      parts.push(`type ${item.name} {\n${fields}\n}`);
    } else {
      parts.push(fmtStmt(item, 0));
    }
  }
  return parts.filter(Boolean).join("\n\n") + "\n";
}

function fmtFrontendNode(n, depth) {
  if (n.kind === "Text") return indent(depth) + `"${n.value.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
  if (n.kind === "Interp") {
    if (n.expr.kind === "Var") return indent(depth) + `{${n.expr.name}}`;
    if (n.expr.kind === "Literal") return indent(depth) + `{"${String(n.expr.value).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"}`;
    return indent(depth) + `{/* unsupported */}`;
  }
  if (n.kind === "Element") {
    const attrs = (n.attrs || []).map(a => ` ${a.name}="${String(a.value).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`).join("");
    const open = indent(depth) + `<${n.tag}${attrs}>`;
    const close = indent(depth) + `</${n.tag}>`;
    if (!n.children || n.children.length === 0) return open + "\n" + close;
    const kids = n.children.map(c => fmtFrontendNode(c, depth + 1)).join("\n");
    return open + "\n" + kids + "\n" + close;
  }
  return indent(depth) + "<unknown/>";
}

function formatFile(absPath, fileName, srcRaw) {
  const mode = findHeaderMode(srcRaw);
  if (mode === "frontend") {
    const fe = parseFrontend(fileName, srcRaw);

    let logicText = "";
    if (fe.logicRaw != null) {
      const lex = new Lexer(fileName, fe.logicRaw);
      const tokens = lex.tokenizeAll();
      const parser = new Parser(fileName, tokens);
      const program = parser.parseProgram();
      logicText = fmtProgram(program).trimEnd();
    }

    const lines = [];
    lines.push("@frontend");
    lines.push("");

    if (fe.logicRaw != null) {
      lines.push("<logic>");
      lines.push(logicText);
      lines.push("</logic>");
      lines.push("");
    }

    if (fe.styleRaw != null) {
      lines.push("<style>");
      lines.push(fe.styleRaw.replace(/\r\n/g, "\n").replace(/\s+$/g, ""));
      lines.push("</style>");
      lines.push("");
    }

    for (let i = 0; i < fe.nodes.length; i++) {
      lines.push(fmtFrontendNode(fe.nodes[i], 0));
      if (i !== fe.nodes.length - 1) lines.push("");
    }

    writeFileText(absPath, lines.join("\n") + "\n");
    return;
  }

  enforceBackendNoFrontendTags(fileName, srcRaw);
  const src = stripHeaderLine(srcRaw);
  const lex = new Lexer(fileName, src);
  const tokens = lex.tokenizeAll();
  const parser = new Parser(fileName, tokens);
  let program = parser.parseProgram();

  // Format original AST, not optimized AST
  const out = fmtProgram(program);

  const trimmed = srcRaw.trimStart();
  const hasBackendHeader = trimmed.startsWith("@backend");
  const finalOut = (hasBackendHeader ? "@backend\n\n" : "") + out;
  writeFileText(absPath, finalOut);
}

// ----------------------------- CLI -----------------------------

function usage() {
  console.log([
    "ONE v0.1 (Balanced)",
    "Usage:",
    "  node one.js run <file.one>",
    "  node one.js fmt <file.one>",
    "  node one.js <file.one>       (same as run)",
    "",
    "Module system:",
    '  import "./mod.one"           // statement form',
    '  let m = import("./mod.one") // expression form, returns exports object',
    "  export let x = 1",
    "  export fn f() { ... }",
  ].join("\n"));
}

function main() {
  const a = process.argv.slice(2);

  if (a.length === 0 || a[0] === "-h" || a[0] === "--help") {
    usage();
    process.exit(0);
  }

  let cmd = "run";
  let fileArg = null;

  if (a[0] === "run" || a[0] === "fmt") {
    cmd = a[0];
    fileArg = a[1] || null;
  } else {
    cmd = "run";
    fileArg = a[0];
  }

  if (!fileArg) {
    usage();
    process.exit(1);
  }

  const abs = path.resolve(process.cwd(), fileArg);
  const fileName = path.basename(abs);
  const srcRaw = readFileText(abs);
  const mode = findHeaderMode(srcRaw);

  const loader = new ModuleLoader();

  try {
    if (cmd === "fmt") {
      formatFile(abs, fileName, srcRaw);
      console.log(`ONE fmt: formatted ${fileName}`);
      return;
    }

    if (mode === "frontend") {
      runFrontend(abs, fileName, srcRaw);
      return;
    }

    runBackend(abs, fileName, srcRaw, loader);
  } catch (e) {
    console.error(formatError(e, srcRaw));
    process.exit(1);
  }
}

main();

/* ----------------------------- Quick Module Test -----------------------------
Create files:

// math.one
@backend
export let pi = 3.14159
export fn add(a: number, b: number): number {
  return a + b
}

// main.one
@backend
let math = import("./math.one")
print(math.pi)
print(math.add(2, 3))

Run:
  node one.js run main.one

Expected:
  3.14159
  5
----------------------------------------------------------------------------- */
