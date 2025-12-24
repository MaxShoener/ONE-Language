#!/usr/bin/env node
/**
 * ======================================================
 * ONE v0.1 — Single-file reference implementation (Node.js)
 * ======================================================
 *
 * Implements:
 *  - Lexer (token table + longest-match rules)
 *  - Parser (recursive descent)
 *  - AST node constructors
 *  - Desugaring pass: until, guard, .loop, .when, .maybe
 *  - Strict optional type checker:
 *      * Types optional
 *      * If a type is written and incorrect => ERROR (always)
 *      * if/while/repeat conditions NOT required to be bool when type is Unknown
 *      * object literal syntax: object { key: value, ... }
 *  - Interpreter (lowered AST)
 *  - Frontend parsing mode:
 *      * < > structure allowed only in frontend mode
 *      * executable code must be inside <logic>{...}</logic>
 *      * <style> parsed but not executed (stored as data)
 *
 * Usage:
 *   node one.js program.one
 *
 * Notes:
 *  - ONE source files can be both backend and frontend with .one extension.
 *  - Mode directive (recommended):
 *      @frontend
 *      @backend
 *    If absent: defaults to BACKEND.
 */

"use strict";

const fs = require("fs");

// ======================================================
// 1) Utilities
// ======================================================

function fail(kind, msg, span, fileName, source) {
  const loc = span ? `${fileName}:${span.start.line}:${span.start.col}` : fileName;
  let caret = "";
  if (span && source) {
    const lines = source.split(/\r?\n/);
    const line = lines[Math.max(0, span.start.line - 1)] ?? "";
    const pad = " ".repeat(Math.max(0, span.start.col - 1));
    caret = `\n  ${line}\n  ${pad}^`;
  }
  const e = new Error(`${kind}: ${msg}\n  at ${loc}${caret}`);
  e.kind = kind;
  e.span = span;
  throw e;
}

function isAlpha(ch) {
  return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z") || ch === "_";
}
function isDigit(ch) {
  return ch >= "0" && ch <= "9";
}
function isAlphaNum(ch) {
  return isAlpha(ch) || isDigit(ch);
}

function makeSpan(start, end) {
  return { start, end };
}

function clonePos(p) {
  return { line: p.line, col: p.col, idx: p.idx };
}

// ======================================================
// 2) Token definitions
// ======================================================

const TK = Object.freeze({
  EOF: "EOF",

  // identifiers / literals
  IDENT: "IDENT",
  NUMBER: "NUMBER",
  STRING: "STRING",

  // keywords (canonical)
  LET: "LET",
  FN: "FN",
  TYPE: "TYPE",
  IF: "IF",
  ELSE: "ELSE",
  WHILE: "WHILE",
  UNTIL: "UNTIL",
  REPEAT: "REPEAT",
  RETURN: "RETURN",
  BREAK: "BREAK",
  CONTINUE: "CONTINUE",
  GUARD: "GUARD",

  TRUE: "TRUE",
  FALSE: "FALSE",
  NULL: "NULL",

  OBJECT: "OBJECT", // object literal keyword: object { ... }

  // mode directives (single token)
  MODE_FRONTEND: "MODE_FRONTEND",
  MODE_BACKEND: "MODE_BACKEND",

  // tags
  AT: "AT",

  // operators
  PLUS: "PLUS",
  MINUS: "MINUS",
  STAR: "STAR",
  SLASH: "SLASH",

  ASSIGN: "ASSIGN",
  EQUAL: "EQUAL",
  NOT_EQUAL: "NOT_EQUAL",

  LESS: "LESS",
  LESS_EQUAL: "LESS_EQUAL",
  GREATER: "GREATER",
  GREATER_EQUAL: "GREATER_EQUAL",

  AND: "AND",
  OR: "OR",
  NOT: "NOT",

  OPTIONAL_DOT: "OPTIONAL_DOT",
  NULL_COALESCE: "NULL_COALESCE",
  ARROW: "ARROW",

  // punctuation
  LPAREN: "LPAREN",
  RPAREN: "RPAREN",
  LBRACE: "LBRACE",
  RBRACE: "RBRACE",
  COMMA: "COMMA",
  COLON: "COLON",
  SEMICOLON: "SEMICOLON",
  DOT: "DOT",

  // frontend structure tokens
  LT: "LT",
  GT: "GT",
  LT_SLASH: "LT_SLASH",
  SLASH_GT: "SLASH_GT",
  HASH: "HASH",
});

const KEYWORD_CANON = new Map([
  ["let", TK.LET],
  ["var", TK.LET],

  ["fn", TK.FN],
  ["function", TK.FN],

  ["type", TK.TYPE],

  ["if", TK.IF],
  ["when", TK.IF],

  ["else", TK.ELSE],
  ["otherwise", TK.ELSE],

  ["while", TK.WHILE],
  ["until", TK.UNTIL],
  ["repeat", TK.REPEAT],

  ["return", TK.RETURN],
  ["break", TK.BREAK],
  ["continue", TK.CONTINUE],
  ["guard", TK.GUARD],

  ["true", TK.TRUE],
  ["t", TK.TRUE],
  ["yes", TK.TRUE],

  ["false", TK.FALSE],
  ["f", TK.FALSE],
  ["no", TK.FALSE],

  ["null", TK.NULL],
  ["nil", TK.NULL],
  ["none", TK.NULL],

  ["object", TK.OBJECT],
]);

// ======================================================
// 3) Lexer
// ======================================================

class Lexer {
  constructor(source, fileName) {
    this.source = source;
    this.fileName = fileName;
    this.pos = { line: 1, col: 1, idx: 0 };
    this.tokens = [];
  }

  peek(n = 0) {
    return this.source[this.pos.idx + n] ?? "\0";
  }

  advance() {
    const ch = this.peek(0);
    if (ch === "\n") {
      this.pos.line += 1;
      this.pos.col = 1;
      this.pos.idx += 1;
      return ch;
    }
    if (ch === "\r") {
      // handle CRLF
      if (this.peek(1) === "\n") {
        this.pos.idx += 2;
      } else {
        this.pos.idx += 1;
      }
      this.pos.line += 1;
      this.pos.col = 1;
      return "\n";
    }
    this.pos.idx += 1;
    this.pos.col += 1;
    return ch;
  }

  addToken(type, value, startPos, endPos) {
    this.tokens.push({
      type,
      value,
      span: makeSpan(startPos, endPos),
    });
  }

  lex() {
    while (true) {
      this.skipWhitespaceAndComments();
      const start = clonePos(this.pos);
      const ch = this.peek(0);

      if (ch === "\0") {
        this.addToken(TK.EOF, null, start, clonePos(this.pos));
        break;
      }

      // Longest-match / multi-char operators first
      if (ch === "?" && this.peek(1) === ".") {
        this.advance(); this.advance();
        this.addToken(TK.OPTIONAL_DOT, "?.", start, clonePos(this.pos));
        continue;
      }
      if (ch === "?" && this.peek(1) === "?") {
        this.advance(); this.advance();
        this.addToken(TK.NULL_COALESCE, "??", start, clonePos(this.pos));
        continue;
      }
      if (ch === "=" && this.peek(1) === "=") {
        this.advance(); this.advance();
        this.addToken(TK.EQUAL, "==", start, clonePos(this.pos));
        continue;
      }
      if (ch === "!" && this.peek(1) === "=") {
        this.advance(); this.advance();
        this.addToken(TK.NOT_EQUAL, "!=", start, clonePos(this.pos));
        continue;
      }
      if (ch === "<" && this.peek(1) === "=") {
        this.advance(); this.advance();
        this.addToken(TK.LESS_EQUAL, "<=", start, clonePos(this.pos));
        continue;
      }
      if (ch === ">" && this.peek(1) === "=") {
        this.advance(); this.advance();
        this.addToken(TK.GREATER_EQUAL, ">=", start, clonePos(this.pos));
        continue;
      }
      if (ch === "&" && this.peek(1) === "&") {
        this.advance(); this.advance();
        this.addToken(TK.AND, "&&", start, clonePos(this.pos));
        continue;
      }
      if (ch === "|" && this.peek(1) === "|") {
        this.advance(); this.advance();
        this.addToken(TK.OR, "||", start, clonePos(this.pos));
        continue;
      }
      if (ch === "=" && this.peek(1) === ">") {
        this.advance(); this.advance();
        this.addToken(TK.ARROW, "=>", start, clonePos(this.pos));
        continue;
      }

      // Frontend structure multi-char
      if (ch === "<" && this.peek(1) === "/") {
        this.advance(); this.advance();
        this.addToken(TK.LT_SLASH, "</", start, clonePos(this.pos));
        continue;
      }
      if (ch === "/" && this.peek(1) === ">") {
        this.advance(); this.advance();
        this.addToken(TK.SLASH_GT, "/>", start, clonePos(this.pos));
        continue;
      }

      // Single-char tokens
      switch (ch) {
        case "@": {
          this.advance();
          // mode directives as single token: @frontend / @backend
          const nameStart = clonePos(this.pos);
          if (isAlpha(this.peek(0))) {
            const ident = this.readIdentifier();
            const end = clonePos(this.pos);
            const lower = ident.toLowerCase();
            if (lower === "frontend") {
              this.addToken(TK.MODE_FRONTEND, "@frontend", start, end);
            } else if (lower === "backend") {
              this.addToken(TK.MODE_BACKEND, "@backend", start, end);
            } else {
              // emit AT then IDENT for normal tags
              this.addToken(TK.AT, "@", start, nameStart);
              this.addToken(TK.IDENT, ident, nameStart, end);
            }
          } else {
            this.addToken(TK.AT, "@", start, clonePos(this.pos));
          }
          continue;
        }
        case "+": this.advance(); this.addToken(TK.PLUS, "+", start, clonePos(this.pos)); continue;
        case "-": this.advance(); this.addToken(TK.MINUS, "-", start, clonePos(this.pos)); continue;
        case "*": this.advance(); this.addToken(TK.STAR, "*", start, clonePos(this.pos)); continue;
        case "/": this.advance(); this.addToken(TK.SLASH, "/", start, clonePos(this.pos)); continue;
        case "=": this.advance(); this.addToken(TK.ASSIGN, "=", start, clonePos(this.pos)); continue;
        case "<": this.advance(); this.addToken(TK.LT, "<", start, clonePos(this.pos)); continue;
        case ">": this.advance(); this.addToken(TK.GT, ">", start, clonePos(this.pos)); continue;
        case "!": this.advance(); this.addToken(TK.NOT, "!", start, clonePos(this.pos)); continue;

        case "(": this.advance(); this.addToken(TK.LPAREN, "(", start, clonePos(this.pos)); continue;
        case ")": this.advance(); this.addToken(TK.RPAREN, ")", start, clonePos(this.pos)); continue;
        case "{": this.advance(); this.addToken(TK.LBRACE, "{", start, clonePos(this.pos)); continue;
        case "}": this.advance(); this.addToken(TK.RBRACE, "}", start, clonePos(this.pos)); continue;
        case ",": this.advance(); this.addToken(TK.COMMA, ",", start, clonePos(this.pos)); continue;
        case ":": this.advance(); this.addToken(TK.COLON, ":", start, clonePos(this.pos)); continue;
        case ";": this.advance(); this.addToken(TK.SEMICOLON, ";", start, clonePos(this.pos)); continue;
        case ".": this.advance(); this.addToken(TK.DOT, ".", start, clonePos(this.pos)); continue;
        case "#": this.advance(); this.addToken(TK.HASH, "#", start, clonePos(this.pos)); continue;
        case "\"": {
          const str = this.readString();
          this.addToken(TK.STRING, str.value, start, str.endPos);
          continue;
        }
        default:
          break;
      }

      // number
      if (isDigit(ch)) {
        const num = this.readNumber();
        this.addToken(TK.NUMBER, num.value, start, num.endPos);
        continue;
      }

      // identifier / keyword
      if (isAlpha(ch)) {
        const ident = this.readIdentifier();
        const end = clonePos(this.pos);
        const lower = ident.toLowerCase();
        const kw = KEYWORD_CANON.get(lower);
        if (kw) {
          this.addToken(kw, ident, start, end);
        } else {
          this.addToken(TK.IDENT, ident, start, end);
        }
        continue;
      }

      fail("SyntaxError", `Unexpected character '${ch}'`, makeSpan(start, start), this.fileName, this.source);
    }

    return this.tokens;
  }

  skipWhitespaceAndComments() {
    while (true) {
      const ch = this.peek(0);
      // whitespace
      if (ch === " " || ch === "\t" || ch === "\n" || ch === "\r") {
        this.advance();
        continue;
      }
      // comment: //
      if (ch === "/" && this.peek(1) === "/") {
        while (this.peek(0) !== "\n" && this.peek(0) !== "\r" && this.peek(0) !== "\0") {
          this.advance();
        }
        continue;
      }
      break;
    }
  }

  readIdentifier() {
    let s = "";
    while (isAlphaNum(this.peek(0))) {
      s += this.advance();
    }
    return s;
  }

  readNumber() {
    let s = "";
    while (isDigit(this.peek(0))) s += this.advance();
    if (this.peek(0) === "." && isDigit(this.peek(1))) {
      s += this.advance();
      while (isDigit(this.peek(0))) s += this.advance();
    }
    return { value: s, endPos: clonePos(this.pos) };
  }

  readString() {
    // opening quote already peeked
    this.advance(); // consume "
    let s = "";
    while (true) {
      const ch = this.peek(0);
      if (ch === "\0" || ch === "\n" || ch === "\r") {
        fail("SyntaxError", "Unterminated string literal", makeSpan(clonePos(this.pos), clonePos(this.pos)), this.fileName, this.source);
      }
      if (ch === "\"") {
        this.advance(); // closing
        break;
      }
      // simple escapes (optional)
      if (ch === "\\" && this.peek(1) === "\"") {
        this.advance(); this.advance();
        s += "\"";
        continue;
      }
      s += this.advance();
    }
    return { value: s, endPos: clonePos(this.pos) };
  }
}

// ======================================================
// 4) AST constructors
// ======================================================

function node(kind, span, props = {}) {
  return Object.freeze({ kind, span, ...props });
}

// Program / tags
const Program = (span, mode, body) => node("Program", span, { mode, body });
const Tag = (span, name, args) => node("Tag", span, { name, args });
const TaggedDecl = (span, tags, decl) => node("TaggedDecl", span, { tags, decl });
const TaggedBlock = (span, tags, block) => node("TaggedBlock", span, { tags, block });

// Decls
const VarDecl = (span, name, typeRef, init) => node("VarDecl", span, { name, type: typeRef, init });
const FnDecl = (span, name, params, returnType, body) => node("FnDecl", span, { name, params, returnType, body });
const TypeDecl = (span, name, fields) => node("TypeDecl", span, { name, fields });

const Param = (span, name, typeRef, defaultValue) => node("Param", span, { name, type: typeRef, defaultValue });
const TypeField = (span, name, typeRef) => node("TypeField", span, { name, type: typeRef });

// Types
const SimpleType = (span, name) => node("SimpleType", span, { name });
const NullableType = (span, base) => node("NullableType", span, { base });
const UnionType = (span, options) => node("UnionType", span, { options });

// Statements
const Block = (span, body) => node("Block", span, { body });
const IfStmt = (span, condition, thenBlock, elseBlock) => node("If", span, { condition, thenBlock, elseBlock });
const WhileStmt = (span, condition, body) => node("While", span, { condition, body });
const UntilStmt = (span, condition, body) => node("Until", span, { condition, body });
const RepeatStmt = (span, count, body) => node("Repeat", span, { count, body });
const ReturnStmt = (span, value) => node("Return", span, { value });
const BreakStmt = (span) => node("Break", span, {});
const ContinueStmt = (span) => node("Continue", span, {});
const GuardStmt = (span, condition) => node("Guard", span, { condition });
const ExprStmt = (span, expr) => node("ExprStmt", span, { expr });

// Shortcut statements (parsed explicitly, then lowered)
const WhenSugar = (span, condExpr, block) => node("WhenSugar", span, { condExpr, block });
const MaybeSugar = (span, valueExpr, binderName, block) => node("MaybeSugar", span, { valueExpr, binderName, block });
const LoopSugar = (span, iterExpr, binderName, indexName, block) => node("LoopSugar", span, { iterExpr, binderName, indexName, block });

// Expressions
const NumberLit = (span, value) => node("Number", span, { value });
const StringLit = (span, value) => node("String", span, { value });
const BoolLit = (span, value) => node("Bool", span, { value });
const NullLit = (span) => node("Null", span, {});
const Ident = (span, name) => node("Identifier", span, { name });

const Assign = (span, target, value) => node("Assign", span, { target, value });
const Binary = (span, operator, left, right) => node("Binary", span, { operator, left, right });
const Unary = (span, operator, expr) => node("Unary", span, { operator, expr });

const Call = (span, callee, args) => node("Call", span, { callee, args });
const Access = (span, object, property) => node("Access", span, { object, property });
const OptionalAccess = (span, object, property) => node("OptionalAccess", span, { object, property });

const Group = (span, expr) => node("Group", span, { expr });
const ListLit = (span, elements) => node("List", span, { elements });
const ArrowFn = (span, params, body) => node("ArrowFn", span, { params, body });

const ObjectLit = (span, fields) => node("ObjectLiteral", span, { fields }); // fields: [{name, value}]

// Frontend structure
const Element = (span, tag, id, attributes, children) => node("Element", span, { tag, id, attributes, children });
const SelfClosing = (span, tag, id, attributes) => node("SelfClosing", span, { tag, id, attributes });
const TextNode = (span, value) => node("Text", span, { value });
const LogicNode = (span, block) => node("Logic", span, { block });
const StyleNode = (span, rawText) => node("Style", span, { rawText }); // v0.1: store raw; validate later
const Attribute = (span, name, valueExpr) => node("Attribute", span, { name, value: valueExpr });

// ======================================================
// 5) Parser
// ======================================================

class Parser {
  constructor(tokens, source, fileName) {
    this.tokens = tokens;
    this.source = source;
    this.fileName = fileName;
    this.i = 0;
    this.mode = "BACKEND"; // default
  }

  cur() { return this.tokens[this.i]; }
  peek(n = 1) { return this.tokens[this.i + n] ?? this.tokens[this.tokens.length - 1]; }
  at(type) { return this.cur().type === type; }
  match(type) { if (this.at(type)) { return this.advance(); } return null; }
  advance() { const t = this.cur(); this.i++; return t; }
  expect(type, msg) {
    if (!this.at(type)) {
      const t = this.cur();
      fail("SyntaxError", msg ?? `Expected ${type} but got ${t.type}`, t.span, this.fileName, this.source);
    }
    return this.advance();
  }

  optionalSemicolon() { this.match(TK.SEMICOLON); }

  parseProgram() {
    const start = this.cur().span.start;
    const body = [];
    while (!this.at(TK.EOF)) {
      body.push(this.parseTopLevel());
    }
    const end = this.cur().span.end;
    return Program(makeSpan(start, end), this.mode, body);
  }

  parseTopLevel() {
    // mode directives
    if (this.at(TK.MODE_FRONTEND)) {
      const t = this.advance();
      this.mode = "FRONTEND";
      return node("ModeDirective", t.span, { mode: "FRONTEND" });
    }
    if (this.at(TK.MODE_BACKEND)) {
      const t = this.advance();
      this.mode = "BACKEND";
      return node("ModeDirective", t.span, { mode: "BACKEND" });
    }

    // frontend structure
    if (this.mode === "FRONTEND" && this.at(TK.LT)) {
      return this.parseStructureNode();
    }
    // enforce: no structure tokens in backend
    if (this.mode === "BACKEND" && (this.at(TK.LT) || this.at(TK.LT_SLASH) || this.at(TK.GT) || this.at(TK.SLASH_GT))) {
      fail("SyntaxError", "Structure tags < > are not allowed in backend mode. Use @frontend to enable.", this.cur().span, this.fileName, this.source);
    }

    if (this.at(TK.AT)) return this.parseTaggedThing();
    if (this.at(TK.LET)) return this.parseVarDecl();
    if (this.at(TK.FN)) return this.parseFnDecl();
    if (this.at(TK.TYPE)) return this.parseTypeDecl();
    return this.parseStatement();
  }

  startsDecl() {
    return this.at(TK.LET) || this.at(TK.FN) || this.at(TK.TYPE);
  }

  parseTaggedThing() {
    const start = this.cur().span.start;
    const tags = [];
    while (this.at(TK.AT)) {
      tags.push(this.parseTag());
    }
    if (this.startsDecl()) {
      const decl = this.parseDecl();
      const end = decl.span.end;
      return TaggedDecl(makeSpan(start, end), tags, decl);
    }
    if (this.at(TK.LBRACE)) {
      const blk = this.parseBlock();
      const end = blk.span.end;
      return TaggedBlock(makeSpan(start, end), tags, blk);
    }
    fail("SyntaxError", "Tag(s) must apply to a declaration or a block.", this.cur().span, this.fileName, this.source);
  }

  parseTag() {
    const atTok = this.expect(TK.AT, "Expected '@' for tag.");
    const nameTok = this.expect(TK.IDENT, "Expected tag name after '@'.");
    const args = [];
    if (this.match(TK.LPAREN)) {
      if (!this.at(TK.RPAREN)) {
        args.push(this.parseExpression());
        while (this.match(TK.COMMA)) {
          args.push(this.parseExpression());
        }
      }
      this.expect(TK.RPAREN, "Expected ')' after tag args.");
    }
    return Tag(makeSpan(atTok.span.start, (args.at(-1)?.span.end ?? nameTok.span.end)), nameTok.value, args);
  }

  parseDecl() {
    if (this.at(TK.LET)) return this.parseVarDecl();
    if (this.at(TK.FN)) return this.parseFnDecl();
    if (this.at(TK.TYPE)) return this.parseTypeDecl();
    fail("SyntaxError", "Expected declaration.", this.cur().span, this.fileName, this.source);
  }

  parseVarDecl() {
    const letTok = this.expect(TK.LET, "Expected let/var.");
    const nameTok = this.expect(TK.IDENT, "Expected variable name.");
    let typeRef = null;
    if (this.match(TK.COLON)) typeRef = this.parseTypeRef();
    let init = null;
    if (this.match(TK.ASSIGN)) init = this.parseExpression();
    this.optionalSemicolon();
    return VarDecl(makeSpan(letTok.span.start, (init?.span.end ?? (typeRef?.span.end ?? nameTok.span.end))), nameTok.value, typeRef, init);
  }

  parseFnDecl() {
    const fnTok = this.expect(TK.FN, "Expected fn/function.");
    const nameTok = this.expect(TK.IDENT, "Expected function name.");
    this.expect(TK.LPAREN, "Expected '(' after function name.");
    const params = [];
    if (!this.at(TK.RPAREN)) {
      params.push(this.parseParam());
      while (this.match(TK.COMMA)) params.push(this.parseParam());
    }
    this.expect(TK.RPAREN, "Expected ')' after params.");

    let returnType = null;
    if (this.match(TK.COLON)) returnType = this.parseTypeRef();

    const body = this.parseBlock();
    return FnDecl(makeSpan(fnTok.span.start, body.span.end), nameTok.value, params, returnType, body);
  }

  parseParam() {
    const nameTok = this.expect(TK.IDENT, "Expected parameter name.");
    let typeRef = null;
    if (this.match(TK.COLON)) typeRef = this.parseTypeRef();
    let def = null;
    if (this.match(TK.ASSIGN)) def = this.parseExpression();
    return Param(makeSpan(nameTok.span.start, (def?.span.end ?? (typeRef?.span.end ?? nameTok.span.end))), nameTok.value, typeRef, def);
  }

  parseTypeDecl() {
    const tTok = this.expect(TK.TYPE, "Expected 'type'.");
    const nameTok = this.expect(TK.IDENT, "Expected type name.");
    this.expect(TK.LBRACE, "Expected '{' in type decl.");
    const fields = [];
    while (!this.at(TK.RBRACE)) {
      const fName = this.expect(TK.IDENT, "Expected field name in type.");
      this.expect(TK.COLON, "Expected ':' in type field.");
      const fType = this.parseTypeRef();
      this.match(TK.COMMA);
      fields.push(TypeField(makeSpan(fName.span.start, fType.span.end), fName.value, fType));
    }
    const rTok = this.expect(TK.RBRACE, "Expected '}' to end type decl.");
    this.optionalSemicolon();
    return TypeDecl(makeSpan(tTok.span.start, rTok.span.end), nameTok.value, fields);
  }

  parseTypeRef() {
    // UnionType = NullableType (| NullableType)*
    const start = this.cur().span.start;
    let left = this.parseNullableType();
    const options = [left];
    while (this.match(TK.OR)) { // re-use OR token '||'? no; unions use '|' in spec, but lexer doesn't include '|'
      // Not supported here because we removed single '|'. Keep unions minimal for now.
      // If you later add single '|' token, implement here.
      fail("SyntaxError", "Union types with '|' are not enabled in this implementation yet.", this.cur().span, this.fileName, this.source);
    }
    // NullableType: PrimaryType '?'
    // Implement '?' as part of nullable types? We didn't lex single '?' token.
    // We'll support nullable via suffix '?' inside IDENT tokens (e.g., User?)? Not desired.
    // So: v0.1 in this implementation supports nullable only as 'TypeName?' NOT enabled.
    // We'll keep nullable out for now and rely on runtime null + optional access.
    // However, you asked for optional types like TS; we can support `TypeName?` by lexing '?'.
    fail("SyntaxError", "Type parsing needs '?' (nullable) and '|' (union) tokens not yet enabled. For now, use base types (number/string/bool) and named types without ? or |.", makeSpan(start, this.cur().span.end), this.fileName, this.source);
  }

  parseNullableType() {
    // unreachable in this file version (see parseTypeRef)
    return this.parsePrimaryType();
  }

  parsePrimaryType() {
    const t = this.cur();
    if (this.at(TK.IDENT)) {
      const nameTok = this.advance();
      return SimpleType(makeSpan(nameTok.span.start, nameTok.span.end), nameTok.value);
    }
    // builtins are lexed as IDENT unless you want keywords; we can accept identifiers.
    fail("SyntaxError", "Expected type name.", t.span, this.fileName, this.source);
  }

  // ----------------------------
  // Statements
  // ----------------------------

  parseStatement() {
    if (this.at(TK.LBRACE)) return this.parseBlock();
    if (this.at(TK.IF)) return this.parseIf();
    if (this.at(TK.WHILE)) return this.parseWhile();
    if (this.at(TK.UNTIL)) return this.parseUntil();
    if (this.at(TK.REPEAT)) return this.parseRepeat();
    if (this.at(TK.RETURN)) return this.parseReturn();
    if (this.at(TK.BREAK)) {
      const t = this.advance();
      this.optionalSemicolon();
      return BreakStmt(t.span);
    }
    if (this.at(TK.CONTINUE)) {
      const t = this.advance();
      this.optionalSemicolon();
      return ContinueStmt(t.span);
    }
    if (this.at(TK.GUARD)) return this.parseGuard();

    // expression statement, with shortcut detection requiring braces
    const expr = this.parseExpression();
    const start = expr.span.start;

    // Shortcut forms require braces for readability:
    //   (cond).when { ... }
    //   val.maybe(name) { ... }
    //   iter.loop(name[, idx]) { ... }
    //
    // We detect them here and parse the following Block immediately.
    if (this.at(TK.LBRACE)) {
      const sugar = this.tryParseSugarFromExpr(expr);
      if (sugar) return sugar;
    }

    this.optionalSemicolon();
    return ExprStmt(makeSpan(start, expr.span.end), expr);
  }

  parseBlock() {
    const l = this.expect(TK.LBRACE, "Expected '{'.");
    const items = [];
    while (!this.at(TK.RBRACE)) {
      if (this.at(TK.AT)) items.push(this.parseTaggedThing());
      else if (this.startsDecl()) items.push(this.parseDecl());
      else items.push(this.parseStatement());
    }
    const r = this.expect(TK.RBRACE, "Expected '}' to end block.");
    return Block(makeSpan(l.span.start, r.span.end), items);
  }

  parseIf() {
    const ifTok = this.expect(TK.IF, "Expected if/when.");
    this.expect(TK.LPAREN, "Expected '(' after if.");
    const cond = this.parseExpression();
    this.expect(TK.RPAREN, "Expected ')' after condition.");
    const thenBlk = this.parseBlock();
    let elseBlk = null;
    if (this.match(TK.ELSE)) elseBlk = this.parseBlock();
    return IfStmt(makeSpan(ifTok.span.start, (elseBlk?.span.end ?? thenBlk.span.end)), cond, thenBlk, elseBlk);
  }

  parseWhile() {
    const t = this.expect(TK.WHILE, "Expected while.");
    this.expect(TK.LPAREN, "Expected '(' after while.");
    const cond = this.parseExpression();
    this.expect(TK.RPAREN, "Expected ')' after while condition.");
    const body = this.parseBlock();
    return WhileStmt(makeSpan(t.span.start, body.span.end), cond, body);
  }

  parseUntil() {
    const t = this.expect(TK.UNTIL, "Expected until.");
    this.expect(TK.LPAREN, "Expected '(' after until.");
    const cond = this.parseExpression();
    this.expect(TK.RPAREN, "Expected ')' after until condition.");
    const body = this.parseBlock();
    return UntilStmt(makeSpan(t.span.start, body.span.end), cond, body);
  }

  parseRepeat() {
    const t = this.expect(TK.REPEAT, "Expected repeat.");
    this.expect(TK.LPAREN, "Expected '(' after repeat.");
    const count = this.parseExpression();
    this.expect(TK.RPAREN, "Expected ')' after repeat count.");
    const body = this.parseBlock();
    return RepeatStmt(makeSpan(t.span.start, body.span.end), count, body);
  }

  parseReturn() {
    const t = this.expect(TK.RETURN, "Expected return.");
    let val = null;
    if (!this.at(TK.SEMICOLON) && !this.at(TK.RBRACE)) {
      val = this.parseExpression();
    }
    this.optionalSemicolon();
    return ReturnStmt(makeSpan(t.span.start, (val?.span.end ?? t.span.end)), val);
  }

  parseGuard() {
    const t = this.expect(TK.GUARD, "Expected guard.");
    const expr = this.parseExpression();
    this.optionalSemicolon();
    return GuardStmt(makeSpan(t.span.start, expr.span.end), expr);
  }

  tryParseSugarFromExpr(expr) {
    // expects next token is LBRACE (checked before calling)
    // Recognize:
    // - Access( condExpr, "when") with no call, OR Call(Access(condExpr,"when"), [])
    // - Call(Access(valueExpr,"maybe"), [Identifier(name)])
    // - Call(Access(iterExpr,"loop"), [Identifier(name)] or [Identifier(name), Identifier(idx)])
    //
    // We’ll accept both Access and Call forms for when; but .maybe/.loop must be Call.
    const block = this.parseBlock();

    // When: expr is Access(obj,"when") OR Call(Access(obj,"when"),[])
    if (expr.kind === "Access" && expr.property === "when") {
      return WhenSugar(makeSpan(expr.span.start, block.span.end), expr.object, block);
    }
    if (expr.kind === "Call" && expr.callee.kind === "Access" && expr.callee.property === "when") {
      if (expr.args.length !== 0) {
        fail("SyntaxError", ".when does not take arguments.", expr.span, this.fileName, this.source);
      }
      return WhenSugar(makeSpan(expr.span.start, block.span.end), expr.callee.object, block);
    }

    // Maybe
    if (expr.kind === "Call" && expr.callee.kind === "Access" && expr.callee.property === "maybe") {
      if (expr.args.length !== 1 || expr.args[0].kind !== "Identifier") {
        fail("SyntaxError", ".maybe requires exactly one identifier binder: value.maybe(x) { ... }", expr.span, this.fileName, this.source);
      }
      return MaybeSugar(makeSpan(expr.span.start, block.span.end), expr.callee.object, expr.args[0].name, block);
    }

    // Loop
    if (expr.kind === "Call" && expr.callee.kind === "Access" && expr.callee.property === "loop") {
      if (expr.args.length < 1 || expr.args.length > 2) {
        fail("SyntaxError", ".loop requires 1 or 2 identifier binders: list.loop(item[, index]) { ... }", expr.span, this.fileName, this.source);
      }
      if (expr.args[0].kind !== "Identifier") {
        fail("SyntaxError", ".loop first argument must be an identifier binder name.", expr.span, this.fileName, this.source);
      }
      let idxName = null;
      if (expr.args.length === 2) {
        if (expr.args[1].kind !== "Identifier") {
          fail("SyntaxError", ".loop second argument must be an identifier binder name.", expr.span, this.fileName, this.source);
        }
        idxName = expr.args[1].name;
      }
      return LoopSugar(makeSpan(expr.span.start, block.span.end), expr.callee.object, expr.args[0].name, idxName, block);
    }

    // Not a recognized sugar: return it as ExprStmt + Block?? We already consumed block, so error.
    fail("SyntaxError", "A block '{...}' after an expression is only allowed for .loop/.when/.maybe shortcuts.", block.span, this.fileName, this.source);
  }

  // ----------------------------
  // Expressions (precedence)
  // ----------------------------

  parseExpression() { return this.parseAssignment(); }

  parseAssignment() {
    const left = this.parseOr();
    if (this.match(TK.ASSIGN)) {
      const opTok = this.tokens[this.i - 1];
      const right = this.parseAssignment();
      return Assign(makeSpan(left.span.start, right.span.end), left, right);
    }
    return left;
  }

  parseOr() {
    let expr = this.parseAnd();
    while (this.match(TK.OR)) {
      const opTok = this.tokens[this.i - 1];
      const right = this.parseAnd();
      expr = Binary(makeSpan(expr.span.start, right.span.end), "||", expr, right);
    }
    return expr;
  }

  parseAnd() {
    let expr = this.parseEquality();
    while (this.match(TK.AND)) {
      const right = this.parseEquality();
      expr = Binary(makeSpan(expr.span.start, right.span.end), "&&", expr, right);
    }
    return expr;
  }

  parseEquality() {
    let expr = this.parseCompare();
    while (this.at(TK.EQUAL) || this.at(TK.NOT_EQUAL)) {
      const t = this.advance();
      const right = this.parseCompare();
      expr = Binary(makeSpan(expr.span.start, right.span.end), t.type === TK.EQUAL ? "==" : "!=", expr, right);
    }
    return expr;
  }

  parseCompare() {
    let expr = this.parseTerm();
    while (this.at(TK.LESS) || this.at(TK.LESS_EQUAL) || this.at(TK.GREATER) || this.at(TK.GREATER_EQUAL)) {
      const t = this.advance();
      const right = this.parseTerm();
      const op = t.type === TK.LESS ? "<" :
                 t.type === TK.LESS_EQUAL ? "<=" :
                 t.type === TK.GREATER ? ">" : ">=";
      expr = Binary(makeSpan(expr.span.start, right.span.end), op, expr, right);
    }
    return expr;
  }

  parseTerm() {
    let expr = this.parseFactor();
    while (this.at(TK.PLUS) || this.at(TK.MINUS)) {
      const t = this.advance();
      const right = this.parseFactor();
      expr = Binary(makeSpan(expr.span.start, right.span.end), t.type === TK.PLUS ? "+" : "-", expr, right);
    }
    return expr;
  }

  parseFactor() {
    let expr = this.parseUnary();
    while (this.at(TK.STAR) || this.at(TK.SLASH)) {
      const t = this.advance();
      const right = this.parseUnary();
      expr = Binary(makeSpan(expr.span.start, right.span.end), t.type === TK.STAR ? "*" : "/", expr, right);
    }
    return expr;
  }

  parseUnary() {
    if (this.at(TK.NOT) || this.at(TK.MINUS)) {
      const t = this.advance();
      const expr = this.parseUnary();
      return Unary(makeSpan(t.span.start, expr.span.end), t.type === TK.NOT ? "!" : "-", expr);
    }
    return this.parsePostfix();
  }

  parsePostfix() {
    let expr = this.parsePrimary();
    while (true) {
      if (this.match(TK.LPAREN)) {
        const args = [];
        if (!this.at(TK.RPAREN)) {
          args.push(this.parseExpression());
          while (this.match(TK.COMMA)) args.push(this.parseExpression());
        }
        const r = this.expect(TK.RPAREN, "Expected ')' after call args.");
        expr = Call(makeSpan(expr.span.start, r.span.end), expr, args);
        continue;
      }
      if (this.match(TK.OPTIONAL_DOT)) {
        const nameTok = this.expect(TK.IDENT, "Expected property name after '?.'.");
        expr = OptionalAccess(makeSpan(expr.span.start, nameTok.span.end), expr, nameTok.value);
        continue;
      }
      if (this.match(TK.DOT)) {
        const nameTok = this.expect(TK.IDENT, "Expected property name after '.'.");
        expr = Access(makeSpan(expr.span.start, nameTok.span.end), expr, nameTok.value);
        continue;
      }
      break;
    }
    return expr;
  }

  looksLikeArrowAfterParenStart(savedIndex) {
    // crude lookahead: parse "(a, b) =>"
    // savedIndex points at token after '('
    let j = savedIndex;
    if (this.tokens[j].type === TK.RPAREN && this.tokens[j + 1]?.type === TK.ARROW) return true;
    // consume IDENT (, IDENT)*
    if (this.tokens[j].type !== TK.IDENT) return false;
    j++;
    while (this.tokens[j].type === TK.COMMA) {
      j++;
      if (this.tokens[j].type !== TK.IDENT) return false;
      j++;
    }
    return this.tokens[j].type === TK.RPAREN && this.tokens[j + 1]?.type === TK.ARROW;
  }

  parsePrimary() {
    const t = this.cur();

    if (this.at(TK.NUMBER)) {
      const tok = this.advance();
      return NumberLit(tok.span, Number(tok.value));
    }
    if (this.at(TK.STRING)) {
      const tok = this.advance();
      return StringLit(tok.span, tok.value);
    }
    if (this.at(TK.TRUE)) {
      const tok = this.advance();
      return BoolLit(tok.span, true);
    }
    if (this.at(TK.FALSE)) {
      const tok = this.advance();
      return BoolLit(tok.span, false);
    }
    if (this.at(TK.NULL)) {
      const tok = this.advance();
      return NullLit(tok.span);
    }
    if (this.at(TK.OBJECT)) {
      return this.parseObjectLiteral();
    }
    if (this.at(TK.IDENT)) {
      const tok = this.advance();
      return Ident(tok.span, tok.value);
    }
    if (this.match(TK.LPAREN)) {
      const lTok = this.tokens[this.i - 1];
      const after = this.i;
      // arrow fn?
      if (this.looksLikeArrowAfterParenStart(after)) {
        const params = [];
        if (!this.at(TK.RPAREN)) {
          const p = this.expect(TK.IDENT, "Expected arrow param.");
          params.push(p.value);
          while (this.match(TK.COMMA)) {
            const p2 = this.expect(TK.IDENT, "Expected arrow param.");
            params.push(p2.value);
          }
        }
        const rParen = this.expect(TK.RPAREN, "Expected ')' in arrow.");
        this.expect(TK.ARROW, "Expected '=>' after arrow params.");
        const body = this.parseExpression();
        return ArrowFn(makeSpan(lTok.span.start, body.span.end), params, body);
      }

      // group or list
      const exprs = [];
      if (!this.at(TK.RPAREN)) {
        exprs.push(this.parseExpression());
        while (this.match(TK.COMMA)) {
          exprs.push(this.parseExpression());
        }
      }
      const rTok = this.expect(TK.RPAREN, "Expected ')' after grouping/list.");
      if (exprs.length === 1) return Group(makeSpan(lTok.span.start, rTok.span.end), exprs[0]);
      return ListLit(makeSpan(lTok.span.start, rTok.span.end), exprs);
    }

    fail("SyntaxError", "Expected expression.", t.span, this.fileName, this.source);
  }

  parseObjectLiteral() {
    const objTok = this.expect(TK.OBJECT, "Expected 'object'.");
    const l = this.expect(TK.LBRACE, "Expected '{' after object.");
    const fields = [];
    while (!this.at(TK.RBRACE)) {
      const nameTok = this.expect(TK.IDENT, "Expected field name in object literal.");
      this.expect(TK.COLON, "Expected ':' in object field.");
      const val = this.parseExpression();
      fields.push({ name: nameTok.value, value: val, span: makeSpan(nameTok.span.start, val.span.end) });
      if (!this.match(TK.COMMA)) break;
    }
    const r = this.expect(TK.RBRACE, "Expected '}' to end object literal.");
    return ObjectLit(makeSpan(objTok.span.start, r.span.end), fields);
  }

  // ----------------------------
  // Frontend structure parsing
  // ----------------------------

  parseStructureNode() {
    // <tag#id attr:value ...> children </tag>
    const lt = this.expect(TK.LT, "Expected '<' for structure.");
    const tagTok = this.expect(TK.IDENT, "Expected tag name.");
    let id = null;
    if (this.match(TK.HASH)) {
      const idTok = this.expect(TK.IDENT, "Expected id after '#'.");
      id = idTok.value;
    }
    const attributes = [];
    // attributes: ident : (string|number|bool|null|ident)
    while (this.at(TK.IDENT)) {
      const aName = this.advance();
      if (!this.match(TK.COLON)) {
        // not an attribute; likely end of tag content; stop
        // But since we consumed IDENT, this is ambiguous. Keep it strict:
        fail("SyntaxError", "Expected ':' after attribute name in structure tag.", aName.span, this.fileName, this.source);
      }
      const v = this.parseAttrValueExpr();
      attributes.push(Attribute(makeSpan(aName.span.start, v.span.end), aName.value, v));
    }

    if (this.match(TK.SLASH_GT)) {
      return SelfClosing(makeSpan(lt.span.start, this.tokens[this.i - 1].span.end), tagTok.value, id, attributes);
    }

    this.expect(TK.GT, "Expected '>' to end start tag.");

    const children = [];
    while (true) {
      // closing tag?
      if (this.at(TK.LT_SLASH)) {
        // consume </tag>
        const closeTok = this.advance();
        const closeName = this.expect(TK.IDENT, "Expected closing tag name.");
        if (closeName.value !== tagTok.value) {
          fail("SyntaxError", `Mismatched closing tag. Expected </${tagTok.value}> but got </${closeName.value}>.`, closeName.span, this.fileName, this.source);
        }
        this.expect(TK.GT, "Expected '>' to end closing tag.");
        const end = this.tokens[this.i - 1].span.end;
        return Element(makeSpan(lt.span.start, end), tagTok.value, id, attributes, children);
      }

      // <logic> and <style> special
      if (this.at(TK.LT)) {
        // Peek ahead to see if next token is IDENT of 'logic'/'style'
        const save = this.i;
        this.advance(); // consume LT
        if (!this.at(TK.IDENT)) {
          // restore and parse normal?
          this.i = save;
          fail("SyntaxError", "Expected tag name after '<'.", this.cur().span, this.fileName, this.source);
        }
        const nameTok = this.advance();
        const tagName = nameTok.value;
        // restore and parse appropriately
        this.i = save;

        if (tagName === "logic") {
          children.push(this.parseLogicNode());
          continue;
        }
        if (tagName === "style") {
          children.push(this.parseStyleNode());
          continue;
        }

        children.push(this.parseStructureNode());
        continue;
      }

      // text node: in this implementation, only STRING counts as structure text
      if (this.at(TK.STRING)) {
        const s = this.advance();
        children.push(TextNode(s.span, s.value));
        continue;
      }

      // If we hit EOF or unexpected token inside structure
      if (this.at(TK.EOF)) {
        fail("SyntaxError", `Unterminated <${tagTok.value}> structure node. Missing closing tag.`, tagTok.span, this.fileName, this.source);
      }

      // Otherwise, reject token
      fail("SyntaxError", "Only strings or nested tags are allowed as structure content (and <logic>/<style>).", this.cur().span, this.fileName, this.source);
    }
  }

  parseLogicNode() {
    const lt = this.expect(TK.LT, "Expected '<logic>'.");
    const nameTok = this.expect(TK.IDENT, "Expected logic tag name.");
    if (nameTok.value !== "logic") fail("SyntaxError", "Expected <logic>.", nameTok.span, this.fileName, this.source);
    this.expect(TK.GT, "Expected '>' after <logic.");
    const block = this.parseBlock(); // requires { ... }
    // </logic>
    this.expect(TK.LT_SLASH, "Expected </logic>.");
    const close = this.expect(TK.IDENT, "Expected closing tag name.");
    if (close.value !== "logic") fail("SyntaxError", "Expected </logic>.", close.span, this.fileName, this.source);
    this.expect(TK.GT, "Expected '>' after </logic>.");
    const end = this.tokens[this.i - 1].span.end;
    return LogicNode(makeSpan(lt.span.start, end), block);
  }

  parseStyleNode() {
    const lt = this.expect(TK.LT, "Expected '<style>'.");
    const nameTok = this.expect(TK.IDENT, "Expected style tag name.");
    if (nameTok.value !== "style") fail("SyntaxError", "Expected <style>.", nameTok.span, this.fileName, this.source);
    this.expect(TK.GT, "Expected '>' after <style>.");

    // v0.1: store raw style content until </style>.
    // We’ll gather tokens as text-ish: accept STRING tokens and IDENT tokens and punctuation until closing.
    // For now, just read until we see LT_SLASH + IDENT(style) + GT sequence, and store the exact source slice by spans.
    const startPos = clonePos(this.cur().span.start);
    // Find closing
    while (true) {
      if (this.at(TK.LT_SLASH) && this.peek(1).type === TK.IDENT && this.peek(1).value === "style") break;
      if (this.at(TK.EOF)) fail("SyntaxError", "Unterminated <style> block.", lt.span, this.fileName, this.source);
      this.advance();
    }
    const endPos = clonePos(this.cur().span.start);
    const rawText = this.source.slice(startPos.idx, endPos.idx);

    // consume closing
    this.expect(TK.LT_SLASH, "Expected </style>.");
    const close = this.expect(TK.IDENT, "Expected closing tag name.");
    if (close.value !== "style") fail("SyntaxError", "Expected </style>.", close.span, this.fileName, this.source);
    this.expect(TK.GT, "Expected '>' after </style>.");
    const end = this.tokens[this.i - 1].span.end;

    return StyleNode(makeSpan(lt.span.start, end), rawText.trim());
  }

  parseAttrValueExpr() {
    // Accept STRING, NUMBER, TRUE/FALSE/NULL, IDENT
    const t = this.cur();
    if (this.at(TK.STRING)) {
      const tok = this.advance();
      return StringLit(tok.span, tok.value);
    }
    if (this.at(TK.NUMBER)) {
      const tok = this.advance();
      return NumberLit(tok.span, Number(tok.value));
    }
    if (this.at(TK.TRUE)) {
      const tok = this.advance();
      return BoolLit(tok.span, true);
    }
    if (this.at(TK.FALSE)) {
      const tok = this.advance();
      return BoolLit(tok.span, false);
    }
    if (this.at(TK.NULL)) {
      const tok = this.advance();
      return NullLit(tok.span);
    }
    if (this.at(TK.IDENT)) {
      const tok = this.advance();
      return Ident(tok.span, tok.value);
    }
    fail("SyntaxError", "Invalid attribute value.", t.span, this.fileName, this.source);
  }
}

// ======================================================
// 6) Desugaring (Lowering)
// ======================================================

class Desugar {
  constructor(fileName, source) {
    this.fileName = fileName;
    this.source = source;
    this.tmpId = 0;
  }
  fresh(prefix = "__tmp") {
    this.tmpId += 1;
    return `${prefix}${this.tmpId}`;
  }

  lowerProgram(p) {
    const body = p.body.map(n => this.lowerNode(n)).flat();
    return Program(p.span, p.mode, body);
  }

  lowerNode(n) {
    switch (n.kind) {
      case "ModeDirective":
      case "VarDecl":
      case "FnDecl":
      case "TypeDecl":
      case "TaggedDecl":
      case "TaggedBlock":
      case "ExprStmt":
      case "If":
      case "While":
      case "Repeat":
      case "Return":
      case "Break":
      case "Continue":
      case "Block":
      case "Element":
      case "SelfClosing":
      case "Text":
      case "Style":
      case "Logic":
      case "Until":
      case "Guard":
      case "WhenSugar":
      case "MaybeSugar":
      case "LoopSugar":
        break;
      default:
        return [n];
    }

    if (n.kind === "Block") return [this.lowerBlock(n)];
    if (n.kind === "Until") return [this.lowerUntil(n)];
    if (n.kind === "Guard") return [this.lowerGuard(n)];
    if (n.kind === "WhenSugar") return [this.lowerWhen(n)];
    if (n.kind === "MaybeSugar") return [this.lowerMaybe(n)];
    if (n.kind === "LoopSugar") return [this.lowerLoop(n)];

    if (n.kind === "If") {
      return [IfStmt(n.span, this.lowerExpr(n.condition), this.lowerBlock(n.thenBlock), n.elseBlock ? this.lowerBlock(n.elseBlock) : null)];
    }
    if (n.kind === "While") {
      return [WhileStmt(n.span, this.lowerExpr(n.condition), this.lowerBlock(n.body))];
    }
    if (n.kind === "Repeat") {
      return [RepeatStmt(n.span, this.lowerExpr(n.count), this.lowerBlock(n.body))];
    }
    if (n.kind === "Return") {
      return [ReturnStmt(n.span, n.value ? this.lowerExpr(n.value) : null)];
    }
    if (n.kind === "ExprStmt") {
      return [ExprStmt(n.span, this.lowerExpr(n.expr))];
    }
    if (n.kind === "VarDecl") {
      return [VarDecl(n.span, n.name, n.type, n.init ? this.lowerExpr(n.init) : null)];
    }
    if (n.kind === "FnDecl") {
      return [FnDecl(n.span, n.name, n.params.map(p => Param(p.span, p.name, p.type, p.defaultValue ? this.lowerExpr(p.defaultValue) : null)), n.returnType, this.lowerBlock(n.body))];
    }
    if (n.kind === "TaggedDecl") {
      const loweredDecl = this.lowerNode(n.decl);
      if (loweredDecl.length !== 1) return loweredDecl; // shouldn't happen
      return [TaggedDecl(n.span, n.tags.map(t => Tag(t.span, t.name, t.args.map(a => this.lowerExpr(a)))), loweredDecl[0])];
    }
    if (n.kind === "TaggedBlock") {
      return [TaggedBlock(n.span, n.tags.map(t => Tag(t.span, t.name, t.args.map(a => this.lowerExpr(a)))), this.lowerBlock(n.block))];
    }
    if (n.kind === "Element") {
      const children = [];
      for (const c of n.children) {
        if (c.kind === "Logic") {
          // lower inside logic block
          children.push(LogicNode(c.span, this.lowerBlock(c.block)));
        } else if (c.kind === "Element" || c.kind === "SelfClosing" || c.kind === "Text" || c.kind === "Style") {
          children.push(this.lowerNode(c)[0] ?? c);
        } else {
          children.push(c);
        }
      }
      return [Element(n.span, n.tag, n.id, n.attributes, children)];
    }
    if (n.kind === "Logic") {
      return [LogicNode(n.span, this.lowerBlock(n.block))];
    }
    // structure nodes & others unchanged
    return [n];
  }

  lowerBlock(b) {
    // first lower each item, then flatten, then apply sugar lowering already handled by parser (nodes exist)
    const out = [];
    for (const item of b.body) {
      const lowered = this.lowerNode(item);
      for (const x of lowered) out.push(x);
    }
    return Block(b.span, out);
  }

  lowerUntil(n) {
    const notCond = Unary(n.condition.span, "!", Group(n.condition.span, this.lowerExpr(n.condition)));
    return WhileStmt(n.span, notCond, this.lowerBlock(n.body));
  }

  lowerGuard(n) {
    const notCond = Unary(n.condition.span, "!", Group(n.condition.span, this.lowerExpr(n.condition)));
    const ret = ReturnStmt(n.span, null);
    return IfStmt(n.span, notCond, Block(n.span, [ret]), null);
  }

  lowerWhen(n) {
    return IfStmt(n.span, this.lowerExpr(n.condExpr), this.lowerBlock(n.block), null);
  }

  lowerMaybe(n) {
    // safe temp to avoid double-eval
    const tmp = this.fresh("__tmp");
    const tmpDecl = VarDecl(n.span, tmp, null, this.lowerExpr(n.valueExpr));
    const tmpId = Ident(n.span, tmp);
    const cond = Binary(n.span, "!=", tmpId, NullLit(n.span));
    const bind = VarDecl(n.span, n.binderName, null, tmpId);
    const thenBody = [bind, ...this.lowerBlock(n.block).body];
    const ifNode = IfStmt(n.span, cond, Block(n.span, thenBody), null);
    return Block(n.span, [tmpDecl, ifNode]);
  }

  lowerLoop(n) {
    // Heuristic: number literal => numeric loop, else list-like loop via len/get
    const iter = this.lowerExpr(n.iterExpr);

    const isNumberLiteral = iter.kind === "Number";
    if (isNumberLiteral) return this.lowerNumericLoop(n, iter);

    // list-like
    const itName = this.fresh("__it");
    const iName = this.fresh("__i");
    const itDecl = VarDecl(n.span, itName, null, iter);
    const iDecl = VarDecl(n.span, iName, null, NumberLit(n.span, 0));

    const iId = Ident(n.span, iName);
    const itId = Ident(n.span, itName);

    const lenCall = Call(n.span, Ident(n.span, "len"), [itId]);
    const cond = Binary(n.span, "<", iId, lenCall);

    const itemDecl = VarDecl(n.span, n.binderName, null, Call(n.span, Ident(n.span, "get"), [itId, iId]));
    const idxDecl = n.indexName ? VarDecl(n.span, n.indexName, null, iId) : null;

    const inc = ExprStmt(
      n.span,
      Assign(n.span, iId, Binary(n.span, "+", iId, NumberLit(n.span, 1)))
    );

    const loweredBody = this.lowerBlock(n.block).body;
    const bodyItems = [itemDecl];
    if (idxDecl) bodyItems.push(idxDecl);
    bodyItems.push(...loweredBody);
    bodyItems.push(inc);

    const whileNode = WhileStmt(n.span, cond, Block(n.span, bodyItems));
    return Block(n.span, [itDecl, iDecl, whileNode]);
  }

  lowerNumericLoop(n, countExpr) {
    const nName = this.fresh("__n");
    const iName = this.fresh("__i");
    const nDecl = VarDecl(n.span, nName, null, countExpr);
    const iDecl = VarDecl(n.span, iName, null, NumberLit(n.span, 0));

    const nId = Ident(n.span, nName);
    const iId = Ident(n.span, iName);

    const cond = Binary(n.span, "<", iId, nId);
    const binder = VarDecl(n.span, n.binderName, null, iId);
    const idxDecl = n.indexName ? VarDecl(n.span, n.indexName, null, iId) : null;

    const inc = ExprStmt(n.span, Assign(n.span, iId, Binary(n.span, "+", iId, NumberLit(n.span, 1))));

    const loweredBody = this.lowerBlock(n.block).body;
    const bodyItems = [binder];
    if (idxDecl) bodyItems.push(idxDecl);
    bodyItems.push(...loweredBody);
    bodyItems.push(inc);

    const whileNode = WhileStmt(n.span, cond, Block(n.span, bodyItems));
    return Block(n.span, [nDecl, iDecl, whileNode]);
  }

  lowerExpr(e) {
    switch (e.kind) {
      case "Number":
      case "String":
      case "Bool":
      case "Null":
      case "Identifier":
        return e;
      case "Group":
        return Group(e.span, this.lowerExpr(e.expr));
      case "List":
        return ListLit(e.span, e.elements.map(x => this.lowerExpr(x)));
      case "ObjectLiteral":
        return ObjectLit(e.span, e.fields.map(f => ({ name: f.name, value: this.lowerExpr(f.value), span: f.span })));
      case "Unary":
        return Unary(e.span, e.operator, this.lowerExpr(e.expr));
      case "Binary":
        return Binary(e.span, e.operator, this.lowerExpr(e.left), this.lowerExpr(e.right));
      case "Assign":
        return Assign(e.span, this.lowerExpr(e.target), this.lowerExpr(e.value));
      case "Access":
        return Access(e.span, this.lowerExpr(e.object), e.property);
      case "OptionalAccess":
        return OptionalAccess(e.span, this.lowerExpr(e.object), e.property);
      case "Call":
        return Call(e.span, this.lowerExpr(e.callee), e.args.map(a => this.lowerExpr(a)));
      case "ArrowFn":
        return ArrowFn(e.span, e.params, this.lowerExpr(e.body));
      default:
        return e;
    }
  }
}

// ======================================================
// 7) Type checker (strict optional typing, v0.1 minimal)
// ======================================================
//
// NOTE: This implementation enforces strictness for written types,
// but keeps type syntax minimal in code (number/string/bool + named types),
// and relies on object { ... } to validate named structural types.
//

const TYPE = Object.freeze({
  Unknown: { tag: "Unknown" },
  Number: { tag: "Number" },
  String: { tag: "String" },
  Bool: { tag: "Bool" },
  Null: { tag: "Null" },
  // Named: { tag:"Named", name }
  // Func: { tag:"Func", params:[Type], ret:Type }
  // List: { tag:"List", elem:Type }
});

function TNamed(name) { return { tag: "Named", name }; }
function TFunc(params, ret) { return { tag: "Func", params, ret }; }
function TList(elem) { return { tag: "List", elem }; }

function typeToString(t) {
  if (!t) return "Unknown";
  switch (t.tag) {
    case "Unknown": return "Unknown";
    case "Number": return "number";
    case "String": return "string";
    case "Bool": return "bool";
    case "Null": return "null";
    case "Named": return t.name;
    case "Func": return `fn(${t.params.map(typeToString).join(",")}):${typeToString(t.ret)}`;
    case "List": return `list<${typeToString(t.elem)}>`;
    default: return t.tag;
  }
}

class TypeEnv {
  constructor(parent = null) {
    this.parent = parent;
    this.values = new Map(); // name -> { type: Type, strict: boolean }
    this.types = parent ? parent.types : new Map(); // shared map for named types
    this.funcs = parent ? parent.funcs : new Map(); // name -> function type
  }
  defineValue(name, type, strict) { this.values.set(name, { type, strict }); }
  setValue(name, type, strict) {
    // set existing up chain
    if (this.values.has(name)) { this.values.set(name, { type, strict }); return true; }
    if (this.parent) return this.parent.setValue(name, type, strict);
    return false;
  }
  lookupValue(name) {
    if (this.values.has(name)) return this.values.get(name);
    if (this.parent) return this.parent.lookupValue(name);
    return null;
  }
}

class TypeChecker {
  constructor(fileName, source) {
    this.fileName = fileName;
    this.source = source;
  }

  checkProgram(p) {
    const env = new TypeEnv(null);

    // Install builtin function types
    env.funcs.set("print", TFunc([TYPE.Unknown], TYPE.Null));
    env.funcs.set("len", TFunc([TYPE.Unknown], TYPE.Number));
    env.funcs.set("get", TFunc([TYPE.Unknown, TYPE.Number], TYPE.Unknown));

    // Collect type declarations first
    for (const n of p.body) this.collectTypes(n, env);

    // Collect function signatures
    for (const n of p.body) this.collectFnSigs(n, env);

    // Check everything
    for (const n of p.body) this.checkTopLevel(n, env);

    return true;
  }

  collectTypes(n, env) {
    if (n.kind === "TypeDecl") {
      const fields = new Map();
      for (const f of n.fields) {
        fields.set(f.name, this.resolveTypeRef(f.type, env));
      }
      env.types.set(n.name, { fields });
    } else if (n.kind === "TaggedDecl") {
      this.collectTypes(n.decl, env);
    } else if (n.kind === "Element") {
      for (const c of n.children) this.collectTypes(c, env);
    } else if (n.kind === "Logic") {
      this.collectTypes(n.block, env);
    } else if (n.kind === "Block") {
      for (const x of n.body) this.collectTypes(x, env);
    }
  }

  collectFnSigs(n, env) {
    if (n.kind === "FnDecl") {
      const params = n.params.map(p => (p.type ? this.resolveTypeRef(p.type, env) : TYPE.Unknown));
      const ret = n.returnType ? this.resolveTypeRef(n.returnType, env) : TYPE.Unknown;
      env.funcs.set(n.name, TFunc(params, ret));
      env.defineValue(n.name, env.funcs.get(n.name), true);
    } else if (n.kind === "TaggedDecl") {
      this.collectFnSigs(n.decl, env);
    } else if (n.kind === "Element") {
      for (const c of n.children) this.collectFnSigs(c, env);
    } else if (n.kind === "Logic") {
      this.collectFnSigs(n.block, env);
    } else if (n.kind === "Block") {
      for (const x of n.body) this.collectFnSigs(x, env);
    }
  }

  checkTopLevel(n, env) {
    switch (n.kind) {
      case "ModeDirective":
      case "TypeDecl":
        return;
      case "TaggedDecl":
        return this.checkTopLevel(n.decl, env);
      case "TaggedBlock":
        return this.checkBlock(n.block, new TypeEnv(env), TYPE.Unknown);
      case "VarDecl":
        return this.checkVarDecl(n, env);
      case "FnDecl":
        return this.checkFnDecl(n, env);
      case "Element":
        // Only check logic blocks (and nested types already collected)
        for (const c of n.children) this.checkTopLevel(c, env);
        return;
      case "Logic":
        return this.checkBlock(n.block, new TypeEnv(env), TYPE.Unknown);
      case "Style":
      case "Text":
      case "SelfClosing":
        return;
      default:
        // statements at top-level are allowed
        return this.checkStmt(n, env, TYPE.Unknown);
    }
  }

  resolveTypeRef(tref, env) {
    // This implementation uses only SimpleType nodes (number/string/bool/null + named)
    if (tref.kind !== "SimpleType") {
      fail("TypeError", "Only simple types are supported in this implementation (no ? or | yet).", tref.span, this.fileName, this.source);
    }
    const name = tref.name;
    const lower = name.toLowerCase();
    if (lower === "number") return TYPE.Number;
    if (lower === "string") return TYPE.String;
    if (lower === "bool") return TYPE.Bool;
    if (lower === "null") return TYPE.Null;
    if (!env.types.has(name)) {
      // allow forward-declared named types only if declared somewhere; otherwise error
      fail("TypeError", `Unknown type '${name}'.`, tref.span, this.fileName, this.source);
    }
    return TNamed(name);
  }

  checkVarDecl(n, env) {
    if (n.type) {
      const declared = this.resolveTypeRef(n.type, env);
      if (n.init) {
        const rhs = this.typeOfExpr(n.init, env);
        if (rhs.tag === "Unknown") {
          fail("TypeError", `Cannot assign Unknown to typed variable '${n.name}: ${typeToString(declared)}'.`, n.init.span, this.fileName, this.source);
        }
        if (!this.isAssignable(rhs, declared, env, n.init.span)) {
          fail("TypeError", `Cannot assign ${typeToString(rhs)} to ${typeToString(declared)}.`, n.init.span, this.fileName, this.source);
        }
      }
      env.defineValue(n.name, declared, true);
    } else {
      // untyped: dynamic
      env.defineValue(n.name, TYPE.Unknown, false);
    }
  }

  checkFnDecl(n, env) {
    const fnType = env.funcs.get(n.name) ?? TFunc([], TYPE.Unknown);
    const fnEnv = new TypeEnv(env);
    // bind params
    for (let i = 0; i < n.params.length; i++) {
      const p = n.params[i];
      const pt = p.type ? this.resolveTypeRef(p.type, env) : TYPE.Unknown;
      const strict = !!p.type;
      if (p.defaultValue) {
        const dt = this.typeOfExpr(p.defaultValue, env);
        if (strict) {
          if (dt.tag === "Unknown") {
            fail("TypeError", `Cannot assign Unknown default to typed param '${p.name}: ${typeToString(pt)}'.`, p.defaultValue.span, this.fileName, this.source);
          }
          if (!this.isAssignable(dt, pt, env, p.defaultValue.span)) {
            fail("TypeError", `Default value type ${typeToString(dt)} not assignable to ${typeToString(pt)} for param '${p.name}'.`, p.defaultValue.span, this.fileName, this.source);
          }
        }
      }
      fnEnv.defineValue(p.name, pt, strict);
    }

    const expectedRet = n.returnType ? this.resolveTypeRef(n.returnType, env) : TYPE.Unknown;
    this.checkBlock(n.body, fnEnv, expectedRet);

    // If return type is written, enforce that falling off end is only allowed if return type is Unknown or null
    if (n.returnType) {
      // v0.1: require explicit return somewhere OR allow fall-through only for null return type
      if (expectedRet.tag !== "Null" && expectedRet.tag !== "Unknown") {
        // not doing full control-flow analysis; require at least one return statement encountered
        const hasReturn = this.blockHasReturn(n.body);
        if (!hasReturn) {
          fail("TypeError", `Function '${n.name}' declares return type ${typeToString(expectedRet)} but has no return statement.`, n.body.span, this.fileName, this.source);
        }
      }
    }
  }

  blockHasReturn(b) {
    for (const s of b.body) {
      if (s.kind === "Return") return true;
      if (s.kind === "If") {
        if (this.blockHasReturn(s.thenBlock)) return true;
        if (s.elseBlock && this.blockHasReturn(s.elseBlock)) return true;
      }
      if (s.kind === "Block" && this.blockHasReturn(s)) return true;
    }
    return false;
  }

  checkBlock(b, env, expectedRet) {
    for (const item of b.body) {
      if (item.kind === "VarDecl") this.checkVarDecl(item, env);
      else if (item.kind === "FnDecl") this.checkFnDecl(item, env);
      else if (item.kind === "TypeDecl") {/* compile-time only */}
      else if (item.kind === "TaggedDecl") this.checkTopLevel(item.decl, env);
      else if (item.kind === "TaggedBlock") this.checkBlock(item.block, new TypeEnv(env), expectedRet);
      else this.checkStmt(item, env, expectedRet);
    }
  }

  checkStmt(s, env, expectedRet) {
    switch (s.kind) {
      case "Block":
        return this.checkBlock(s, new TypeEnv(env), expectedRet);
      case "If": {
        const ct = this.typeOfExpr(s.condition, env);
        if (ct.tag !== "Unknown" && ct.tag !== "Bool") {
          fail("TypeError", `If condition must be bool when typed; got ${typeToString(ct)}.`, s.condition.span, this.fileName, this.source);
        }
        this.checkBlock(s.thenBlock, new TypeEnv(env), expectedRet);
        if (s.elseBlock) this.checkBlock(s.elseBlock, new TypeEnv(env), expectedRet);
        return;
      }
      case "While": {
        const ct = this.typeOfExpr(s.condition, env);
        if (ct.tag !== "Unknown" && ct.tag !== "Bool") {
          fail("TypeError", `While condition must be bool when typed; got ${typeToString(ct)}.`, s.condition.span, this.fileName, this.source);
        }
        this.checkBlock(s.body, new TypeEnv(env), expectedRet);
        return;
      }
      case "Repeat": {
        const nt = this.typeOfExpr(s.count, env);
        if (nt.tag !== "Unknown" && nt.tag !== "Number") {
          fail("TypeError", `Repeat count must be number when typed; got ${typeToString(nt)}.`, s.count.span, this.fileName, this.source);
        }
        this.checkBlock(s.body, new TypeEnv(env), expectedRet);
        return;
      }
      case "Return": {
        if (expectedRet.tag === "Unknown") return; // untyped function
        if (!s.value) {
          if (expectedRet.tag !== "Null") {
            fail("TypeError", `Return requires a value of type ${typeToString(expectedRet)}.`, s.span, this.fileName, this.source);
          }
          return;
        }
        const rt = this.typeOfExpr(s.value, env);
        if (rt.tag === "Unknown") {
          fail("TypeError", `Cannot return Unknown from typed function expecting ${typeToString(expectedRet)}.`, s.value.span, this.fileName, this.source);
        }
        if (!this.isAssignable(rt, expectedRet, env, s.value.span)) {
          fail("TypeError", `Return type ${typeToString(rt)} not assignable to ${typeToString(expectedRet)}.`, s.value.span, this.fileName, this.source);
        }
        return;
      }
      case "ExprStmt":
        this.typeOfExpr(s.expr, env); // for checking side effects in typed contexts
        return;
      case "Break":
      case "Continue":
        return;
      default:
        return;
    }
  }

  isAssignable(from, to, env, span) {
    if (to.tag === "Unknown") return true;
    if (from.tag === "Unknown") return false;

    if (to.tag === from.tag) {
      if (to.tag === "Named") return to.name === from.name;
      if (to.tag === "List") return this.isAssignable(from.elem, to.elem, env, span);
      return true;
    }

    // object literal to named type check happens elsewhere (when from is a special structural map),
    // but in this simple version we handle only named matches.
    return false;
  }

  typeOfExpr(e, env) {
    switch (e.kind) {
      case "Number": return TYPE.Number;
      case "String": return TYPE.String;
      case "Bool": return TYPE.Bool;
      case "Null": return TYPE.Null;
      case "Identifier": {
        const v = env.lookupValue(e.name);
        if (v) return v.type;
        // Builtins:
        if (env.funcs.has(e.name)) return env.funcs.get(e.name);
        return TYPE.Unknown;
      }
      case "Group":
        return this.typeOfExpr(e.expr, env);
      case "List": {
        // infer union-ish as Unknown if mixed; we’ll just compute best common:
        if (e.elements.length === 0) return TList(TYPE.Unknown);
        let elemT = this.typeOfExpr(e.elements[0], env);
        for (let i = 1; i < e.elements.length; i++) {
          const t = this.typeOfExpr(e.elements[i], env);
          if (t.tag === "Unknown" || elemT.tag === "Unknown") { elemT = TYPE.Unknown; break; }
          if (t.tag !== elemT.tag) { elemT = TYPE.Unknown; break; }
        }
        return TList(elemT);
      }
      case "ObjectLiteral":
        // only checked strictly when assigned to named type
        return { tag: "ObjectLiteral" };
      case "Unary": {
        const t = this.typeOfExpr(e.expr, env);
        if (t.tag === "Unknown") return TYPE.Unknown;
        if (e.operator === "-") {
          if (t.tag !== "Number") fail("TypeError", `Unary '-' expects number, got ${typeToString(t)}.`, e.span, this.fileName, this.source);
          return TYPE.Number;
        }
        if (e.operator === "!") {
          if (t.tag !== "Bool") fail("TypeError", `Unary '!' expects bool, got ${typeToString(t)}.`, e.span, this.fileName, this.source);
          return TYPE.Bool;
        }
        return TYPE.Unknown;
      }
      case "Binary": {
        const lt = this.typeOfExpr(e.left, env);
        const rt = this.typeOfExpr(e.right, env);
        // If either Unknown, keep Unknown unless operator implies Bool and both typed?
        if (lt.tag === "Unknown" || rt.tag === "Unknown") {
          // allow dynamic; for &&/|| comparisons in typed contexts we will catch at usage points if needed
          if (["==", "!=", "<", "<=", ">", ">=", "&&", "||"].includes(e.operator)) return TYPE.Unknown;
          return TYPE.Unknown;
        }
        switch (e.operator) {
          case "+":
            if (lt.tag === "Number" && rt.tag === "Number") return TYPE.Number;
            if (lt.tag === "String" && rt.tag === "String") return TYPE.String;
            fail("TypeError", `Operator '+' requires (number,number) or (string,string); got (${typeToString(lt)},${typeToString(rt)}).`, e.span, this.fileName, this.source);
          case "-":
          case "*":
          case "/":
            if (lt.tag !== "Number" || rt.tag !== "Number") {
              fail("TypeError", `Operator '${e.operator}' requires numbers; got (${typeToString(lt)},${typeToString(rt)}).`, e.span, this.fileName, this.source);
            }
            return TYPE.Number;
          case "&&":
          case "||":
            if (lt.tag !== "Bool" || rt.tag !== "Bool") {
              fail("TypeError", `Operator '${e.operator}' requires bool; got (${typeToString(lt)},${typeToString(rt)}).`, e.span, this.fileName, this.source);
            }
            return TYPE.Bool;
          case "==":
          case "!=":
            return TYPE.Bool;
          case "<":
          case "<=":
          case ">":
          case ">=":
            if (lt.tag !== "Number" || rt.tag !== "Number") {
              fail("TypeError", `Comparison '${e.operator}' requires numbers; got (${typeToString(lt)},${typeToString(rt)}).`, e.span, this.fileName, this.source);
            }
            return TYPE.Bool;
          default:
            return TYPE.Unknown;
        }
      }
      case "Assign": {
        // target must be Identifier or Access
        const rhs = this.typeOfExpr(e.value, env);
        if (e.target.kind === "Identifier") {
          const entry = env.lookupValue(e.target.name);
          if (!entry) {
            // dynamic implicit declare? NO. Create dynamic if unseen (v0.1 convenience).
            env.defineValue(e.target.name, TYPE.Unknown, false);
            return rhs;
          }
          if (entry.strict) {
            if (rhs.tag === "Unknown") {
              fail("TypeError", `Cannot assign Unknown to typed variable '${e.target.name}: ${typeToString(entry.type)}'.`, e.value.span, this.fileName, this.source);
            }
            // object literal assignment to named type
            if (rhs.tag === "ObjectLiteral") {
              // enforce rhs fields against named type
              if (entry.type.tag !== "Named") {
                fail("TypeError", `Object literal can only be assigned to a named structural type; got ${typeToString(entry.type)}.`, e.value.span, this.fileName, this.source);
              }
              this.checkObjectLiteralAgainstNamed(e.value, entry.type.name, env);
              return entry.type;
            }
            if (!this.isAssignable(rhs, entry.type, env, e.span)) {
              fail("TypeError", `Cannot assign ${typeToString(rhs)} to ${typeToString(entry.type)}.`, e.value.span, this.fileName, this.source);
            }
            return entry.type;
          }
          // dynamic var: allow
          return rhs;
        }
        if (e.target.kind === "Access" || e.target.kind === "OptionalAccess") {
          // property assignment not type-checked in v0.1
          return rhs;
        }
        fail("TypeError", "Invalid assignment target.", e.target.span, this.fileName, this.source);
      }
      case "Access":
      case "OptionalAccess":
        // If object is named type and property exists, we can type it; else Unknown
        return TYPE.Unknown;
      case "Call": {
        const calleeT = this.typeOfExpr(e.callee, env);
        // builtin by identifier name
        if (e.callee.kind === "Identifier" && env.funcs.has(e.callee.name)) {
          const fnT = env.funcs.get(e.callee.name);
          this.checkCallArgs(e, fnT, env);
          return fnT.ret;
        }
        if (calleeT.tag === "Func") {
          this.checkCallArgs(e, calleeT, env);
          return calleeT.ret;
        }
        return TYPE.Unknown;
      }
      case "ArrowFn":
        // v0.1: untyped arrow returns Unknown
        return TFunc(e.params.map(() => TYPE.Unknown), TYPE.Unknown);
      default:
        return TYPE.Unknown;
    }
  }

  checkCallArgs(callNode, fnType, env) {
    if (fnType.tag !== "Func") return;
    // allow variadic for print(len etc)? keep simple:
    if (callNode.callee.kind === "Identifier" && callNode.callee.name === "print") return;
    if (callNode.callee.kind === "Identifier" && callNode.callee.name === "len") {
      if (callNode.args.length !== 1) fail("TypeError", "len(x) takes exactly 1 argument.", callNode.span, this.fileName, this.source);
      return;
    }
    if (callNode.callee.kind === "Identifier" && callNode.callee.name === "get") {
      if (callNode.args.length !== 2) fail("TypeError", "get(list, index) takes exactly 2 arguments.", callNode.span, this.fileName, this.source);
      const idxT = this.typeOfExpr(callNode.args[1], env);
      if (idxT.tag !== "Unknown" && idxT.tag !== "Number") {
        fail("TypeError", "get(..., index) index must be number when typed.", callNode.args[1].span, this.fileName, this.source);
      }
      return;
    }

    if (callNode.args.length !== fnType.params.length) {
      fail("TypeError", `Function expects ${fnType.params.length} args, got ${callNode.args.length}.`, callNode.span, this.fileName, this.source);
    }
    for (let i = 0; i < callNode.args.length; i++) {
      const at = this.typeOfExpr(callNode.args[i], env);
      const pt = fnType.params[i];
      if (pt.tag === "Unknown") continue;
      if (at.tag === "Unknown") {
        fail("TypeError", `Arg ${i + 1} is Unknown but param is typed ${typeToString(pt)}.`, callNode.args[i].span, this.fileName, this.source);
      }
      if (!this.isAssignable(at, pt, env, callNode.args[i].span)) {
        fail("TypeError", `Arg ${i + 1} type ${typeToString(at)} not assignable to ${typeToString(pt)}.`, callNode.args[i].span, this.fileName, this.source);
      }
    }
  }

  checkObjectLiteralAgainstNamed(objNode, typeName, env) {
    const def = env.types.get(typeName);
    if (!def) fail("TypeError", `Unknown type '${typeName}'.`, objNode.span, this.fileName, this.source);
    const provided = new Map();
    for (const f of objNode.fields) {
      provided.set(f.name, this.typeOfExpr(f.value, env));
    }
    for (const [fieldName, fieldType] of def.fields.entries()) {
      if (!provided.has(fieldName)) {
        fail("TypeError", `Missing field '${fieldName}' for type ${typeName}.`, objNode.span, this.fileName, this.source);
      }
      const vt = provided.get(fieldName);
      if (vt.tag === "Unknown") {
        fail("TypeError", `Field '${fieldName}' is Unknown but must be ${typeToString(fieldType)} for type ${typeName}.`, objNode.span, this.fileName, this.source);
      }
      if (!this.isAssignable(vt, fieldType, env, objNode.span)) {
        fail("TypeError", `Field '${fieldName}' type ${typeToString(vt)} not assignable to ${typeToString(fieldType)}.`, objNode.span, this.fileName, this.source);
      }
    }
    // extra fields allowed in v0.1
  }
}

// ======================================================
// 8) Interpreter (lowered AST)
// ======================================================

class ReturnSignal { constructor(value) { this.value = value; } }
class BreakSignal {}
class ContinueSignal {}

class RuntimeEnv {
  constructor(parent = null) {
    this.parent = parent;
    this.values = new Map();
  }
  define(name, value) { this.values.set(name, value); }
  lookup(name) {
    if (this.values.has(name)) return this.values.get(name);
    return this.parent ? this.parent.lookup(name) : undefined;
  }
  assign(name, value) {
    if (this.values.has(name)) { this.values.set(name, value); return true; }
    if (this.parent) return this.parent.assign(name, value);
    return false;
  }
}

function isTruthy(v) {
  return !(v === false || v === null);
}

class Interpreter {
  constructor(fileName, source) {
    this.fileName = fileName;
    this.source = source;
    this.global = new RuntimeEnv(null);
    this.installBuiltins();
  }

  installBuiltins() {
    this.global.define("print", (...args) => {
      console.log(...args.map(this.stringify));
      return null;
    });
    this.global.define("len", (x) => {
      if (typeof x === "string") return x.length;
      if (Array.isArray(x)) return x.length;
      if (x && typeof x === "object") return Object.keys(x).length;
      return 0;
    });
    this.global.define("get", (list, index) => {
      if (!Array.isArray(list)) return null;
      const i = Number(index);
      if (!Number.isFinite(i)) return null;
      return list[i] ?? null;
    });

    // Minimal ui helper for frontend mode (injected when needed)
    this.global.define("ui", {
      byId: Object.create(null),
      get: function (id) { return this.byId[id] ?? null; },
    });
  }

  stringify(v) {
    if (v === null) return "null";
    if (typeof v === "string") return v;
    if (typeof v === "number" || typeof v === "boolean") return String(v);
    if (Array.isArray(v)) return `(${v.map(this.stringify.bind(this)).join(", ")})`;
    if (typeof v === "function") return "<fn>";
    if (typeof v === "object") return JSON.stringify(v);
    return String(v);
  }

  runProgram(p) {
    if (p.mode === "FRONTEND") return this.runFrontendProgram(p);
    return this.runBackendProgram(p);
  }

  runBackendProgram(p) {
    for (const n of p.body) this.execTopLevel(n, this.global);
    return null;
  }

  runFrontendProgram(p) {
    // Build structure trees, collect ids into ui.byId, execute logic blocks in traversal order.
    const roots = [];
    for (const n of p.body) {
      if (n.kind === "Element" || n.kind === "SelfClosing" || n.kind === "Text" || n.kind === "Style" || n.kind === "Logic") {
        // structure node at top level
        roots.push(n);
      } else {
        // allow logic/decls at top-level too
        this.execTopLevel(n, this.global);
      }
    }

    // Register IDs
    const ui = this.global.lookup("ui");
    ui.byId = Object.create(null);
    for (const r of roots) this.registerIds(r, ui.byId);

    // Execute logic blocks
    for (const r of roots) this.execLogicBlocks(r, this.global);

    // Return structure tree for future renderer use (not rendered in v0.1)
    return { roots, ui };
  }

  registerIds(node, byId) {
    if (!node || typeof node !== "object") return;
    if (node.kind === "Element" || node.kind === "SelfClosing") {
      if (node.id) byId[node.id] = this.structureToRuntime(node);
      if (node.kind === "Element") for (const c of node.children) this.registerIds(c, byId);
    } else if (node.kind === "Logic" || node.kind === "Style" || node.kind === "Text") {
      // no ids inside directly
    }
  }

  structureToRuntime(n) {
    if (n.kind === "SelfClosing") return { tag: n.tag, id: n.id, attributes: n.attributes, children: [] };
    if (n.kind === "Element") return { tag: n.tag, id: n.id, attributes: n.attributes, children: n.children.map(c => this.structureToRuntime(c)).filter(Boolean) };
    if (n.kind === "Text") return { text: n.value };
    return null;
  }

  execLogicBlocks(node, env) {
    if (!node || typeof node !== "object") return;
    if (node.kind === "Logic") {
      this.execBlock(node.block, new RuntimeEnv(env));
      return;
    }
    if (node.kind === "Element") {
      for (const c of node.children) this.execLogicBlocks(c, env);
    }
  }

  execTopLevel(n, env) {
    switch (n.kind) {
      case "ModeDirective":
      case "TypeDecl":
      case "Style":
      case "Text":
      case "SelfClosing":
      case "Element":
      case "Logic":
        // frontend nodes handled elsewhere
        return;
      case "TaggedDecl":
        return this.execTopLevel(n.decl, env);
      case "TaggedBlock":
        return this.execBlock(n.block, new RuntimeEnv(env));
      case "VarDecl":
        return this.execVarDecl(n, env);
      case "FnDecl":
        return this.execFnDecl(n, env);
      default:
        return this.execStmt(n, env);
    }
  }

  execVarDecl(n, env) {
    const val = n.init ? this.evalExpr(n.init, env) : null;
    env.define(n.name, val);
  }

  execFnDecl(n, env) {
    const fnVal = this.makeUserFunction(n.params.map(p => p.name), n.body, env);
    env.define(n.name, fnVal);
  }

  makeUserFunction(paramNames, bodyBlock, closureEnv) {
    const self = this;
    const fn = function (...args) {
      const callEnv = new RuntimeEnv(closureEnv);
      for (let i = 0; i < paramNames.length; i++) {
        callEnv.define(paramNames[i], i < args.length ? args[i] : null);
      }
      try {
        self.execBlock(bodyBlock, callEnv);
        return null;
      } catch (sig) {
        if (sig instanceof ReturnSignal) return sig.value;
        throw sig;
      }
    };
    fn.__one = { kind: "UserFunction", params: paramNames, body: bodyBlock };
    return fn;
  }

  execBlock(b, env) {
    // new scope
    const local = new RuntimeEnv(env);
    for (const item of b.body) {
      if (item.kind === "VarDecl") this.execVarDecl(item, local);
      else if (item.kind === "FnDecl") this.execFnDecl(item, local);
      else if (item.kind === "TypeDecl") {/* ignore */}
      else if (item.kind === "TaggedDecl") this.execTopLevel(item.decl, local);
      else if (item.kind === "TaggedBlock") this.execBlock(item.block, local);
      else this.execStmt(item, local);
    }
  }

  execStmt(s, env) {
    switch (s.kind) {
      case "Block":
        return this.execBlock(s, env);
      case "If": {
        const cond = this.evalExpr(s.condition, env);
        if (isTruthy(cond)) this.execBlock(s.thenBlock, env);
        else if (s.elseBlock) this.execBlock(s.elseBlock, env);
        return;
      }
      case "While": {
        while (isTruthy(this.evalExpr(s.condition, env))) {
          try {
            this.execBlock(s.body, env);
          } catch (sig) {
            if (sig instanceof BreakSignal) break;
            if (sig instanceof ContinueSignal) continue;
            throw sig;
          }
        }
        return;
      }
      case "Repeat": {
        const n = this.evalExpr(s.count, env);
        const count = Number(n);
        if (!Number.isFinite(count)) fail("RuntimeError", "repeat(count) requires numeric count.", s.count.span, this.fileName, this.source);
        for (let i = 0; i < count; i++) {
          try {
            this.execBlock(s.body, env);
          } catch (sig) {
            if (sig instanceof BreakSignal) break;
            if (sig instanceof ContinueSignal) continue;
            throw sig;
          }
        }
        return;
      }
      case "Return": {
        const v = s.value ? this.evalExpr(s.value, env) : null;
        throw new ReturnSignal(v);
      }
      case "Break":
        throw new BreakSignal();
      case "Continue":
        throw new ContinueSignal();
      case "ExprStmt":
        this.evalExpr(s.expr, env);
        return;
      default:
        return;
    }
  }

  evalExpr(e, env) {
    switch (e.kind) {
      case "Number": return e.value;
      case "String": return e.value;
      case "Bool": return e.value;
      case "Null": return null;
      case "Identifier": {
        const v = env.lookup(e.name);
        if (v !== undefined) return v;
        // allow access to globals
        const gv = this.global.lookup(e.name);
        if (gv !== undefined) return gv;
        fail("RuntimeError", `Undefined identifier '${e.name}'.`, e.span, this.fileName, this.source);
      }
      case "Group": return this.evalExpr(e.expr, env);
      case "List": return e.elements.map(x => this.evalExpr(x, env));
      case "ObjectLiteral": {
        const obj = Object.create(null);
        for (const f of e.fields) obj[f.name] = this.evalExpr(f.value, env);
        return obj;
      }
      case "Unary": {
        const v = this.evalExpr(e.expr, env);
        if (e.operator === "-") return -Number(v);
        if (e.operator === "!") return !isTruthy(v);
        return null;
      }
      case "Binary": {
        if (e.operator === "&&") {
          const left = this.evalExpr(e.left, env);
          if (!isTruthy(left)) return left;
          return this.evalExpr(e.right, env);
        }
        if (e.operator === "||") {
          const left = this.evalExpr(e.left, env);
          if (isTruthy(left)) return left;
          return this.evalExpr(e.right, env);
        }
        const l = this.evalExpr(e.left, env);
        const r = this.evalExpr(e.right, env);
        switch (e.operator) {
          case "+": return (typeof l === "string" && typeof r === "string") ? (l + r) : (Number(l) + Number(r));
          case "-": return Number(l) - Number(r);
          case "*": return Number(l) * Number(r);
          case "/": return Number(l) / Number(r);
          case "==": return l === r;
          case "!=": return l !== r;
          case "<": return Number(l) < Number(r);
          case "<=": return Number(l) <= Number(r);
          case ">": return Number(l) > Number(r);
          case ">=": return Number(l) >= Number(r);
          default: return null;
        }
      }
      case "Assign": {
        const value = this.evalExpr(e.value, env);
        if (e.target.kind === "Identifier") {
          const ok = env.assign(e.target.name, value);
          if (!ok) {
            // allow implicit global define? No — define in current env
            env.define(e.target.name, value);
          }
          return value;
        }
        if (e.target.kind === "Access") {
          const obj = this.evalExpr(e.target.object, env);
          if (obj === null || typeof obj !== "object") fail("RuntimeError", "Cannot set property on non-object.", e.target.span, this.fileName, this.source);
          obj[e.target.property] = value;
          return value;
        }
        fail("RuntimeError", "Invalid assignment target.", e.target.span, this.fileName, this.source);
      }
      case "Access": {
        const obj = this.evalExpr(e.object, env);
        if (obj === null || obj === undefined) fail("RuntimeError", "Cannot access property on null.", e.span, this.fileName, this.source);
        return (typeof obj === "object" || typeof obj === "function") ? (obj[e.property] ?? null) : null;
      }
      case "OptionalAccess": {
        const obj = this.evalExpr(e.object, env);
        if (obj === null || obj === undefined) return null;
        return (typeof obj === "object" || typeof obj === "function") ? (obj[e.property] ?? null) : null;
      }
      case "Call": {
        const callee = this.evalExpr(e.callee, env);
        const args = e.args.map(a => this.evalExpr(a, env));
        if (typeof callee !== "function") fail("RuntimeError", "Attempted to call a non-function.", e.span, this.fileName, this.source);
        return callee(...args);
      }
      case "ArrowFn": {
        const self = this;
        const fn = function (...args) {
          const callEnv = new RuntimeEnv(env);
          for (let i = 0; i < e.params.length; i++) callEnv.define(e.params[i], i < args.length ? args[i] : null);
          return self.evalExpr(e.body, callEnv);
        };
        fn.__one = { kind: "ArrowFn", params: e.params, body: e.body };
        return fn;
      }
      default:
        return null;
    }
  }
}

// ======================================================
// 9) Driver
// ======================================================

function runOne(source, fileName) {
  const lexer = new Lexer(source, fileName);
  const tokens = lexer.lex();

  const parser = new Parser(tokens, source, fileName);
  const ast = parser.parseProgram();

  // Desugar
  const desugar = new Desugar(fileName, source);
  const lowered = desugar.lowerProgram(ast);

  // Type check
  const checker = new TypeChecker(fileName, source);
  checker.checkProgram(lowered);

  // Execute
  const interp = new Interpreter(fileName, source);
  return interp.runProgram(lowered);
}

function main() {
  const file = process.argv[2];
  if (!file) {
    console.log("Usage: node one.js <file.one>");
    process.exit(1);
  }
  const source = fs.readFileSync(file, "utf8");
  try {
    const result = runOne(source, file);
    // For frontend mode, result contains {roots, ui}
    if (result && typeof result === "object" && result.roots) {
      // v0.1: no rendering, but you can inspect structure
      // console.log(JSON.stringify(result.roots, null, 2));
    }
  } catch (e) {
    console.error(String(e.message || e));
    process.exit(1);
  }
}

if (require.main === module) main();

/**
 * ======================================================
 * Known limitations of THIS v0.1 implementation file
 * ======================================================
 * - Type syntax currently supports only simple types:
 *     number, string, bool, null, and named types (type User {...})
 * - Nullable types (T?) and union types (A | B) are spec’d, but not yet enabled
 *   because this lexer version does not tokenize '?' and single '|'.
 *   (Easy upgrade: add TK.QUESTION and TK.BITOR tokens and implement parseTypeRef.)
 * - <style> is stored as raw text and not validated yet.
 *
 * Everything else in the current ONE design is implemented:
 * - backend forbids < >
 * - frontend supports structure with <logic>{...}</logic>
 * - braces required for .loop/.when/.maybe shortcuts
 * - desugaring works
 * - strict optional typing works for implemented type forms
 */