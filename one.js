#!/usr/bin/env node
"use strict";

/**
 * ONE v1.0 — single-file compiler
 * Backend: bytecode VM
 * Frontend: build -> file.html + file.js (DOM runtime)
 * Pipeline: Lexer -> Parser -> Analyzer -> Optimizer -> (VM or JS emit)
 *
 * CLI:
 *   node one.js run <file.one>
 *   node one.js build <file.one>
 *   node one.js fmt <file.one>
 *   node one.js serve <dir> [port]
 *   node one.js test
 *   node one.js init <dir>
 *
 * Modes:
 *   @backend (default) and @frontend
 *
 * Backend rule:
 *   - Angled tags like <div> or </div> are SYNTAX ERROR in @backend
 *   - Comparisons (a < b) are OK
 */

const fs = require("fs");
const path = require("path");
const http = require("http");

/* ============================================================
   Utilities
============================================================ */

function panic(msg, code = 1) { console.error(msg); process.exit(code); }

function readText(p) {
  try { return fs.readFileSync(p, "utf8"); }
  catch (e) { panic(`Could not read file: ${p}\n${String(e?.message || e)}`); }
}
function writeText(p, s) {
  try { fs.writeFileSync(p, s, "utf8"); }
  catch (e) { panic(`Could not write file: ${p}\n${String(e?.message || e)}`); }
}
function fileExists(p) { try { fs.accessSync(p); return true; } catch { return false; } }

function isWs(ch){ return ch===" "||ch==="\t"||ch==="\r"; }
function isNl(ch){ return ch==="\n"; }
function isDigit(ch){ return ch>="0"&&ch<="9"; }
function isAlpha(ch){ return (ch>="a"&&ch<="z")||(ch>="A"&&ch<="Z")||ch==="_"; }
function isIdentStart(ch){ return isAlpha(ch); }
function isIdentPart(ch){ return isIdentStart(ch)||isDigit(ch); }

function indexToLineCol(src, idx) {
  let line=1, col=1;
  for(let i=0;i<idx && i<src.length;i++){
    const ch=src[i];
    if(ch==="\n"){ line++; col=1; } else col++;
  }
  return {line,col};
}

function findHeaderMode(src) {
  const t = src.trimStart();
  if (t.startsWith("@frontend")) return "frontend";
  if (t.startsWith("@backend")) return "backend";
  return "backend";
}
function stripHeaderLine(src) { return src.replace(/^\s*@(?:backend|frontend)[^\n]*\n/, ""); }

// Backend: forbid markup-like tags <div ...> or </div>, but allow comparisons
function enforceBackendNoFrontendTags(fileName, srcRaw) {
  // find "<" then optional whitespace and optional "/" then optional whitespace then a letter
  const re = /<\s*\/?\s*[A-Za-z]/g;
  const m = re.exec(srcRaw);
  if (!m) return;
  const idx = m.index;
  const { line, col } = indexToLineCol(srcRaw, idx);
  throwErr(fileName, line, col, `Backend mode forbids angled tags. Found '${m[0]}...'. Use @frontend for markup.`);
}

/* ============================================================
   Error Model
============================================================ */

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

/* ============================================================
   Tokens + Lexer
============================================================ */

const TokenKind = Object.freeze({
  EOF:"EOF",
  NEWLINE:"NEWLINE",
  SEMI:"SEMI",

  IDENT:"IDENT",
  NUMBER:"NUMBER",
  STRING:"STRING",

  // keywords
  KW_LET:"KW_LET",
  KW_FN:"KW_FN",
  KW_IF:"KW_IF",
  KW_ELSE:"KW_ELSE",
  KW_WHILE:"KW_WHILE",
  KW_FOR:"KW_FOR",
  KW_BREAK:"KW_BREAK",
  KW_CONTINUE:"KW_CONTINUE",
  KW_RETURN:"KW_RETURN",
  KW_TRUE:"KW_TRUE",
  KW_FALSE:"KW_FALSE",
  KW_NULL:"KW_NULL",
  KW_IMPORT:"KW_IMPORT",
  KW_EXPORT:"KW_EXPORT",
  KW_TRY:"KW_TRY",
  KW_CATCH:"KW_CATCH",
  KW_THROW:"KW_THROW",

  // symbols
  LPAREN:"LPAREN",
  RPAREN:"RPAREN",
  LBRACE:"LBRACE",
  RBRACE:"RBRACE",
  COMMA:"COMMA",
  DOT:"DOT",
  COLON:"COLON",

  // ops
  EQ:"EQ",
  EQEQ:"EQEQ",
  BANGEQ:"BANGEQ",
  LT:"LT",
  LTE:"LTE",
  GT:"GT",
  GTE:"GTE",
  PLUS:"PLUS",
  MINUS:"MINUS",
  STAR:"STAR",
  SLASH:"SLASH",
  BANG:"BANG",
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
  ["try", TokenKind.KW_TRY],
  ["catch", TokenKind.KW_CATCH],
  ["throw", TokenKind.KW_THROW],
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
    this.src = src;
    this.i = 0;
    this.line = 1;
    this.col = 1;
  }
  peek(offset=0){
    const idx=this.i+offset;
    return idx>=0 && idx<this.src.length ? this.src[idx] : "\0";
  }
  advance(){
    const ch=this.peek();
    this.i++;
    if(ch==="\n"){ this.line++; this.col=1; } else this.col++;
    return ch;
  }
  make(kind,value,line,col){ return new Token(kind,value,line,col); }

  lexString(){
    const startLine=this.line, startCol=this.col;
    this.advance(); // "
    let out="";
    while(true){
      const ch=this.peek();
      if(ch==="\0") throwErr(this.fileName,startLine,startCol,"Unterminated string literal");
      if(ch==="\""){ this.advance(); break; }
      if(ch==="\\"){
        this.advance();
        const esc=this.peek();
        if(esc==="\0") throwErr(this.fileName,startLine,startCol,"Unterminated escape sequence");
        this.advance();
        switch(esc){
          case "n": out+="\n"; break;
          case "t": out+="\t"; break;
          case "\"": out+="\""; break;
          case "\\": out+="\\"; break;
          default: out+=esc; break;
        }
      } else out+=this.advance();
    }
    return this.make(TokenKind.STRING,out,startLine,startCol);
  }

  lexNumber(){
    const startLine=this.line, startCol=this.col;
    let s="";
    while(isDigit(this.peek())) s+=this.advance();
    if(this.peek()==="." && isDigit(this.peek(1))){
      s+=this.advance();
      while(isDigit(this.peek())) s+=this.advance();
    }
    return this.make(TokenKind.NUMBER,Number(s),startLine,startCol);
  }

  lexIdentOrKeyword(){
    const startLine=this.line, startCol=this.col;
    let s="";
    while(isIdentPart(this.peek())) s+=this.advance();
    const kw=KEYWORDS.get(s);
    if(kw) return this.make(kw,s,startLine,startCol);
    return this.make(TokenKind.IDENT,s,startLine,startCol);
  }

  nextToken(){
    while(true){
      const ch=this.peek();
      if(ch==="\0") return this.make(TokenKind.EOF,null,this.line,this.col);

      if(isWs(ch)){ this.advance(); continue; }
      if(ch==="/" && this.peek(1)==="/"){
        while(!isNl(this.peek()) && this.peek()!=="\0") this.advance();
        continue;
      }
      if(isNl(ch)){
        const line=this.line, col=this.col;
        this.advance();
        return this.make(TokenKind.NEWLINE,"\n",line,col);
      }
      if(ch===";"){
        const line=this.line,col=this.col;
        this.advance();
        return this.make(TokenKind.SEMI,";",line,col);
      }

      if(ch==="\"") return this.lexString();
      if(isDigit(ch)) return this.lexNumber();
      if(isIdentStart(ch)) return this.lexIdentOrKeyword();

      const line=this.line,col=this.col;
      if(ch==="=" && this.peek(1)==="="){ this.advance(); this.advance(); return this.make(TokenKind.EQEQ,"==",line,col); }
      if(ch==="!" && this.peek(1)==="="){ this.advance(); this.advance(); return this.make(TokenKind.BANGEQ,"!=",line,col); }
      if(ch==="<" && this.peek(1)==="="){ this.advance(); this.advance(); return this.make(TokenKind.LTE,"<=",line,col); }
      if(ch===">" && this.peek(1)==="="){ this.advance(); this.advance(); return this.make(TokenKind.GTE,">=",line,col); }

      switch(ch){
        case "(": this.advance(); return this.make(TokenKind.LPAREN,"(",line,col);
        case ")": this.advance(); return this.make(TokenKind.RPAREN,")",line,col);
        case "{": this.advance(); return this.make(TokenKind.LBRACE,"{",line,col);
        case "}": this.advance(); return this.make(TokenKind.RBRACE,"}",line,col);
        case ",": this.advance(); return this.make(TokenKind.COMMA,",",line,col);
        case ".": this.advance(); return this.make(TokenKind.DOT,".",line,col);
        case ":": this.advance(); return this.make(TokenKind.COLON,":",line,col);
        case "=": this.advance(); return this.make(TokenKind.EQ,"=",line,col);
        case "<": this.advance(); return this.make(TokenKind.LT,"<",line,col);
        case ">": this.advance(); return this.make(TokenKind.GT,">",line,col);
        case "+": this.advance(); return this.make(TokenKind.PLUS,"+",line,col);
        case "-": this.advance(); return this.make(TokenKind.MINUS,"-",line,col);
        case "*": this.advance(); return this.make(TokenKind.STAR,"*",line,col);
        case "/": this.advance(); return this.make(TokenKind.SLASH,"/",line,col);
        case "!": this.advance(); return this.make(TokenKind.BANG,"!",line,col);
        default:
          throwErr(this.fileName,line,col,`Unexpected character: '${ch}'`);
      }
    }
  }

  tokenizeAll(){
    const out=[];
    while(true){
      const t=this.nextToken();
      out.push(t);
      if(t.kind===TokenKind.EOF) break;
    }
    return out;
  }
}

/* ============================================================
   AST helpers
============================================================ */

function node(kind, props){ return Object.freeze({kind, ...props}); }

/* ============================================================
   Parser
============================================================ */

class Parser {
  constructor(fileName, tokens) { this.fileName=fileName; this.tokens=tokens; this.i=0; }
  cur(){ return this.tokens[this.i]; }
  at(k){ return this.cur().kind===k; }
  eat(k,msg){
    const t=this.cur();
    if(t.kind!==k) throwErr(this.fileName,t.line,t.col,msg||`Expected ${k} but got ${t.kind}`);
    this.i++; return t;
  }
  tryEat(k){ if(this.at(k)) return this.eat(k); return null; }
  eatTerms(){ while(this.at(TokenKind.NEWLINE)||this.at(TokenKind.SEMI)) this.i++; }
  requireTerm(msg){
    if(this.at(TokenKind.SEMI)||this.at(TokenKind.NEWLINE)){ this.eatTerms(); return; }
    if(this.at(TokenKind.RBRACE)||this.at(TokenKind.EOF)) return;
    const t=this.cur();
    throwErr(this.fileName,t.line,t.col,msg||"Expected end of statement (newline or ';')");
  }

  parseProgram(){
    const items=[];
    this.eatTerms();
    while(!this.at(TokenKind.EOF)){
      if(this.at(TokenKind.KW_EXPORT)) items.push(this.parseExportDecl());
      else if(this.at(TokenKind.KW_IMPORT)) items.push(this.parseImportDecl());
      else if(this.at(TokenKind.KW_FN)) items.push(this.parseFuncDecl(false));
      else items.push(this.parseStmt());
      this.eatTerms();
    }
    return node("Program",{items});
  }

  parseExportDecl(){
    const kw=this.eat(TokenKind.KW_EXPORT);
    if(this.at(TokenKind.KW_LET)) return this.parseLet(true, kw.line, kw.col);
    if(this.at(TokenKind.KW_FN)) return this.parseFuncDecl(true, kw.line, kw.col);
    throwErr(this.fileName,kw.line,kw.col,"Expected 'let' or 'fn' after export");
  }

  parseImportDecl(){
    const kw=this.eat(TokenKind.KW_IMPORT);
    const p=this.eat(TokenKind.STRING,"Expected string after import");
    this.requireTerm("Expected end of statement after import");
    return node("ImportDecl",{line:kw.line,col:kw.col,path:p.value});
  }

  parseFuncDecl(exported, exportLine=null, exportCol=null){
    const kw=this.eat(TokenKind.KW_FN);
    const nameTok=this.eat(TokenKind.IDENT,"Expected function name");
    this.eat(TokenKind.LPAREN,"Expected '(' after function name");
    const params=[];
    if(!this.at(TokenKind.RPAREN)){
      while(true){
        const pt=this.cur();
        const pName=this.eat(TokenKind.IDENT,"Expected parameter name").value;
        let pType=null;
        if(this.tryEat(TokenKind.COLON)) pType=this.parseTypeRef();
        params.push({name:pName,typeRef:pType,line:pt.line,col:pt.col});
        if(this.tryEat(TokenKind.COMMA)) continue;
        break;
      }
    }
    this.eat(TokenKind.RPAREN,"Expected ')' after parameters");
    let returnType=null;
    if(this.tryEat(TokenKind.COLON)) returnType=this.parseTypeRef();
    const body=this.parseBlock();
    return node("FuncDecl",{line:kw.line,col:kw.col,name:nameTok.value,params,returnType,body,exported:!!exported,exportLine,exportCol});
  }

  parseTypeRef(){
    const t=this.cur();
    const id=this.eat(TokenKind.IDENT,"Expected type name");
    return {name:id.value,line:t.line,col:t.col};
  }

  parseStmt(){
    if(this.at(TokenKind.KW_LET)) return this.parseLet(false);
    if(this.at(TokenKind.KW_IF)) return this.parseIf();
    if(this.at(TokenKind.KW_WHILE)) return this.parseWhile();
    if(this.at(TokenKind.KW_FOR)) return this.parseFor();
    if(this.at(TokenKind.KW_BREAK)) { const t=this.eat(TokenKind.KW_BREAK); this.requireTerm(); return node("BreakStmt",{line:t.line,col:t.col}); }
    if(this.at(TokenKind.KW_CONTINUE)) { const t=this.eat(TokenKind.KW_CONTINUE); this.requireTerm(); return node("ContinueStmt",{line:t.line,col:t.col}); }
    if(this.at(TokenKind.KW_RETURN)) return this.parseReturn();
    if(this.at(TokenKind.KW_TRY)) return this.parseTryCatch();
    if(this.at(TokenKind.KW_THROW)) return this.parseThrow();
    if(this.at(TokenKind.LBRACE)) return this.parseBlock();

    // assignment / expr stmt (supports obj.prop = ...)
    const save=this.i;
    const lv=this.tryParseLValue();
    if(lv && this.at(TokenKind.EQ)){
      const eq=this.eat(TokenKind.EQ);
      const value=this.parseExpr();
      this.requireTerm("Expected end of statement after assignment");
      return node("AssignStmt",{line:eq.line,col:eq.col,target:lv,value});
    }
    this.i=save;

    const expr=this.parseExpr();
    this.requireTerm("Expected end of statement after expression");
    return node("ExprStmt",{line:expr.line,col:expr.col,expr});
  }

  parseLet(exported, exportLine=null, exportCol=null){
    const kw=this.eat(TokenKind.KW_LET);
    const nameTok=this.eat(TokenKind.IDENT,"Expected identifier after let");
    let typeRef=null;
    if(this.tryEat(TokenKind.COLON)) typeRef=this.parseTypeRef();
    this.eat(TokenKind.EQ,"Expected '=' in let");
    const value=this.parseExpr();
    this.requireTerm("Expected end of statement after let");
    return node("LetStmt",{line:kw.line,col:kw.col,name:nameTok.value,typeRef,value,exported:!!exported,exportLine,exportCol});
  }

  parseIf(){
    const kw=this.eat(TokenKind.KW_IF);
    this.eat(TokenKind.LPAREN,"Expected '(' after if");
    const cond=this.parseExpr();
    this.eat(TokenKind.RPAREN,"Expected ')' after condition");
    const thenBlock=this.parseBlock();
    let elseBlock=null;
    if(this.tryEat(TokenKind.KW_ELSE)) elseBlock=this.parseBlock();
    return node("IfStmt",{line:kw.line,col:kw.col,cond,thenBlock,elseBlock});
  }

  parseWhile(){
    const kw=this.eat(TokenKind.KW_WHILE);
    this.eat(TokenKind.LPAREN,"Expected '(' after while");
    const cond=this.parseExpr();
    this.eat(TokenKind.RPAREN,"Expected ')'");
    const body=this.parseBlock();
    return node("WhileStmt",{line:kw.line,col:kw.col,cond,body});
  }

  parseFor(){
    const kw=this.eat(TokenKind.KW_FOR);
    this.eat(TokenKind.LPAREN,"Expected '(' after for");
    // init: let/assign/expr/empty, then ';'
    let init=null;
    if(!this.at(TokenKind.SEMI)){
      if(this.at(TokenKind.KW_LET)) init=this.parseLet(false);
      else {
        // parse as assignment or expr but require semicolon as terminator inside for
        const save=this.i;
        const lv=this.tryParseLValue();
        if(lv && this.at(TokenKind.EQ)){
          const eq=this.eat(TokenKind.EQ);
          const v=this.parseExpr();
          init=node("AssignStmt",{line:eq.line,col:eq.col,target:lv,value:v});
        } else {
          this.i=save;
          const e=this.parseExpr();
          init=node("ExprStmt",{line:e.line,col:e.col,expr:e});
        }
      }
    }
    this.eat(TokenKind.SEMI,"Expected ';' after for init");

    let cond=null;
    if(!this.at(TokenKind.SEMI)) cond=this.parseExpr();
    this.eat(TokenKind.SEMI,"Expected ';' after for condition");

    let update=null;
    if(!this.at(TokenKind.RPAREN)){
      const save=this.i;
      const lv=this.tryParseLValue();
      if(lv && this.at(TokenKind.EQ)){
        const eq=this.eat(TokenKind.EQ);
        const v=this.parseExpr();
        update=node("AssignStmt",{line:eq.line,col:eq.col,target:lv,value:v});
      } else {
        this.i=save;
        const e=this.parseExpr();
        update=node("ExprStmt",{line:e.line,col:e.col,expr:e});
      }
    }
    this.eat(TokenKind.RPAREN,"Expected ')' after for");
    const body=this.parseBlock();
    return node("ForStmt",{line:kw.line,col:kw.col,init,cond,update,body});
  }

  parseReturn(){
    const kw=this.eat(TokenKind.KW_RETURN);
    if(this.at(TokenKind.NEWLINE)||this.at(TokenKind.SEMI)||this.at(TokenKind.RBRACE)||this.at(TokenKind.EOF)){
      this.eatTerms();
      return node("ReturnStmt",{line:kw.line,col:kw.col,expr:null});
    }
    const expr=this.parseExpr();
    this.requireTerm("Expected end of statement after return");
    return node("ReturnStmt",{line:kw.line,col:kw.col,expr});
  }

  parseThrow(){
    const kw=this.eat(TokenKind.KW_THROW);
    const expr=this.parseExpr();
    this.requireTerm("Expected end of statement after throw");
    return node("ThrowStmt",{line:kw.line,col:kw.col,expr});
  }

  parseTryCatch(){
    const kw=this.eat(TokenKind.KW_TRY);
    const tryBlock=this.parseBlock();
    this.eat(TokenKind.KW_CATCH,"Expected catch after try");
    this.eat(TokenKind.LPAREN,"Expected '(' after catch");
    const id=this.eat(TokenKind.IDENT,"Expected catch identifier").value;
    this.eat(TokenKind.RPAREN,"Expected ')'");
    const catchBlock=this.parseBlock();
    return node("TryCatchStmt",{line:kw.line,col:kw.col,catchName:id,tryBlock,catchBlock});
  }

  parseBlock(){
    const lb=this.eat(TokenKind.LBRACE,"Expected '{'");
    this.eatTerms();
    const statements=[];
    while(!this.at(TokenKind.RBRACE)){
      if(this.at(TokenKind.EOF)) throwErr(this.fileName,lb.line,lb.col,"Unterminated block (missing '}')");
      statements.push(this.parseStmt());
      this.eatTerms();
    }
    this.eat(TokenKind.RBRACE,"Expected '}'");
    return node("Block",{line:lb.line,col:lb.col,statements});
  }

  // LValue: ident(.ident)*
  tryParseLValue(){
    if(!this.at(TokenKind.IDENT)) return null;
    const t=this.cur();
    let parts=[this.eat(TokenKind.IDENT).value];
    while(this.tryEat(TokenKind.DOT)){
      parts.push(this.eat(TokenKind.IDENT,"Expected identifier after '.'").value);
    }
    return node("LValue",{line:t.line,col:t.col,parts});
  }

  /* ---------------- Expressions (precedence) ---------------- */

  parseExpr(){ return this.parseEquality(); }

  parseEquality(){
    let e=this.parseComparison();
    while(this.at(TokenKind.EQEQ)||this.at(TokenKind.BANGEQ)){
      const op=this.cur(); this.i++;
      const r=this.parseComparison();
      e=node("Binary",{line:op.line,col:op.col,op:op.kind,left:e,right:r});
    }
    return e;
  }

  parseComparison(){
    let e=this.parseTerm();
    while(this.at(TokenKind.LT)||this.at(TokenKind.LTE)||this.at(TokenKind.GT)||this.at(TokenKind.GTE)){
      const op=this.cur(); this.i++;
      const r=this.parseTerm();
      e=node("Binary",{line:op.line,col:op.col,op:op.kind,left:e,right:r});
    }
    return e;
  }

  parseTerm(){
    let e=this.parseFactor();
    while(this.at(TokenKind.PLUS)||this.at(TokenKind.MINUS)){
      const op=this.cur(); this.i++;
      const r=this.parseFactor();
      e=node("Binary",{line:op.line,col:op.col,op:op.kind,left:e,right:r});
    }
    return e;
  }

  parseFactor(){
    let e=this.parseUnary();
    while(this.at(TokenKind.STAR)||this.at(TokenKind.SLASH)){
      const op=this.cur(); this.i++;
      const r=this.parseUnary();
      e=node("Binary",{line:op.line,col:op.col,op:op.kind,left:e,right:r});
    }
    return e;
  }

  parseUnary(){
    if(this.at(TokenKind.BANG)||this.at(TokenKind.MINUS)){
      const op=this.cur(); this.i++;
      const e=this.parseUnary();
      return node("Unary",{line:op.line,col:op.col,op:op.kind,expr:e});
    }
    return this.parseCall();
  }

  parseCall(){
    let e=this.parsePrimary();
    while(true){
      if(this.at(TokenKind.LPAREN)){
        const lp=this.eat(TokenKind.LPAREN);
        const args=[];
        if(!this.at(TokenKind.RPAREN)){
          while(true){
            args.push(this.parseExpr());
            if(this.tryEat(TokenKind.COMMA)) continue;
            break;
          }
        }
        this.eat(TokenKind.RPAREN,"Expected ')'");
        e=node("Call",{line:lp.line,col:lp.col,callee:e,args});
        continue;
      }
      if(this.tryEat(TokenKind.DOT)){
        const p=this.eat(TokenKind.IDENT,"Expected property after '.'");
        e=node("Member",{line:p.line,col:p.col,object:e,prop:p.value});
        continue;
      }
      break;
    }
    return e;
  }

  parsePrimary(){
    const t=this.cur();

    if(this.at(TokenKind.NUMBER)){ this.i++; return node("Literal",{line:t.line,col:t.col,value:t.value,litType:"number"}); }
    if(this.at(TokenKind.STRING)){ this.i++; return node("Literal",{line:t.line,col:t.col,value:t.value,litType:"string"}); }
    if(this.at(TokenKind.KW_TRUE)){ this.i++; return node("Literal",{line:t.line,col:t.col,value:true,litType:"bool"}); }
    if(this.at(TokenKind.KW_FALSE)){ this.i++; return node("Literal",{line:t.line,col:t.col,value:false,litType:"bool"}); }
    if(this.at(TokenKind.KW_NULL)){ this.i++; return node("Literal",{line:t.line,col:t.col,value:null,litType:"null"}); }

    if(this.at(TokenKind.IDENT)){ this.i++; return node("Var",{line:t.line,col:t.col,name:t.value}); }

    // list: tuple syntax (a, b, c) or () empty list
    if(this.at(TokenKind.LPAREN)){
      const lp=this.eat(TokenKind.LPAREN);
      if(this.at(TokenKind.RPAREN)){ this.eat(TokenKind.RPAREN); return node("List",{line:lp.line,col:lp.col,items:[]}); }
      const first=this.parseExpr();
      if(this.tryEat(TokenKind.COMMA)){
        const items=[first];
        while(true){
          items.push(this.parseExpr());
          if(this.tryEat(TokenKind.COMMA)) continue;
          break;
        }
        this.eat(TokenKind.RPAREN,"Expected ')'");
        return node("List",{line:lp.line,col:lp.col,items});
      }
      this.eat(TokenKind.RPAREN,"Expected ')'");
      return first;
    }

    // object literal: { key: expr, ... }
    if(this.at(TokenKind.LBRACE)){
      const lb=this.eat(TokenKind.LBRACE);
      const pairs=[];
      // allow empty: {}
      if(!this.at(TokenKind.RBRACE)){
        while(true){
          // key is identifier or string
          let keyTok=this.cur();
          let key=null;
          if(this.at(TokenKind.IDENT)){ key=this.eat(TokenKind.IDENT).value; }
          else if(this.at(TokenKind.STRING)){ key=this.eat(TokenKind.STRING).value; }
          else throwErr(this.fileName,keyTok.line,keyTok.col,"Expected object key (identifier or string)");
          this.eat(TokenKind.COLON,"Expected ':' after object key");
          const val=this.parseExpr();
          pairs.push({key,val,line:keyTok.line,col:keyTok.col});
          if(this.tryEat(TokenKind.COMMA)) continue;
          break;
        }
      }
      this.eat(TokenKind.RBRACE,"Expected '}' to close object literal");
      return node("ObjectLit",{line:lb.line,col:lb.col,pairs});
    }

    if(this.at(TokenKind.EQ)) throwErr(this.fileName,t.line,t.col,"Unexpected '=' in expression. Did you mean '=='?");
    throwErr(this.fileName,t.line,t.col,`Unexpected token in expression: ${t.kind}`);
  }
}

/* ============================================================
   Types + Runtime helpers
============================================================ */

function typeOf(v){
  if(v===null) return "null";
  if(Array.isArray(v)) return "list";
  switch(typeof v){
    case "number": return "number";
    case "string": return "string";
    case "boolean": return "bool";
    case "object": return "object";
    case "function": return "function";
    default: return "unknown";
  }
}

function assertTypeAt(file,line,col,expected,v){
  const a=typeOf(v);
  if(a!==expected) throwErr(file,line,col,`Type error: expected ${expected} but got ${a}`);
}

function isTruthy(v){
  // ONE uses strict bool for conditions
  return v===true;
}

/* ============================================================
   Analyzer (declarations, scope, hoisting, type annotations)
============================================================ */

function analyzeProgram(fileName, program){
  // enforce: declared before use (vars), function decl hoisted
  // block scope, catch variable scope, loop scopes
  // exports top-level only
  const errors=[];

  function note(err){ errors.push(err); }

  function scope(parent){
    return { parent, vars:new Map(), funcs:new Map(), exported:new Set() };
  }

  function declareVar(sc, name, nodeRef){
    if(sc.vars.has(name)||sc.funcs.has(name)) note({node:nodeRef,msg:`Redeclaration: '${name}' already defined`});
    sc.vars.set(name, nodeRef);
  }

  function declareFunc(sc, name, nodeRef){
    if(sc.vars.has(name)||sc.funcs.has(name)) note({node:nodeRef,msg:`Redeclaration: '${name}' already defined`});
    sc.funcs.set(name, nodeRef);
  }

  function resolve(sc, name){
    let cur=sc;
    while(cur){
      if(cur.vars.has(name)) return {kind:"var",decl:cur.vars.get(name)};
      if(cur.funcs.has(name)) return {kind:"func",decl:cur.funcs.get(name)};
      cur=cur.parent;
    }
    return null;
  }

  function walkExpr(sc, e){
    switch(e.kind){
      case "Literal": return;
      case "Var":{
        const r=resolve(sc,e.name);
        if(!r) note({node:e,msg:`Undefined identifier: ${e.name}`});
        return;
      }
      case "List": e.items.forEach(x=>walkExpr(sc,x)); return;
      case "ObjectLit": e.pairs.forEach(p=>walkExpr(sc,p.val)); return;
      case "Unary": walkExpr(sc,e.expr); return;
      case "Binary": walkExpr(sc,e.left); walkExpr(sc,e.right); return;
      case "Member": walkExpr(sc,e.object); return;
      case "Call": walkExpr(sc,e.callee); e.args.forEach(a=>walkExpr(sc,a)); return;
      default: return;
    }
  }

  function walkStmt(sc, st){
    switch(st.kind){
      case "ImportDecl": return;
      case "LetStmt":
        walkExpr(sc, st.value);
        declareVar(sc, st.name, st);
        return;
      case "AssignStmt":
        // target parts: allow obj.prop or var
        // but must exist
        if(st.target.parts.length===1){
          const nm=st.target.parts[0];
          const r=resolve(sc,nm);
          if(!r || r.kind!=="var") note({node:st,msg:`Assignment to undefined variable: ${nm}`});
        } else {
          // base var must exist
          const base=st.target.parts[0];
          const r=resolve(sc,base);
          if(!r) note({node:st,msg:`Undefined identifier: ${base}`});
        }
        walkExpr(sc, st.value);
        return;
      case "ExprStmt": walkExpr(sc, st.expr); return;
      case "IfStmt":{
        walkExpr(sc, st.cond);
        walkBlock(newScope(sc), st.thenBlock);
        if(st.elseBlock) walkBlock(newScope(sc), st.elseBlock);
        return;
      }
      case "WhileStmt":{
        walkExpr(sc, st.cond);
        walkBlock(newScope(sc), st.body);
        return;
      }
      case "ForStmt":{
        const inner=newScope(sc);
        if(st.init) walkStmt(inner, st.init);
        if(st.cond) walkExpr(inner, st.cond);
        if(st.update) walkStmt(inner, st.update);
        walkBlock(newScope(inner), st.body);
        return;
      }
      case "BreakStmt":
      case "ContinueStmt":
        return;
      case "ReturnStmt":
        if(st.expr) walkExpr(sc, st.expr);
        return;
      case "ThrowStmt":
        walkExpr(sc, st.expr);
        return;
      case "TryCatchStmt":{
        walkBlock(newScope(sc), st.tryBlock);
        const csc=newScope(sc);
        declareVar(csc, st.catchName, st);
        walkBlock(csc, st.catchBlock);
        return;
      }
      case "Block":
        walkBlock(newScope(sc), st);
        return;
      default:
        return;
    }
  }

  function newScope(p){ return scope(p); }

  function walkBlock(sc, block){
    // functions are hoisted within their containing scope (block or program)
    // but vars not hoisted
    // First pass: hoist func decls in this block
    for(const s of block.statements){
      // no nested FuncDecl nodes as statements in our grammar (fn only top-level)
      // so nothing here
    }
    for(const s of block.statements) walkStmt(sc,s);
  }

  // Program-level scope
  const g=scope(null);

  // Hoist top-level functions first
  for(const it of program.items){
    if(it.kind==="FuncDecl") declareFunc(g,it.name,it);
  }
  // Walk all top-level items in order (vars must be declared before use)
  for(const it of program.items){
    if(it.kind==="FuncDecl"){
      // analyze function body with params scope
      const fsc=scope(g);
      // params are vars
      for(const p of it.params) declareVar(fsc,p.name,it);
      // NOTE: allow recursion because function already hoisted in g
      walkBlock(scope(fsc), it.body);
    } else {
      walkStmt(g, it);
    }
  }

  // validate exports top-level only (parser enforces), also ensure exported names exist
  for(const it of program.items){
    if((it.kind==="LetStmt"||it.kind==="FuncDecl") && it.exported){
      g.exported.add(it.name);
    }
  }

  if(errors.length){
    const first=errors[0];
    const n=first.node;
    throwErr(fileName, n.line||1, n.col||1, first.msg);
  }

  return { exports:[...g.exported] };
}

/* ============================================================
   Optimizer: constant folding + DCE + return-based DCE
============================================================ */

function isConstExpr(e){
  if(e.kind==="Literal") return true;
  if(e.kind==="Unary") return isConstExpr(e.expr);
  if(e.kind==="Binary") return isConstExpr(e.left) && isConstExpr(e.right);
  if(e.kind==="List") return e.items.every(isConstExpr);
  if(e.kind==="ObjectLit") return e.pairs.every(p=>isConstExpr(p.val));
  return false;
}

function evalConstExpr(e){
  switch(e.kind){
    case "Literal": return e.value;
    case "List": return e.items.map(evalConstExpr);
    case "ObjectLit":{
      const o=Object.create(null);
      for(const p of e.pairs) o[p.key]=evalConstExpr(p.val);
      return o;
    }
    case "Unary":{
      const v=evalConstExpr(e.expr);
      if(e.op===TokenKind.MINUS){ if(typeOf(v)!=="number") return undefined; return -v; }
      if(e.op===TokenKind.BANG){ if(typeOf(v)!=="bool") return undefined; return !v; }
      return undefined;
    }
    case "Binary":{
      const a=evalConstExpr(e.left), b=evalConstExpr(e.right);
      const ta=typeOf(a), tb=typeOf(b);
      switch(e.op){
        case TokenKind.PLUS:
          if(ta==="number"&&tb==="number") return a+b;
          if(ta==="string"&&tb==="string") return a+b;
          return undefined;
        case TokenKind.MINUS: if(ta==="number"&&tb==="number") return a-b; return undefined;
        case TokenKind.STAR: if(ta==="number"&&tb==="number") return a*b; return undefined;
        case TokenKind.SLASH: if(ta==="number"&&tb==="number") return a/b; return undefined;
        case TokenKind.EQEQ: if(ta!==tb) return false; return a===b;
        case TokenKind.BANGEQ: if(ta!==tb) return true; return a!==b;
        case TokenKind.LT:
        case TokenKind.LTE:
        case TokenKind.GT:
        case TokenKind.GTE:{
          if((ta==="number"&&tb==="number")||(ta==="string"&&tb==="string")){
            if(e.op===TokenKind.LT) return a<b;
            if(e.op===TokenKind.LTE) return a<=b;
            if(e.op===TokenKind.GT) return a>b;
            return a>=b;
          }
          return undefined;
        }
        default: return undefined;
      }
    }
    default: return undefined;
  }
}

function foldExpr(e){
  switch(e.kind){
    case "Literal": return e;
    case "Var": return e;
    case "List":{
      const items=e.items.map(foldExpr);
      const ne=node("List",{...e,items});
      if(isConstExpr(ne)){
        const v=evalConstExpr(ne);
        if(v!==undefined) return node("Literal",{line:e.line,col:e.col,value:v,litType:"listConst"});
      }
      return ne;
    }
    case "ObjectLit":{
      const pairs=e.pairs.map(p=>({ ...p, val: foldExpr(p.val) }));
      const ne=node("ObjectLit",{...e,pairs});
      if(isConstExpr(ne)){
        const v=evalConstExpr(ne);
        if(v!==undefined) return node("Literal",{line:e.line,col:e.col,value:v,litType:"objectConst"});
      }
      return ne;
    }
    case "Unary":{
      const expr=foldExpr(e.expr);
      const ne=node("Unary",{...e,expr});
      if(isConstExpr(ne)){
        const v=evalConstExpr(ne);
        if(v!==undefined){
          return node("Literal",{line:e.line,col:e.col,value:v,litType:typeof v});
        }
      }
      return ne;
    }
    case "Binary":{
      const left=foldExpr(e.left), right=foldExpr(e.right);
      const ne=node("Binary",{...e,left,right});
      if(isConstExpr(ne)){
        const v=evalConstExpr(ne);
        if(v!==undefined){
          return node("Literal",{line:e.line,col:e.col,value:v,litType:typeof v});
        }
      }
      return ne;
    }
    case "Member": return node("Member",{...e,object:foldExpr(e.object)});
    case "Call": return node("Call",{...e,callee:foldExpr(e.callee),args:e.args.map(foldExpr)});
    default: return e;
  }
}

function dceBlock(block){
  const out=[];
  let live=true;
  for(const st of block.statements){
    if(!live) continue;

    // optimize inside
    const opt=optStmt(st);
    if(!opt) continue; // removed
    out.push(opt);

    if(opt.kind==="ReturnStmt"||opt.kind==="ThrowStmt"||opt.kind==="BreakStmt"||opt.kind==="ContinueStmt"){
      live=false; // return-based DCE
    }
  }
  return node("Block",{...block,statements:out});
}

function optStmt(st){
  switch(st.kind){
    case "LetStmt": return node("LetStmt",{...st,value:foldExpr(st.value)});
    case "AssignStmt": return node("AssignStmt",{...st,value:foldExpr(st.value)});
    case "ExprStmt":{
      const e=foldExpr(st.expr);
      // remove pure literal expression statements
      if(e.kind==="Literal") return null;
      return node("ExprStmt",{...st,expr:e});
    }
    case "IfStmt":{
      const cond=foldExpr(st.cond);
      const thenBlock=dceBlock(st.thenBlock);
      const elseBlock=st.elseBlock?dceBlock(st.elseBlock):null;
      // if cond is literal bool, pick branch
      if(cond.kind==="Literal" && typeof cond.value==="boolean"){
        return cond.value ? node("Block",{...thenBlock}) : (elseBlock? node("Block",{...elseBlock}) : null);
      }
      return node("IfStmt",{...st,cond,thenBlock,elseBlock});
    }
    case "WhileStmt":{
      const cond=foldExpr(st.cond);
      const body=dceBlock(st.body);
      // while(false) -> removed
      if(cond.kind==="Literal" && cond.value===false) return null;
      return node("WhileStmt",{...st,cond,body});
    }
    case "ForStmt":{
      const init=st.init?optStmt(st.init):null;
      const cond=st.cond?foldExpr(st.cond):null;
      const update=st.update?optStmt(st.update):null;
      const body=dceBlock(st.body);
      // for(...; false; ...) -> init only
      if(cond && cond.kind==="Literal" && cond.value===false){
        return init ? init : null;
      }
      return node("ForStmt",{...st,init,cond,update,body});
    }
    case "ReturnStmt": return node("ReturnStmt",{...st,expr:st.expr?foldExpr(st.expr):null});
    case "ThrowStmt": return node("ThrowStmt",{...st,expr:foldExpr(st.expr)});
    case "TryCatchStmt":{
      return node("TryCatchStmt",{...st,tryBlock:dceBlock(st.tryBlock),catchBlock:dceBlock(st.catchBlock)});
    }
    case "Block": return dceBlock(st);
    default: return st;
  }
}

function optimizeProgram(program){
  const items=[];
  for(const it of program.items){
    if(it.kind==="FuncDecl"){
      const body=dceBlock(it.body);
      items.push(node("FuncDecl",{...it,body}));
    } else {
      const o=optStmt(it);
      if(o){
        // If optStmt returns a Block, splice it into top-level
        if(o.kind==="Block") items.push(...o.statements);
        else items.push(o);
      }
    }
  }
  return node("Program",{items});
}

/* ============================================================
   Bytecode + VM
============================================================ */

const OP = Object.freeze({
  PUSH_CONST:"PUSH_CONST",
  LOAD:"LOAD",           // name
  STORE:"STORE",         // name
  DEF:"DEF",             // name (define local)
  POP:"POP",

  OBJ_NEW:"OBJ_NEW",
  OBJ_SET:"OBJ_SET",     // key
  OBJ_GET:"OBJ_GET",     // key
  LIST_NEW:"LIST_NEW",   // n
  LIST_GET:"LIST_GET",
  LIST_SET:"LIST_SET",
  LIST_LEN:"LIST_LEN",

  ADD:"ADD", SUB:"SUB", MUL:"MUL", DIV:"DIV",
  EQ:"EQ", NEQ:"NEQ",
  LT:"LT", LTE:"LTE", GT:"GT", GTE:"GTE",
  NEG:"NEG", NOT:"NOT",

  JMP:"JMP",             // addr
  JMPF:"JMPF",           // addr (requires bool true/false)
  TRY:"TRY",             // catchAddr
  ENDTRY:"ENDTRY",
  THROW:"THROW",

  CALL:"CALL",           // argc
  RET:"RET",

  ENTER:"ENTER",         // no-op marker
  LEAVE:"LEAVE",

  BREAK:"BREAK",
  CONTINUE:"CONTINUE",
});

class Env {
  constructor(parent=null){
    this.parent=parent;
    this.vars=Object.create(null);
    this.types=Object.create(null); // name -> type string (annotation)
  }
  hasLocal(n){ return Object.prototype.hasOwnProperty.call(this.vars,n); }
  define(file,line,col,n,v,annotType=null){
    if(this.hasLocal(n)) throwErr(file,line,col,`Redeclaration: '${n}' already defined in this scope`);
    this.vars[n]=v;
    if(annotType) this.types[n]=annotType;
  }
  set(file,line,col,n,v){
    if(Object.prototype.hasOwnProperty.call(this.vars,n)){
      this.vars[n]=v;
      return true;
    }
    if(this.parent) return this.parent.set(file,line,col,n,v);
    throwErr(file,line,col,`Assignment to undefined variable: ${n}`);
  }
  get(file,line,col,n){
    if(Object.prototype.hasOwnProperty.call(this.vars,n)) return this.vars[n];
    if(this.parent) return this.parent.get(file,line,col,n);
    throwErr(file,line,col,`Undefined identifier: ${n}`);
  }
  getAnnot(n){
    if(Object.prototype.hasOwnProperty.call(this.types,n)) return this.types[n];
    if(this.parent) return this.parent.getAnnot(n);
    return null;
  }
}

class ReturnSignal { constructor(v){this.value=v;} }
class BreakSignal {}
class ContinueSignal {}
class ThrowSignal { constructor(v){this.value=v;} }

function makeBuiltinPrint(){
  return (...args)=>{
    console.log(args.map(a=>{
      if(a===null) return "null";
      if(Array.isArray(a)) return `(${a.map(String).join(", ")})`;
      if(typeof a==="object") return JSON.stringify(a);
      return String(a);
    }).join(" "));
    return null;
  };
}

function makeBuiltinLoad(currentDir){
  return (p)=>{
    if(typeof p!=="string") throw new Error("load() expects string path");
    const abs = path.isAbsolute(p) ? p : path.resolve(currentDir, p);
    return readText(abs);
  };
}

function makeBuiltinTypeof(){
  return (v)=>typeOf(v);
}

class Chunk {
  constructor(name){
    this.name=name;
    this.code=[];      // [{op,arg,line,col}]
    this.consts=[];    // constants pool
  }
  c(v){ // constant index
    const i=this.consts.length;
    this.consts.push(v);
    return i;
  }
  emit(op,arg=null,line=0,col=0){ this.code.push({op,arg,line,col}); return this.code.length-1; }
  patch(at, newArg){ this.code[at].arg=newArg; }
}

function compileProgramToChunk(fileName, program, analysis){
  const chunk=new Chunk(fileName);

  // function table: name -> compiled function object
  const funcObjs=Object.create(null);

  // First compile all top-level functions to closures (capturing global env at runtime)
  for(const it of program.items){
    if(it.kind!=="FuncDecl") continue;
    funcObjs[it.name] = compileFunc(fileName, it);
  }

  // Emit top-level code: define builtins + compiled functions + statements
  // Builtins installed at runtime, functions defined into env before executing statements.
  chunk.emit(OP.ENTER,null,1,1);
  // (Top-level body compiled as a pseudo-function)
  for(const it of program.items){
    if(it.kind==="FuncDecl") continue;
    compileStmt(chunk, fileName, it);
  }
  chunk.emit(OP.PUSH_CONST, chunk.c(null), 1, 1);
  chunk.emit(OP.RET,null,1,1);

  return { chunk, funcObjs, exports: analysis.exports };
}

function compileFunc(fileName, fnDecl){
  const chunk=new Chunk(`fn:${fnDecl.name}`);
  chunk.emit(OP.ENTER,null,fnDecl.line,fnDecl.col);

  // function body
  compileBlock(chunk, fileName, fnDecl.body);

  // default return null
  chunk.emit(OP.PUSH_CONST, chunk.c(null), fnDecl.line, fnDecl.col);
  chunk.emit(OP.RET,null,fnDecl.line,fnDecl.col);

  return {
    __one_fn: true,
    name: fnDecl.name,
    params: fnDecl.params.map(p=>({name:p.name,type:p.typeRef?p.typeRef.name:null,line:p.line,col:p.col})),
    returnType: fnDecl.returnType?fnDecl.returnType.name:null,
    chunk
  };
}

function compileBlock(chunk, fileName, block){
  for(const st of block.statements) compileStmt(chunk,fileName,st);
}

function compileStmt(chunk, fileName, st){
  switch(st.kind){
    case "ImportDecl":
      // statement import: call import(path), discard return
      chunk.emit(OP.LOAD, "import", st.line, st.col);
      chunk.emit(OP.PUSH_CONST, chunk.c(st.path), st.line, st.col);
      chunk.emit(OP.CALL, 1, st.line, st.col);
      chunk.emit(OP.POP, null, st.line, st.col);
      return;

    case "LetStmt":{
      compileExpr(chunk,fileName,st.value);
      chunk.emit(OP.DEF, {name:st.name, type: st.typeRef?st.typeRef.name:null}, st.line, st.col);
      return;
    }

    case "AssignStmt":{
      const parts=st.target.parts;
      if(parts.length===1){
        compileExpr(chunk,fileName,st.value);
        chunk.emit(OP.STORE, {name:parts[0]}, st.line, st.col);
        return;
      }
      // obj.prop = value
      // compile base object
      chunk.emit(OP.LOAD, parts[0], st.line, st.col);
      // traverse intermediate props
      for(let i=1;i<parts.length-1;i++){
        chunk.emit(OP.OBJ_GET, parts[i], st.line, st.col);
      }
      // value
      compileExpr(chunk,fileName,st.value);
      // set last prop
      chunk.emit(OP.OBJ_SET, parts[parts.length-1], st.line, st.col);
      // obj_set returns assigned value; pop it (assignment statement)
      chunk.emit(OP.POP,null,st.line,st.col);
      return;
    }

    case "ExprStmt":
      compileExpr(chunk,fileName,st.expr);
      chunk.emit(OP.POP,null,st.line,st.col);
      return;

    case "IfStmt":{
      compileExpr(chunk,fileName,st.cond);
      const jf=chunk.emit(OP.JMPF, null, st.line, st.col);
      compileBlock(chunk,fileName,st.thenBlock);
      const jend=chunk.emit(OP.JMP, null, st.line, st.col);
      chunk.patch(jf, chunk.code.length);
      if(st.elseBlock) compileBlock(chunk,fileName,st.elseBlock);
      chunk.patch(jend, chunk.code.length);
      return;
    }

    case "WhileStmt":{
      const start=chunk.code.length;
      compileExpr(chunk,fileName,st.cond);
      const jf=chunk.emit(OP.JMPF,null,st.line,st.col);
      tryCompileLoopBody(chunk,fileName,st.body,start, null);
      chunk.emit(OP.JMP,start,st.line,st.col);
      chunk.patch(jf, chunk.code.length);
      return;
    }

    case "ForStmt":{
      // init
      if(st.init) compileStmt(chunk,fileName,st.init);
      const start=chunk.code.length;
      // cond
      if(st.cond){
        compileExpr(chunk,fileName,st.cond);
      } else {
        // default true
        chunk.emit(OP.PUSH_CONST, chunk.c(true), st.line, st.col);
      }
      const jf=chunk.emit(OP.JMPF,null,st.line,st.col);
      // body
      // update target addr
      const updateAnchor = { addr: null };
      tryCompileLoopBody(chunk,fileName,st.body,start, updateAnchor);
      // update
      updateAnchor.addr = chunk.code.length;
      if(st.update) compileStmt(chunk,fileName,st.update);
      chunk.emit(OP.JMP,start,st.line,st.col);
      chunk.patch(jf, chunk.code.length);
      return;
    }

    case "BreakStmt":
      chunk.emit(OP.BREAK,null,st.line,st.col);
      return;

    case "ContinueStmt":
      chunk.emit(OP.CONTINUE,null,st.line,st.col);
      return;

    case "ReturnStmt":
      if(st.expr) compileExpr(chunk,fileName,st.expr);
      else chunk.emit(OP.PUSH_CONST, chunk.c(null), st.line, st.col);
      chunk.emit(OP.RET,null,st.line,st.col);
      return;

    case "ThrowStmt":
      compileExpr(chunk,fileName,st.expr);
      chunk.emit(OP.THROW,null,st.line,st.col);
      return;

    case "TryCatchStmt":{
      // TRY catchAddr (patched)
      const tryOp=chunk.emit(OP.TRY,null,st.line,st.col);
      compileBlock(chunk,fileName,st.tryBlock);
      chunk.emit(OP.ENDTRY,null,st.line,st.col);
      const jend=chunk.emit(OP.JMP,null,st.line,st.col);
      // catch address
      const catchAddr = chunk.code.length;
      chunk.patch(tryOp, catchAddr);
      // catch block expects exception value on stack
      // define catch var
      chunk.emit(OP.DEF, {name:st.catchName, type:null}, st.line, st.col);
      compileBlock(chunk,fileName,st.catchBlock);
      chunk.emit(OP.ENDTRY,null,st.line,st.col);
      chunk.patch(jend, chunk.code.length);
      return;
    }

    case "Block":
      compileBlock(chunk,fileName,st);
      return;

    default:
      throwErr(fileName, st.line||1, st.col||1, `Unknown statement kind: ${st.kind}`);
  }
}

function tryCompileLoopBody(chunk,fileName,body, loopStartAddr, updateAnchor){
  // We handle break/continue by raising signals in VM, so compiler doesn't need patch lists
  // but we do need to know where continue jumps: for while -> loopStartAddr, for for-loop -> updateAnchor.addr
  // We'll store "loop context" as metadata op markers:
  // For simplicity, we emit ENTER/LEAVE around the loop body and let VM handle signals with a loop stack.
  chunk.emit(OP.ENTER,{loop:true,loopStart:loopStartAddr,forUpdateAnchor:updateAnchor},body.line,body.col);
  compileBlock(chunk,fileName,body);
  chunk.emit(OP.LEAVE,{loop:true},body.line,body.col);
}

function compileExpr(chunk,fileName,e){
  switch(e.kind){
    case "Literal":
      chunk.emit(OP.PUSH_CONST, chunk.c(e.value), e.line, e.col);
      return;

    case "Var":
      chunk.emit(OP.LOAD, e.name, e.line, e.col);
      return;

    case "List":
      for(const it of e.items) compileExpr(chunk,fileName,it);
      chunk.emit(OP.LIST_NEW, e.items.length, e.line, e.col);
      return;

    case "ObjectLit":
      chunk.emit(OP.OBJ_NEW,null,e.line,e.col);
      for(const p of e.pairs){
        compileExpr(chunk,fileName,p.val);
        chunk.emit(OP.OBJ_SET, p.key, p.line, p.col);
        // OBJ_SET leaves assigned value on stack; pop it to keep object at top
        chunk.emit(OP.POP,null,p.line,p.col);
      }
      return;

    case "Unary":
      compileExpr(chunk,fileName,e.expr);
      if(e.op===TokenKind.MINUS) chunk.emit(OP.NEG,null,e.line,e.col);
      else if(e.op===TokenKind.BANG) chunk.emit(OP.NOT,null,e.line,e.col);
      else throwErr(fileName,e.line,e.col,"Unknown unary operator");
      return;

    case "Binary":
      compileExpr(chunk,fileName,e.left);
      compileExpr(chunk,fileName,e.right);
      switch(e.op){
        case TokenKind.PLUS: chunk.emit(OP.ADD,null,e.line,e.col); break;
        case TokenKind.MINUS: chunk.emit(OP.SUB,null,e.line,e.col); break;
        case TokenKind.STAR: chunk.emit(OP.MUL,null,e.line,e.col); break;
        case TokenKind.SLASH: chunk.emit(OP.DIV,null,e.line,e.col); break;
        case TokenKind.EQEQ: chunk.emit(OP.EQ,null,e.line,e.col); break;
        case TokenKind.BANGEQ: chunk.emit(OP.NEQ,null,e.line,e.col); break;
        case TokenKind.LT: chunk.emit(OP.LT,null,e.line,e.col); break;
        case TokenKind.LTE: chunk.emit(OP.LTE,null,e.line,e.col); break;
        case TokenKind.GT: chunk.emit(OP.GT,null,e.line,e.col); break;
        case TokenKind.GTE: chunk.emit(OP.GTE,null,e.line,e.col); break;
        default: throwErr(fileName,e.line,e.col,"Unknown binary operator");
      }
      return;

    case "Member":
      // special list len
      compileExpr(chunk,fileName,e.object);
      if(e.prop==="len"){
        chunk.emit(OP.LIST_LEN,null,e.line,e.col);
      } else {
        chunk.emit(OP.OBJ_GET,e.prop,e.line,e.col);
      }
      return;

    case "Call":
      // list.get / list.set sugar:
      // If callee is Member(object, prop) and object is list at runtime,
      // VM handles OBJ_GET returning callable builtin bound method.
      compileExpr(chunk,fileName,e.callee);
      for(const a of e.args) compileExpr(chunk,fileName,a);
      chunk.emit(OP.CALL, e.args.length, e.line, e.col);
      return;

    default:
      throwErr(fileName,e.line||1,e.col||1,`Unknown expression kind: ${e.kind}`);
  }
}

class VM {
  constructor(fileName, rootChunk, funcObjs, loader, absPath){
    this.fileName=fileName;
    this.rootChunk=rootChunk;
    this.funcObjs=funcObjs;
    this.loader=loader;
    this.absPath=absPath;

    this.stack=[];
    this.tryStack=[];  // {catchIp, envDepth}
    this.loopStack=[]; // {startIp, updateIpOrNull, envDepth}

    this.env=new Env(null);
    this.installBuiltins();
    this.installFuncs();
  }

  installBuiltins(){
    this.env.define(this.fileName,1,1,"print",makeBuiltinPrint());
    this.env.define(this.fileName,1,1,"typeof",makeBuiltinTypeof());
    this.env.define(this.fileName,1,1,"load",makeBuiltinLoad(path.dirname(this.absPath)));

    // import() expression
    this.env.define(this.fileName,1,1,"import",(spec)=>{
      if(typeof spec!=="string") throw new Error("import() expects string path");
      const resolved=this.loader.resolve(spec, path.dirname(this.absPath));
      const rec=this.loader.loadModule(resolved);
      return rec.exports;
    });
  }

  installFuncs(){
    // put compiled func objects into env
    for(const name of Object.keys(this.funcObjs)){
      const fnObj=this.funcObjs[name];
      // wrap as callable closure capturing current env
      const closure=this.makeClosure(fnObj, this.env);
      this.env.define(this.fileName,fnObj.chunk.code[0]?.line||1,fnObj.chunk.code[0]?.col||1,name,closure);
    }
  }

  makeClosure(fnObj, capturedEnv){
    const vm=this;
    const f=function(...args){
      return vm.runFunction(fnObj, capturedEnv, args);
    };
    f.__one_closure = true;
    f.__one_name = fnObj.name;
    f.__one_params = fnObj.params;
    f.__one_returnType = fnObj.returnType;
    f.__one_chunk = fnObj.chunk;
    f.__one_env = capturedEnv;
    return f;
  }

  runFunction(fnObj, capturedEnv, args){
    // new env chained to capturedEnv (closures)
    const local=new Env(capturedEnv);

    // bind params
    for(let i=0;i<fnObj.params.length;i++){
      const p=fnObj.params[i];
      const v = (i<args.length) ? args[i] : null;
      if(p.type){
        const a=typeOf(v);
        if(a!==p.type) throwErr(this.fileName,p.line,p.col,`Type error: param ${p.name}: expected ${p.type} got ${a}`);
      }
      local.define(this.fileName,p.line,p.col,p.name,v,p.type);
    }

    // execute chunk
    const savedEnv=this.env;
    const savedTry=this.tryStack;
    const savedLoop=this.loopStack;
    const savedStack=this.stack;

    this.env=local;
    this.tryStack=[];
    this.loopStack=[];
    this.stack=[];

    try{
      const ret=this.exec(fnObj.chunk);
      // return type check
      if(fnObj.returnType){
        const a=typeOf(ret);
        if(a!==fnObj.returnType) throwErr(this.fileName,fnObj.chunk.code[0]?.line||1,fnObj.chunk.code[0]?.col||1,`Type error: return ${fnObj.name}: expected ${fnObj.returnType} got ${a}`);
      }
      return ret;
    } finally {
      this.env=savedEnv;
      this.tryStack=savedTry;
      this.loopStack=savedLoop;
      this.stack=savedStack;
    }
  }

  exec(chunk){
    let ip=0;
    const code=chunk.code;
    const consts=chunk.consts;

    const pop=()=>this.stack.pop();
    const push=(v)=>this.stack.push(v);

    const binNum=(line,col,a,b,op)=>{
      assertTypeAt(this.fileName,line,col,"number",a);
      assertTypeAt(this.fileName,line,col,"number",b);
      return op(a,b);
    };

    const cmp=(line,col,a,b,op)=>{
      const ta=typeOf(a), tb=typeOf(b);
      if(ta!==tb) {
        // only == and != allow mismatch
        return op==="eq" ? false : op==="neq" ? true : throwErr(this.fileName,line,col,`Type error: cannot compare ${ta} with ${tb}`);
      }
      if(ta!=="number" && ta!=="string") throwErr(this.fileName,line,col,`Type error: comparisons require number or string`);
      if(op==="lt") return a<b;
      if(op==="lte") return a<=b;
      if(op==="gt") return a>b;
      return a>=b;
    };

    const makeListMethod=(list, method)=>{
      if(method==="get") return (i)=>{
        assertTypeAt(this.fileName,1,1,"number",i);
        const idx=Math.trunc(i);
        if(idx<0 || idx>=list.length) throwErr(this.fileName,1,1,`List index out of range: ${idx}`);
        return list[idx];
      };
      if(method==="set") return (i,v)=>{
        assertTypeAt(this.fileName,1,1,"number",i);
        const idx=Math.trunc(i);
        if(idx<0 || idx>=list.length) throwErr(this.fileName,1,1,`List index out of range: ${idx}`);
        list[idx]=v;
        return v;
      };
      return null;
    };

    const unwindToCatch = (thrown)=>{
      // unwind loop scopes by resetting env if needed
      while(this.tryStack.length){
        const top=this.tryStack.pop();
        // unwind env depth
        // we can't count depth precisely, but we can restore to a parent chain length
        // We'll store envDepth as count of parent hops from current to root env at TRY time.
        // We'll rebuild by walking up.
        const restoreToDepth=(depth)=>{
          let e=this.env;
          let curDepth=0;
          while(e.parent){ curDepth++; e=e.parent; }
          // e is root, now walk down? can't. So we instead store the actual env pointer at try start.
          // For correctness, store envRef instead of depth.
        };
        // In this implementation, we stored envDepth loosely; we will store envRef instead (see TRY handler below).
        // Thus: top.envRef
        this.env = top.envRef;
        // push exception for catch var
        push(thrown);
        ip = top.catchIp;
        return { ip, handled:true };
      }
      return { handled:false };
    };

    while(ip < code.length){
      const ins=code[ip];
      const op=ins.op;
      const arg=ins.arg;
      const line=ins.line||1, col=ins.col||1;

      try{
        switch(op){
          case OP.ENTER:{
            // if loop marker, push loop context
            if(arg && arg.loop){
              // determine update address if known (for for loops)
              const updateIp = (arg.forUpdateAnchor && typeof arg.forUpdateAnchor.addr==="number") ? arg.forUpdateAnchor.addr : null;
              this.loopStack.push({ startIp: arg.loopStart, updateIp, envRef: this.env });
            }
            ip++; break;
          }
          case OP.LEAVE:{
            if(arg && arg.loop){
              this.loopStack.pop();
            }
            ip++; break;
          }
          case OP.PUSH_CONST: push(consts[arg]); ip++; break;

          case OP.LOAD:{
            const v=this.env.get(this.fileName,line,col,arg);
            push(v);
            ip++; break;
          }

          case OP.DEF:{
            const v=pop();
            if(arg && arg.type){
              const a=typeOf(v);
              if(a!==arg.type) throwErr(this.fileName,line,col,`Type error: ${arg.name}: expected ${arg.type} got ${a}`);
            }
            this.env.define(this.fileName,line,col,arg.name,v,arg.type||null);
            ip++; break;
          }

          case OP.STORE:{
            const v=pop();
            const annot=this.env.getAnnot(arg.name);
            if(annot){
              const a=typeOf(v);
              if(a!==annot) throwErr(this.fileName,line,col,`Type error: ${arg.name}: expected ${annot} got ${a}`);
            }
            this.env.set(this.fileName,line,col,arg.name,v);
            push(v);
            ip++; break;
          }

          case OP.POP: pop(); ip++; break;

          case OP.OBJ_NEW: push(Object.create(null)); ip++; break;

          case OP.OBJ_SET:{
            const v=pop();
            const o=pop();
            if(o===null || typeof o!=="object" || Array.isArray(o)) throwErr(this.fileName,line,col,"Object property set on non-object");
            o[arg]=v;
            push(o);
            push(v);
            ip++; break;
          }

          case OP.OBJ_GET:{
            const o=pop();
            if(Array.isArray(o)){
              // list methods
              if(arg==="get"||arg==="set"){
                push(makeListMethod(o,arg));
                ip++; break;
              }
              if(arg==="len"){ push(o.length); ip++; break; }
              throwErr(this.fileName,line,col,`List has no property '${arg}' (use .get/.set/.len)`);
            }
            if(o===null || typeof o!=="object") throwErr(this.fileName,line,col,"Property access on non-object");
            if(!(arg in o)) throwErr(this.fileName,line,col,`Property not found: ${arg}`);
            push(o[arg]);
            ip++; break;
          }

          case OP.LIST_NEW:{
            const n=arg;
            const items=new Array(n);
            for(let i=n-1;i>=0;i--) items[i]=pop();
            push(items);
            ip++; break;
          }
          case OP.LIST_GET:{
            const iVal=pop(), list=pop();
            if(!Array.isArray(list)) throwErr(this.fileName,line,col,".get called on non-list");
            assertTypeAt(this.fileName,line,col,"number",iVal);
            const idx=Math.trunc(iVal);
            if(idx<0 || idx>=list.length) throwErr(this.fileName,line,col,`List index out of range: ${idx}`);
            push(list[idx]);
            ip++; break;
          }
          case OP.LIST_SET:{
            const v=pop(), iVal=pop(), list=pop();
            if(!Array.isArray(list)) throwErr(this.fileName,line,col,".set called on non-list");
            assertTypeAt(this.fileName,line,col,"number",iVal);
            const idx=Math.trunc(iVal);
            if(idx<0 || idx>=list.length) throwErr(this.fileName,line,col,`List index out of range: ${idx}`);
            list[idx]=v;
            push(v);
            ip++; break;
          }
          case OP.LIST_LEN:{
            const list=pop();
            if(!Array.isArray(list)) throwErr(this.fileName,line,col,".len on non-list");
            push(list.length);
            ip++; break;
          }

          case OP.ADD:{
            const b=pop(), a=pop();
            const ta=typeOf(a), tb=typeOf(b);
            if(ta==="number"&&tb==="number"){ push(a+b); ip++; break; }
            if(ta==="string"&&tb==="string"){ push(a+b); ip++; break; }
            throwErr(this.fileName,line,col,"'+' expects number+number or string+string");
          }
          case OP.SUB:{ const b=pop(), a=pop(); push(binNum(line,col,a,b,(x,y)=>x-y)); ip++; break; }
          case OP.MUL:{ const b=pop(), a=pop(); push(binNum(line,col,a,b,(x,y)=>x*y)); ip++; break; }
          case OP.DIV:{ const b=pop(), a=pop(); push(binNum(line,col,a,b,(x,y)=>x/y)); ip++; break; }

          case OP.EQ:{ const b=pop(), a=pop(); if(typeOf(a)!==typeOf(b)) push(false); else push(a===b); ip++; break; }
          case OP.NEQ:{ const b=pop(), a=pop(); if(typeOf(a)!==typeOf(b)) push(true); else push(a!==b); ip++; break; }

          case OP.LT:{ const b=pop(), a=pop(); push(cmp(line,col,a,b,"lt")); ip++; break; }
          case OP.LTE:{ const b=pop(), a=pop(); push(cmp(line,col,a,b,"lte")); ip++; break; }
          case OP.GT:{ const b=pop(), a=pop(); push(cmp(line,col,a,b,"gt")); ip++; break; }
          case OP.GTE:{ const b=pop(), a=pop(); push(cmp(line,col,a,b,"gte")); ip++; break; }

          case OP.NEG:{ const v=pop(); assertTypeAt(this.fileName,line,col,"number",v); push(-v); ip++; break; }
          case OP.NOT:{ const v=pop(); assertTypeAt(this.fileName,line,col,"bool",v); push(!v); ip++; break; }

          case OP.JMP: ip=arg; break;

          case OP.JMPF:{
            const v=pop();
            assertTypeAt(this.fileName,line,col,"bool",v);
            if(v===false) ip=arg; else ip++;
            break;
          }

          case OP.TRY:{
            // store envRef for correct restoration
            this.tryStack.push({ catchIp: arg, envRef: this.env });
            ip++; break;
          }
          case OP.ENDTRY:{
            // pop one try frame if present (normal completion)
            if(this.tryStack.length) this.tryStack.pop();
            ip++; break;
          }

          case OP.THROW:{
            const v=pop();
            const res=unwindToCatch(v);
            if(!res.handled) throwErr(this.fileName,line,col,`Uncaught throw: ${String(v)}`);
            ip=res.ip;
            break;
          }

          case OP.CALL:{
            const argc=arg;
            const args=new Array(argc);
            for(let i=argc-1;i>=0;i--) args[i]=pop();
            const callee=pop();
            if(typeof callee!=="function") throwErr(this.fileName,line,col,`Attempted to call non-function (${typeOf(callee)})`);
            const ret=callee(...args);
            push(ret===undefined?null:ret);
            ip++; break;
          }

          case OP.RET:{
            const v=pop();
            return v;
          }

          case OP.BREAK:{
            // jump out of nearest loop by setting ip to after loop (handled via signals)
            // We implement break by throwing a signal, caught by loop boundaries in bytecode via ENTER/LEAVE markers.
            throw new BreakSignal();
          }
          case OP.CONTINUE:{
            throw new ContinueSignal();
          }

          default:
            throwErr(this.fileName,line,col,`Unknown opcode: ${op}`);
        }
      } catch (sig){
        // Handle loop control flow signals
        if(sig instanceof BreakSignal){
          // consume loop: advance ip until we exit loop scope
          if(!this.loopStack.length) throwErr(this.fileName,line,col,"break used outside loop");
          // pop loop context and jump to end: easiest is to scan forward to matching LEAVE(loop)
          this.loopStack.pop();
          // scan forward to next LEAVE(loop:true) after current ip
          let depth=0;
          for(let j=ip+1;j<code.length;j++){
            const ins2=code[j];
            if(ins2.op===OP.ENTER && ins2.arg && ins2.arg.loop) depth++;
            if(ins2.op===OP.LEAVE && ins2.arg && ins2.arg.loop){
              if(depth===0){ ip=j+1; break; }
              depth--;
            }
          }
          continue;
        }
        if(sig instanceof ContinueSignal){
          if(!this.loopStack.length) throwErr(this.fileName,line,col,"continue used outside loop");
          const loop=this.loopStack[this.loopStack.length-1];
          // jump to update for for-loop if known, else to start
          if(typeof loop.updateIp==="number") ip=loop.updateIp;
          else ip=loop.startIp;
          continue;
        }
        throw sig;
      }
    }

    return null;
  }
}

/* ============================================================
   Module Loader (backend bytecode)
============================================================ */

class ModuleLoader {
  constructor(){
    this.cache=new Map();   // abs -> {exports}
    this.loading=new Set();
    this.stack=[];
  }

  resolve(specifier, fromDir){
    if(typeof specifier!=="string") return null;
    let p=specifier;
    if(!path.extname(p)) p += ".one";
    return path.isAbsolute(p) ? p : path.resolve(fromDir, p);
  }

  loadModule(absPath){
    if(this.cache.has(absPath)) return this.cache.get(absPath);
    if(this.loading.has(absPath)){
      const chain=[...this.stack, absPath].map(x=>path.basename(x)).join(" -> ");
      throw new Error(`Circular import detected: ${chain}`);
    }
    this.loading.add(absPath);
    this.stack.push(absPath);

    const srcRaw=readText(absPath);
    const fileName=path.basename(absPath);
    const mode=findHeaderMode(srcRaw);
    if(mode!=="backend") throw new OneError("Only @backend modules can be imported in v1.0", fileName, 1, 1);

    enforceBackendNoFrontendTags(fileName, srcRaw);
    const src=stripHeaderLine(srcRaw);

    const lex=new Lexer(fileName, src);
    const tokens=lex.tokenizeAll();
    const parser=new Parser(fileName, tokens);
    let program=parser.parseProgram();

    const analysis=analyzeProgram(fileName, program);
    program=optimizeProgram(program);

    // compile and run
    const compiled=compileProgramToChunk(fileName, program, analysis);
    const vm=new VM(fileName, compiled.chunk, compiled.funcObjs, this, absPath);
    vm.exec(compiled.chunk);

    // exports snapshot
    const exportsObj=Object.create(null);
    for(const name of compiled.exports){
      exportsObj[name]=vm.env.get(fileName,1,1,name);
    }

    const rec={exports:exportsObj};
    this.cache.set(absPath,rec);

    this.stack.pop();
    this.loading.delete(absPath);
    return rec;
  }
}

/* ============================================================
   Backend runner
============================================================ */

function runBackend(absPath, fileName, srcRaw, loader){
  enforceBackendNoFrontendTags(fileName, srcRaw);
  const src=stripHeaderLine(srcRaw);

  const lex=new Lexer(fileName, src);
  const tokens=lex.tokenizeAll();
  const parser=new Parser(fileName, tokens);
  let program=parser.parseProgram();

  const analysis=analyzeProgram(fileName, program);
  program=optimizeProgram(program);

  const compiled=compileProgramToChunk(fileName, program, analysis);
  const vm=new VM(fileName, compiled.chunk, compiled.funcObjs, loader, absPath);
  vm.exec(compiled.chunk);
}

/* ============================================================
   Frontend compiler
============================================================ */

class FScanner {
  constructor(fileName, src){
    this.fileName=fileName; this.src=src; this.i=0; this.line=1; this.col=1;
  }
  peek(n=0){ const idx=this.i+n; return idx>=0&&idx<this.src.length?this.src[idx]:"\0"; }
  advance(){ const ch=this.peek(); this.i++; if(ch==="\n"){this.line++;this.col=1}else this.col++; return ch; }
  startsWith(s){ return this.src.slice(this.i,this.i+s.length)===s; }
  skipWs(){ while(true){ const ch=this.peek(); if(ch==="\0") break; if(ch===" "||ch==="\t"||ch==="\r"||ch==="\n") this.advance(); else break; } }
}

function parseFrontend(fileName, srcRaw){
  let markupSrc=srcRaw;

  function extractBlock(tag){
    const open=`<${tag}>`, close=`</${tag}>`;
    const oi=markupSrc.indexOf(open);
    if(oi===-1) return null;
    const ci=markupSrc.indexOf(close, oi+open.length);
    if(ci===-1){
      const {line,col}=indexToLineCol(markupSrc,oi);
      throwErr(fileName,line,col,`Unterminated <${tag}> (missing ${close})`);
    }
    const inner=markupSrc.slice(oi+open.length, ci);
    markupSrc = markupSrc.slice(0,oi) + markupSrc.slice(ci+close.length);
    return inner;
  }

  const logicRaw=extractBlock("logic");
  const styleRaw=extractBlock("style");

  markupSrc = markupSrc.replace(/^\s*@frontend[^\n]*\n/, "");

  const s=new FScanner(fileName, markupSrc);

  function parseStringLiteral(){
    const startLine=s.line, startCol=s.col;
    if(s.peek()!=="\"") throwErr(fileName,startLine,startCol,"Expected string literal");
    s.advance();
    let out="";
    while(true){
      const c=s.peek();
      if(c==="\0") throwErr(fileName,startLine,startCol,"Unterminated string");
      if(c==="\""){ s.advance(); break; }
      if(c==="\\"){
        s.advance();
        const esc=s.peek();
        if(esc==="\0") throwErr(fileName,startLine,startCol,"Unterminated escape");
        s.advance();
        if(esc==="n") out+="\n";
        else if(esc==="t") out+="\t";
        else out+=esc;
      } else out+=s.advance();
    }
    return node("Text",{line:startLine,col:startCol,value:out});
  }

  function parseTextOrInterp(){
    s.skipWs();
    const ch=s.peek();
    if(ch==="\"") return parseStringLiteral();
    if(ch==="{"){
      const startLine=s.line,startCol=s.col;
      s.advance();
      s.skipWs();
      // parse ONE expression by reusing backend lexer/parser on the snippet until "}"
      // We'll collect raw inside braces.
      let depth=1;
      let inner="";
      while(true){
        const c=s.peek();
        if(c==="\0") throwErr(fileName,startLine,startCol,"Unterminated { ... } in markup");
        if(c==="{"){ depth++; inner+=s.advance(); continue; }
        if(c==="}"){
          depth--;
          if(depth===0){ s.advance(); break; }
          inner+=s.advance(); continue;
        }
        inner+=s.advance();
      }
      return node("Interp",{line:startLine,col:startCol,exprSrc:inner.trim()});
    }
    return null;
  }

  function parseAttrs(){
    const attrs=[];
    while(true){
      s.skipWs();
      const ch=s.peek();
      if(ch===">"||ch==="\0") break;

      if(!isIdentStart(ch)) throwErr(fileName,s.line,s.col,"Expected attribute name");
      let name="";
      const startLine=s.line,startCol=s.col;
      while(isIdentPart(s.peek())) name+=s.advance();

      s.skipWs();
      if(s.peek()!=="=") throwErr(fileName,s.line,s.col,"Expected '=' after attribute name");
      s.advance();
      s.skipWs();
      if(s.peek()!=="\"") throwErr(fileName,s.line,s.col,'Expected attribute value like "..."');
      const val=parseStringLiteral().value;
      attrs.push({name,value:val,line:startLine,col:startCol});
    }
    return attrs;
  }

  function parseElement(){
    const startLine=s.line,startCol=s.col;
    if(s.peek()!=="<") return null;
    if(s.startsWith("</")) return null;

    s.advance();
    s.skipWs();
    if(!isIdentStart(s.peek())) throwErr(fileName,s.line,s.col,"Expected tag name");
    let tag="";
    while(isIdentPart(s.peek())) tag+=s.advance();

    const attrs=parseAttrs();
    if(s.peek()!==">") throwErr(fileName,s.line,s.col,"Expected '>'");
    s.advance();

    const children=[];
    while(true){
      s.skipWs();
      if(s.startsWith(`</${tag}`)) break;
      if(s.peek()==="\0") throwErr(fileName,startLine,startCol,`Unterminated <${tag}> (missing </${tag}>)`);

      const el=parseElement();
      if(el){ children.push(el); continue; }

      const t=parseTextOrInterp();
      if(t){ children.push(t); continue; }

      throwErr(fileName,s.line,s.col,'Markup text must be quoted like "Hello" or use {expr}');
    }

    // closing
    s.advance(); s.advance(); // </
    for(const c of tag){
      if(s.peek()!==c) throwErr(fileName,s.line,s.col,"Mismatched closing tag");
      s.advance();
    }
    s.skipWs();
    if(s.peek()!==">") throwErr(fileName,s.line,s.col,"Expected '>' at end of closing tag");
    s.advance();

    return node("Element",{line:startLine,col:startCol,tag,attrs,children});
  }

  function parseRoot(){
    const nodes=[];
    while(true){
      s.skipWs();
      if(s.peek()==="\0") break;

      const el=parseElement();
      if(el){ nodes.push(el); continue; }

      const t=parseTextOrInterp();
      if(t){ nodes.push(t); continue; }

      throwErr(fileName,s.line,s.col,"Unexpected content in frontend file");
    }
    return nodes;
  }

  const nodes=parseRoot();
  return { logicRaw, styleRaw, nodes };
}

function escapeJs(s){
  return String(s)
    .replace(/\\/g,"\\\\")
    .replace(/`/g,"\\`")
    .replace(/\$/g,"\\$")
    .replace(/\r/g,"\\r")
    .replace(/\n/g,"\\n");
}

// JS emitter for ONE backend (subset) used in <logic> for frontend
function emitBackendToJS(fileName, backendSrc){
  // compile backendSrc to AST, optimize, then emit JS
  const lex=new Lexer(fileName, backendSrc);
  const tokens=lex.tokenizeAll();
  const parser=new Parser(fileName, tokens);
  let program=parser.parseProgram();
  analyzeProgram(fileName, program);
  program=optimizeProgram(program);

  function eExpr(e){
    switch(e.kind){
      case "Literal":
        return JSON.stringify(e.value);
      case "Var":
        return e.name;
      case "List":
        return `[${e.items.map(eExpr).join(", ")}]`;
      case "ObjectLit":{
        const parts=e.pairs.map(p=>`${JSON.stringify(p.key)}: ${eExpr(p.val)}`);
        return `{${parts.join(", ")}}`;
      }
      case "Unary":{
        const op = (e.op===TokenKind.MINUS) ? "-" : "!";
        return `(${op}${eExpr(e.expr)})`;
      }
      case "Binary":{
        const opMap={
          [TokenKind.PLUS]:"+",[TokenKind.MINUS]:"-",
          [TokenKind.STAR]:"*",[TokenKind.SLASH]:"/",
          [TokenKind.EQEQ]:"===",[TokenKind.BANGEQ]:"!==",
          [TokenKind.LT]:"<",[TokenKind.LTE]:"<=",
          [TokenKind.GT]:">",[TokenKind.GTE]:">=",
        };
        const op=opMap[e.op]||"?";
        return `(${eExpr(e.left)} ${op} ${eExpr(e.right)})`;
      }
      case "Member":
        if(e.prop==="len") return `(${eExpr(e.object)}.length)`;
        return `(${eExpr(e.object)}[${JSON.stringify(e.prop)}])`;
      case "Call":
        return `${eExpr(e.callee)}(${e.args.map(eExpr).join(", ")})`;
      default:
        return "undefined";
    }
  }

  function sStmt(st, depth){
    const ind="  ".repeat(depth);
    switch(st.kind){
      case "ImportDecl":
        return `${ind}// import "${st.path}" (ignored in frontend logic)`;
      case "LetStmt":
        return `${ind}let ${st.name} = ${eExpr(st.value)};`;
      case "AssignStmt":{
        const parts=st.target.parts;
        if(parts.length===1) return `${ind}${parts[0]} = ${eExpr(st.value)};`;
        // obj.prop chain
        let lhs=parts[0];
        for(let i=1;i<parts.length;i++){
          lhs += `[${JSON.stringify(parts[i])}]`;
        }
        return `${ind}${lhs} = ${eExpr(st.value)};`;
      }
      case "ExprStmt":
        return `${ind}${eExpr(st.expr)};`;
      case "IfStmt":{
        const then=st.thenBlock.statements.map(x=>sStmt(x,depth+1)).join("\n");
        const els=st.elseBlock? st.elseBlock.statements.map(x=>sStmt(x,depth+1)).join("\n") : "";
        return `${ind}if (${eExpr(st.cond)}) {\n${then}\n${ind}}` + (st.elseBlock? ` else {\n${els}\n${ind}}`:"");
      }
      case "WhileStmt":{
        const body=st.body.statements.map(x=>sStmt(x,depth+1)).join("\n");
        return `${ind}while (${eExpr(st.cond)}) {\n${body}\n${ind}}`;
      }
      case "ForStmt":{
        const init=st.init? sStmt(st.init,0).trim().replace(/;$/,"") : "";
        const cond=st.cond? eExpr(st.cond) : "true";
        const upd=st.update? sStmt(st.update,0).trim().replace(/;$/,"") : "";
        const body=st.body.statements.map(x=>sStmt(x,depth+1)).join("\n");
        return `${ind}for (${init}; ${cond}; ${upd}) {\n${body}\n${ind}}`;
      }
      case "BreakStmt": return `${ind}break;`;
      case "ContinueStmt": return `${ind}continue;`;
      case "ReturnStmt": return `${ind}return ${st.expr?eExpr(st.expr):"null"};`;
      case "ThrowStmt": return `${ind}throw ${eExpr(st.expr)};`;
      case "TryCatchStmt":{
        const tb=st.tryBlock.statements.map(x=>sStmt(x,depth+1)).join("\n");
        const cb=st.catchBlock.statements.map(x=>sStmt(x,depth+1)).join("\n");
        return `${ind}try {\n${tb}\n${ind}} catch (${st.catchName}) {\n${cb}\n${ind}}`;
      }
      case "Block":{
        const b=st.statements.map(x=>sStmt(x,depth+1)).join("\n");
        return `${ind}{\n${b}\n${ind}}`;
      }
      default:
        return `${ind}// <unknown stmt>`;
    }
  }

  // include function decls at top
  const lines=[];
  lines.push(`// Generated by ONE v1.0 frontend logic emitter`);
  lines.push(`(function(){`);
  lines.push(`  "use strict";`);
  lines.push(`  // helpers for lists`);
  lines.push(`  function __listGet(a,i){ i|=0; if(i<0||i>=a.length) throw new Error("List index out of range: "+i); return a[i]; }`);
  lines.push(`  function __listSet(a,i,v){ i|=0; if(i<0||i>=a.length) throw new Error("List index out of range: "+i); a[i]=v; return v; }`);
  lines.push(`  // expose a namespace for handlers`);
  lines.push(`  const ONE = (window.ONE = window.ONE || Object.create(null));`);

  // function decls
  for(const it of program.items){
    if(it.kind!=="FuncDecl") continue;
    const params=it.params.map(p=>p.name).join(", ");
    lines.push(`  function ${it.name}(${params}) {`);
    const body=it.body.statements.map(s=>sStmt(s,2)).join("\n");
    lines.push(body);
    lines.push(`  }`);
    lines.push(`  ONE[${JSON.stringify(it.name)}] = ${it.name};`);
  }

  // top-level statements
  for(const it of program.items){
    if(it.kind==="FuncDecl") continue;
    lines.push(sStmt(it,1));
  }

  // export top-level lets into ONE namespace
  for(const it of program.items){
    if(it.kind==="LetStmt" && it.exported){
      lines.push(`  ONE[${JSON.stringify(it.name)}] = ${it.name};`);
    }
    if(it.kind==="FuncDecl" && it.exported){
      lines.push(`  ONE[${JSON.stringify(it.name)}] = ${it.name};`);
    }
  }

  lines.push(`})();`);
  return lines.join("\n");
}

function emitFrontendJS(fileName, fe){
  const lines=[];
  lines.push(`// Generated by ONE v1.0 frontend compiler`);
  lines.push(`(function(){`);
  lines.push(`  "use strict";`);
  lines.push(`  function __text(v){ return document.createTextNode(String(v)); }`);
  lines.push(`  function __el(t){ return document.createElement(t); }`);
  lines.push(`  function __setAttr(el,k,v){ el.setAttribute(k,String(v)); }`);
  lines.push(`  function __bindEvent(el, name, expr){`);
  lines.push(`    // expr like "handler()" or "ONE.handler()"`);
  lines.push(`    el.addEventListener(name.toLowerCase().replace(/^on/, ""), function(ev){`);
  lines.push(`      try {`);
  lines.push(`        // give handler access to event`);
  lines.push(`        const event = ev;`);
  lines.push(`        return (new Function("event", "ONE", "return ("+expr+")")).call(null, event, window.ONE||{});`);
  lines.push(`      } catch(e){ console.error(e); }`);
  lines.push(`    });`);
  lines.push(`  }`);

  if(fe.styleRaw && fe.styleRaw.trim().length){
    lines.push(`  (function(){`);
    lines.push(`    const st=document.createElement("style");`);
    lines.push(`    st.textContent = \`${escapeJs(fe.styleRaw)}\`;`);
    lines.push(`    document.head.appendChild(st);`);
    lines.push(`  })();`);
  }

  // compile interpolations as ONE expressions evaluated via JS Function in context of window.ONE and local vars
  lines.push(`  function __evalExpr(src){`);
  lines.push(`    try { return (new Function("ONE", "return ("+src+")"))(window.ONE||{}); }`);
  lines.push(`    catch(e){ console.error("Interp error:", src, e); return ""; }`);
  lines.push(`  }`);

  lines.push(`  const __root=document.createDocumentFragment();`);

  function emitNode(n, parentVar, idx){
    if(n.kind==="Text"){
      lines.push(`  ${parentVar}.appendChild(__text(\`${escapeJs(n.value)}\`));`);
      return;
    }
    if(n.kind==="Interp"){
      lines.push(`  ${parentVar}.appendChild(__text(__evalExpr(${JSON.stringify(n.exprSrc)})));`);
      return;
    }
    if(n.kind==="Element"){
      const v=`__n${idx}`;
      lines.push(`  const ${v} = __el(${JSON.stringify(n.tag)});`);
      for(const a of n.attrs){
        // event attrs: onClick="handler()"
        if(/^on[A-Za-z]/.test(a.name)){
          lines.push(`  __bindEvent(${v}, ${JSON.stringify(a.name)}, ${JSON.stringify(a.value)});`);
        } else {
          lines.push(`  __setAttr(${v}, ${JSON.stringify(a.name)}, ${JSON.stringify(a.value)});`);
        }
      }
      lines.push(`  ${parentVar}.appendChild(${v});`);
      let childIdx=idx*1000+1;
      for(const c of n.children) emitNode(c,v,childIdx++);
      return;
    }
  }

  let idx=1;
  for(const n of fe.nodes) emitNode(n,"__root",idx++);

  lines.push(`  document.body.appendChild(__root);`);
  lines.push(`})();`);
  return lines.join("\n");
}

function buildFrontend(absPath, fileName, srcRaw){
  const fe=parseFrontend(fileName, srcRaw);

  // logic -> JS (full backend supported)
  let logicJS="";
  if(fe.logicRaw && fe.logicRaw.trim().length){
    logicJS = emitBackendToJS(fileName, fe.logicRaw);
  } else {
    logicJS = `// no <logic> provided\nwindow.ONE = window.ONE || Object.create(null);`;
  }

  const domJS = emitFrontendJS(fileName, fe);
  const outBase = absPath.replace(/\.one$/i,"");
  const jsPath = outBase + ".js";
  const htmlPath = outBase + ".html";

  const html = [
    "<!doctype html>",
    "<html>",
    "<head>",
    `  <meta charset="utf-8" />`,
    `  <meta name="viewport" content="width=device-width,initial-scale=1" />`,
    `  <title>${escapeHtml(fileName)}</title>`,
    "</head>",
    "<body>",
    `  <script src="${path.basename(jsPath)}"></script>`,
    "</body>",
    "</html>",
    ""
  ].join("\n");

  // combined JS: logic first, then DOM builder
  const js = [logicJS, "", domJS, ""].join("\n");

  writeText(jsPath, js);
  writeText(htmlPath, html);
  console.log(`ONE build: ${path.basename(htmlPath)} + ${path.basename(jsPath)}`);
}

function escapeHtml(s){
  return String(s).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");
}

/* ============================================================
   Formatter
============================================================ */

function indent(n){ return "  ".repeat(n); }

function opToStr(op){
  return {
    [TokenKind.PLUS]:"+",
    [TokenKind.MINUS]:"-",
    [TokenKind.STAR]:"*",
    [TokenKind.SLASH]:"/",
    [TokenKind.EQEQ]:"==",
    [TokenKind.BANGEQ]:"!=",
    [TokenKind.LT]:"<",
    [TokenKind.LTE]:"<=",
    [TokenKind.GT]:">",
    [TokenKind.GTE]:">=",
  }[op] || "?";
}

function fmtExpr(e){
  switch(e.kind){
    case "Literal":
      if(typeof e.value==="string") return `"${e.value.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`;
      if(e.value===null) return "null";
      if(Array.isArray(e.value)) return `(${e.value.map(v=>JSON.stringify(v)).join(", ")})`;
      if(typeof e.value==="object") return JSON.stringify(e.value);
      return String(e.value);
    case "Var": return e.name;
    case "List": return `(${e.items.map(fmtExpr).join(", ")})`;
    case "ObjectLit":{
      const ps=e.pairs.map(p=>`${p.key}: ${fmtExpr(p.val)}`).join(", ");
      return `{ ${ps} }`;
    }
    case "Unary": return (e.op===TokenKind.MINUS?"-":"!")+fmtExpr(e.expr);
    case "Binary": return `${fmtExpr(e.left)} ${opToStr(e.op)} ${fmtExpr(e.right)}`;
    case "Member": return `${fmtExpr(e.object)}.${e.prop}`;
    case "Call": return `${fmtExpr(e.callee)}(${e.args.map(fmtExpr).join(", ")})`;
    default: return "<expr>";
  }
}

function fmtStmt(s, depth){
  switch(s.kind){
    case "ImportDecl": return indent(depth)+`import "${s.path}"`;
    case "LetStmt":{
      const head=(s.exported?"export ":"")+"let";
      return indent(depth)+`${head} ${s.name}`+(s.typeRef?`: ${s.typeRef.name}`:"")+` = ${fmtExpr(s.value)}`;
    }
    case "AssignStmt":
      return indent(depth)+`${s.target.parts.join(".")} = ${fmtExpr(s.value)}`;
    case "ExprStmt":
      return indent(depth)+fmtExpr(s.expr);
    case "BreakStmt": return indent(depth)+"break";
    case "ContinueStmt": return indent(depth)+"continue";
    case "ReturnStmt": return indent(depth)+(s.expr?`return ${fmtExpr(s.expr)}`:"return");
    case "ThrowStmt": return indent(depth)+`throw ${fmtExpr(s.expr)}`;
    case "TryCatchStmt":{
      const tb=s.tryBlock.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      const cb=s.catchBlock.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      return indent(depth)+`try {\n${tb}\n${indent(depth)}} catch (${s.catchName}) {\n${cb}\n${indent(depth)}}`;
    }
    case "IfStmt":{
      const thenBody=s.thenBlock.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      let out=indent(depth)+`if (${fmtExpr(s.cond)}) {\n${thenBody}\n${indent(depth)}}`;
      if(s.elseBlock){
        const elseBody=s.elseBlock.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
        out += ` else {\n${elseBody}\n${indent(depth)}}`;
      }
      return out;
    }
    case "WhileStmt":{
      const body=s.body.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      return indent(depth)+`while (${fmtExpr(s.cond)}) {\n${body}\n${indent(depth)}}`;
    }
    case "ForStmt":{
      const init=s.init?fmtStmt(s.init,0).trim():"";
      const cond=s.cond?fmtExpr(s.cond):"";
      const upd=s.update?fmtStmt(s.update,0).trim():"";
      const body=s.body.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      return indent(depth)+`for (${init}; ${cond}; ${upd}) {\n${body}\n${indent(depth)}}`;
    }
    case "Block":{
      const body=s.statements.map(x=>fmtStmt(x,depth+1)).join("\n");
      return indent(depth)+`{\n${body}\n${indent(depth)}}`;
    }
    default: return indent(depth)+"<stmt>";
  }
}

function fmtProgram(program){
  const parts=[];
  for(const it of program.items){
    if(it.kind==="FuncDecl"){
      const head=(it.exported?"export ":"")+"fn";
      const sig = `${head} ${it.name}(` + it.params.map(p=>p.name+(p.typeRef?`: ${p.typeRef.name}`:"")).join(", ") + `)` + (it.returnType?`: ${it.returnType.name}`:"");
      const body=it.body.statements.map(s=>fmtStmt(s,1)).join("\n");
      parts.push(sig+` {\n${body}\n}`);
    } else {
      parts.push(fmtStmt(it,0));
    }
  }
  return parts.filter(Boolean).join("\n\n")+"\n";
}

function formatFile(absPath, fileName, srcRaw){
  const mode=findHeaderMode(srcRaw);
  if(mode==="frontend"){
    // keep frontend mostly as-is but normalize header and blocks
    const fe=parseFrontend(fileName, srcRaw);
    const lines=[];
    lines.push("@frontend");
    lines.push("");
    if(fe.logicRaw!=null){
      lines.push("<logic>");
      // format logic with backend formatter
      const lex=new Lexer(fileName, fe.logicRaw);
      const tokens=lex.tokenizeAll();
      const parser=new Parser(fileName, tokens);
      let prog=parser.parseProgram();
      analyzeProgram(fileName, prog);
      prog=optimizeProgram(prog);
      lines.push(fmtProgram(prog).trimEnd());
      lines.push("</logic>");
      lines.push("");
    }
    if(fe.styleRaw!=null){
      lines.push("<style>");
      lines.push(fe.styleRaw.replace(/\r\n/g,"\n").replace(/\s+$/g,""));
      lines.push("</style>");
      lines.push("");
    }
    // emit markup with minimal formatting (keep structure)
    function fmtFeNode(n,d){
      if(n.kind==="Text") return indent(d)+`"${n.value.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`;
      if(n.kind==="Interp") return indent(d)+`{${n.exprSrc}}`;
      if(n.kind==="Element"){
        const attrs=(n.attrs||[]).map(a=>` ${a.name}="${String(a.value).replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`).join("");
        const open=indent(d)+`<${n.tag}${attrs}>`;
        const close=indent(d)+`</${n.tag}>`;
        if(!n.children||!n.children.length) return open+"\n"+close;
        const kids=n.children.map(c=>fmtFeNode(c,d+1)).join("\n");
        return open+"\n"+kids+"\n"+close;
      }
      return indent(d)+"<unknown/>";
    }
    for(let i=0;i<fe.nodes.length;i++){
      lines.push(fmtFeNode(fe.nodes[i],0));
      if(i!==fe.nodes.length-1) lines.push("");
    }
    writeText(absPath, lines.join("\n")+"\n");
    return;
  }

  // backend
  enforceBackendNoFrontendTags(fileName, srcRaw);
  const src=stripHeaderLine(srcRaw);
  const lex=new Lexer(fileName, src);
  const tokens=lex.tokenizeAll();
  const parser=new Parser(fileName, tokens);
  let program=parser.parseProgram();
  analyzeProgram(fileName, program);
  program=optimizeProgram(program);
  const out=fmtProgram(program);
  const trimmed=srcRaw.trimStart();
  const hasBackendHeader=trimmed.startsWith("@backend");
  const finalOut=(hasBackendHeader?"@backend\n\n":"")+out;
  writeText(absPath, finalOut);
}

/* ============================================================
   Serve + Test + Init
============================================================ */

function serveDir(dir, port){
  const root=path.resolve(dir);
  const p=Number(port||"8000");
  const server=http.createServer((req,res)=>{
    const urlPath = decodeURIComponent((req.url||"/").split("?")[0]);
    const safePath = path.normalize(urlPath).replace(/^(\.\.[/\\])+/, "");
    let fp=path.join(root,safePath);
    if(fs.existsSync(fp) && fs.statSync(fp).isDirectory()) fp=path.join(fp,"index.html");
    if(!fs.existsSync(fp)){ res.statusCode=404; res.end("Not Found"); return; }
    const ext=path.extname(fp).toLowerCase();
    const ct = ext===".html"?"text/html":ext===".js"?"text/javascript":ext===".css"?"text/css":"application/octet-stream";
    res.setHeader("Content-Type", ct);
    res.end(fs.readFileSync(fp));
  });
  server.listen(p, ()=>console.log(`ONE serve: http://localhost:${p} (root ${root})`));
}

function runTests(){
  const tmpDir=path.join(process.cwd(),".one_tmp_tests");
  if(!fs.existsSync(tmpDir)) fs.mkdirSync(tmpDir,{recursive:true});

  const onePath=path.join(tmpDir,"t.one");
  const src=[
    "@backend",
    "export fn fib(n: number): number {",
    "  if (n <= 1) { return n }",
    "  return fib(n - 1) + fib(n - 2)",
    "}",
    "let a: number = 10",
    "let b = fib(a)",
    "print(b)",
    "let xs = (1, 2, 3)",
    "print(xs.len)",
    "print(xs.get(1))",
    "xs.set(1, 99)",
    "print(xs.get(1))",
    "let obj = { x: 1, y: 2 }",
    "obj.x = 7",
    "print(obj.x)",
    "try {",
    "  throw \"boom\"",
    "} catch (e) {",
    "  print(e)",
    "}",
    "let sum: number = 0",
    "for (let i: number = 1; i <= 5; i = i + 1) {",
    "  if (i == 3) { continue }",
    "  sum = sum + i",
    "}",
    "print(sum)",
    ""
  ].join("\n");
  writeText(onePath, src);

  const loader=new ModuleLoader();
  try{
    runBackend(onePath, "t.one", readText(onePath), loader);
    console.log("✅ All tests passed");
  } catch(e){
    console.error("❌ Tests failed");
    console.error(formatError(e, src));
    process.exit(1);
  } finally {
    // keep tmp for inspection
  }
}

function initProject(dir){
  const root=path.resolve(dir);
  fs.mkdirSync(root,{recursive:true});
  const mainBackend=path.join(root,"main.one");
  const mainFrontend=path.join(root,"app.one");

  if(!fileExists(mainBackend)){
    writeText(mainBackend, [
      "@backend",
      "export fn add(a: number, b: number): number {",
      "  return a + b",
      "}",
      "let x: number = 2",
      "let y: number = 3",
      "print(add(x, y))",
      ""
    ].join("\n"));
  }

  if(!fileExists(mainFrontend)){
    writeText(mainFrontend, [
      "@frontend",
      "",
      "<logic>",
      "export fn inc(x: number): number { return x + 1 }",
      "export let counter: number = 0",
      "export fn click() {",
      "  counter = inc(counter)",
      "  // DOM update happens in JS after build using ONE namespace",
      "  const el = document.getElementById(\"count\")",
      "  if (el) { el.textContent = String(counter) }",
      "}",
      "</logic>",
      "",
      "<style>",
      "body { font-family: system-ui, sans-serif; padding: 24px; }",
      "button { font-size: 16px; padding: 8px 12px; }",
      "</style>",
      "",
      "<div id=\"app\">",
      "  \"Counter: \" {ONE.counter}",
      "  <div id=\"count\">{ONE.counter}</div>",
      "  <button onClick=\"ONE.click()\">\"Click\"</button>",
      "</div>",
      ""
    ].join("\n"));
  }

  console.log(`ONE init: created ${root}`);
}

/* ============================================================
   CLI
============================================================ */

function usage(){
  console.log([
    "ONE v1.0",
    "Usage:",
    "  node one.js run <file.one>",
    "  node one.js build <file.one>",
    "  node one.js fmt <file.one>",
    "  node one.js serve <dir> [port]",
    "  node one.js test",
    "  node one.js init <dir>",
  ].join("\n"));
}

function main(){
  const a=process.argv.slice(2);
  if(a.length===0 || a[0]==="-h" || a[0]==="--help"){ usage(); process.exit(0); }

  const cmd=a[0];
  if(cmd==="serve"){
    const dir=a[1]||".";
    const port=a[2]||"8000";
    return serveDir(dir, port);
  }
  if(cmd==="test"){
    return runTests();
  }
  if(cmd==="init"){
    const dir=a[1]||"one-project";
    return initProject(dir);
  }

  // run/build/fmt default
  let modeCmd="run";
  let fileArg=null;
  if(cmd==="run"||cmd==="build"||cmd==="fmt"){
    modeCmd=cmd;
    fileArg=a[1]||null;
  } else {
    modeCmd="run";
    fileArg=a[0]||null;
  }
  if(!fileArg){ usage(); process.exit(1); }

  const abs=path.resolve(process.cwd(), fileArg);
  const fileName=path.basename(abs);
  const srcRaw=readText(abs);
  const mode=findHeaderMode(srcRaw);
  const loader=new ModuleLoader();

  try{
    if(modeCmd==="fmt"){
      formatFile(abs,fileName,srcRaw);
      console.log(`ONE fmt: formatted ${fileName}`);
      return;
    }

    if(modeCmd==="build"){
      if(mode==="frontend"){
        buildFrontend(abs,fileName,srcRaw);
      } else {
        // backend build = run optimizer + compile check (no output file yet)
        enforceBackendNoFrontendTags(fileName, srcRaw);
        const src=stripHeaderLine(srcRaw);
        const lex=new Lexer(fileName, src);
        const tokens=lex.tokenizeAll();
        const parser=new Parser(fileName, tokens);
        let program=parser.parseProgram();
        analyzeProgram(fileName, program);
        program=optimizeProgram(program);
        console.log(`ONE build: backend compiled OK (${fileName})`);
      }
      return;
    }

    // run
    if(mode==="frontend"){
      // build then serve hint
      buildFrontend(abs,fileName,srcRaw);
      console.log(`Tip: node one.js serve ${path.dirname(abs)} 8000`);
      return;
    }
    runBackend(abs,fileName,srcRaw,loader);
  } catch(e){
    console.error(formatError(e, srcRaw));
    process.exit(1);
  }
}

main();