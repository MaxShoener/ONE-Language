# ONE v1.1 Spec (SPEC_v1_1.md)

> ONE v1.1 = “production hardening”: frontend parity, universal modules, bytecode artifacts, stronger optimizer, and CLI polish.

This spec is written to match the ONE v1.0 blueprint you provided, with v1.1 fixes for:
1) frontend imports/exports
2) strict ONE semantics in emitted JS (no JS truthiness leaks)
3) `.onebc` bytecode build artifacts (fast startup)
4) editor/tooling integration path (VS Code tasks/commands)
5) optimizer upgrades (safe folding + DCE + branch pruning + return-based DCE + light LICM)

---

## 0. Concepts

### Modes
- `@backend` (default)
- `@frontend`

A file’s mode is determined by the first directive line, if present:
- `@backend`
- `@frontend`

If absent, the mode is `@backend`.

### File extension
- Source: `.one`
- Bytecode artifact: `.onebc`

---

## 1. Lexical syntax (backend)

### Whitespace & terminators
- Newlines act as statement terminators (like “soft semicolons”).
- `;` is optional and may terminate a statement.
- Comments:
  - `//` line comment

### Identifiers
- `[A-Za-z_][A-Za-z0-9_]*`

### Keywords
- `let`, `fn`, `type`
- `if`, `else`
- `while`, `for`
- `break`, `continue`, `return`
- `true`, `false`, `null`
- `import`, `export`
- `throw`, `try`, `catch`

### Literals
- numbers: `123`, `3.14`
- strings: `"text"` with escapes `\n \t \" \\`
- bool: `true`, `false`
- null: `null`

### Operators
- Arithmetic: `+ - * /`
- Comparison: `< <= > >=`
- Equality: `== !=`
- Unary: `! -`
- Assignment: `=`
- Member access: `.`
- Object literal: `{ key: expr, ... }`

### Backend angle bracket rule
Backend mode forbids HTML-like tags:
- Disallowed patterns: `<name` or `</name`
- Allowed: comparisons like `a < b` or `x <= y`

---

## 2. Types

### Runtime types
- `number`
- `string`
- `bool`
- `null`
- `object`
- `list`

### Type annotations (optional)
- `let x: number = 1`
- `fn add(a: number, b: number): number { return a + b }`

Type checking is **strict**:
- If annotated, runtime value must match exactly.
- Incorrect types produce a runtime error (ONE error).

### Lists
ONE uses parentheses list syntax:
- list literal: `(1, 2, 3)`
- empty list: `()`

List operations (v1.1):
- `.len` property (read-only)
- `.get(i)` returns element
- `.set(i, v)` mutates element, returns `null`

Indexing `[i]` is not used in v1.1.

---

## 3. Declarations

### Variables
- Only `let` (no `const` yet).
- Must be declared before use.
- Block scoped.

### Functions
- Declared with `fn`.
- Hoisted at module scope (callable before definition).
- Support recursion and closures.

Default return value: `null`.

### Types
- `type Name { field: Type ... }` exists for future shaping.
- v1.1 runtime enforces only primitive annotations (`number|string|bool|null|object|list`) unless extended.

---

## 4. Statements & control flow

### Blocks
- `{ ... }` is a block scope.

### if / else
- Condition must be `bool` (strict).

### while
- Condition must be `bool`.
- Supports `break`, `continue`.

### for
Syntax:
- `init` may be:
  - `let x = expr`
  - assignment `x = expr`
  - expression statement (rare)
  - empty
- `cond` is an expression; if present must evaluate to `bool`
- `update` may be assignment or expression; may be empty
- Supports `break`, `continue`

### return
- `return` or `return expr`

### break / continue
- Only valid inside loops.

---

## 5. Expressions

### Precedence (high → low)
- member/call
- unary `! -`
- `* /`
- `+ -`
- comparisons `< <= > >=`
- equality `== !=`

### Calls
- `f(1, 2)`

### Member access
- `obj.key`
- assignment: `obj.key = expr` (v1.1)

### Objects
- literal: `{ a: 1, b: "x" }`
- keys are identifiers only (no computed keys).
- nested objects allowed.

---

## 6. Error handling

### throw
- `throw expr`
- throws any value (commonly string)

### try / catch
- `e` is a new block-scoped variable in the catch block.
- if no catch matches, error propagates and stops the program.

---

## 7. Modules

### Import forms
- statement form:
  - `import "./file.one"`
  - executes module once for side effects, caches exports
- expression form:
  - `let m = import("./file.one")`
  - returns an exports object

### Exports
- `export let x = 1`
- `export fn f() { ... }`

### Circular imports
- Error with a clear “cycle” chain.

### v1.1 frontend module support
- Imports and exports inside frontend `<logic>` are supported.
- Frontend compilation bundles imported backend modules into the emitted JS runtime so the browser can resolve them.

---

## 8. Frontend (`@frontend`)

A frontend file may contain:
- `<logic> ... </logic>` : full backend language (v1.1), including imports/exports, loops, throw/try/catch, objects, lists
- `<style> ... </style>` : preserved and embedded into output HTML
- Markup:
  - elements: `<div id="x"> ... </div>`
  - text must be quoted: `"Hello"`
  - interpolation: `{expr}` (backend expression)
  - event attributes: `onClick="handler()"`, `onInput="handler()"`, etc.

### Output
`one build file.one` where file is `@frontend` produces:
- `file.html` — contains markup and style
- `file.js` — contains compiled/bundled runtime + bytecode + DOM builder glue

Semantics guarantee:
- Frontend execution must enforce ONE rules (strict bool conditions, type checks, etc.). No JS truthiness.

---

## 9. Compiler pipeline

### Backend pipeline
1) Lexer
2) Parser (AST)
3) Analyzer (scope, declaration-before-use, loop control validity, etc.)
4) Optimizer
   - constant folding
   - constant propagation (safe + local)
   - dead code elimination (statement-level)
   - return-based DCE (unreachable after return/throw/break/continue)
   - branch pruning for constant conditions
   - light loop-invariant code motion (safe-only)
5) Bytecode compiler
6) Bytecode VM execution OR `.onebc` artifact output

### Frontend pipeline
1) Parse frontend blocks
2) Compile `<logic>` through backend pipeline to bytecode
3) Bundle imported modules’ bytecode
4) Emit JS runtime containing VM and module loader
5) Emit HTML + DOM builder that renders markup and wires events

---

## 10. CLI

Binary entrypoint:
- `node one.js ...` (or installed as `one`)

Commands:
- `one run <file.one>`  
  - backend: run via VM
  - frontend: compile and open instructions (or run `serve`)
- `one build <file.one>`  
  - backend: produce `<file>.onebc`
  - frontend: produce `<file>.html` and `<file>.js`
- `one fmt <file.one>`  
  - format backend or frontend with stable formatting
- `one serve <file.one> [--port N]`  
  - build (frontend) then serve directory with a simple dev server
- `one test`  
  - runs built-in compiler/VM tests
- `one init <dir>`  
  - scaffolds a sample ONE project

---

## 11. VS Code integration (v1.1 path)

v1.1 ships the CLI stability needed for VS Code:
- Run uses: `one run ${file}`
- Build uses: `one build ${file}`
- Format uses: `one fmt ${file}`

Extension responsibilities:
- syntax highlighting + language config
- formatter hook that calls `one fmt`
- commands: Run/Build
- diagnostics: parse ONE stderr output and surface as problems

---

## 12. Examples

### Backend modules
**math.one**
```one
@backend
export fn add(a: number, b: number): number { return a + b }
export let pi: number = 3.14159

**main.one**
@backend
```one
let m = import("./math.one")
print(m.add(2, 3))
print(m.pi)

Objects + Lists
@backend
let user = { name: "Max", score: 10 }
user.score = user.score + 5
let xs = (1, 2, 3)
xs.set(1, 99)
print(xs.get(1))
print(xs.len)

Try/Catch
@backend
try {
  throw "nope"
} catch (e) {
  print("caught:", e)
}

### Frontend
@frontend
<logic>
  import "./math.one"
  let clicks: number = 0
  fn inc(): null {
    clicks = clicks + 1
    return null
  }
</logic>

<style>
  body { font-family: sans-serif; }
</style>

<div id="app">
  <h1>"Clicks:" {clicks}</h1>
  <button onClick="inc()">"Click"</button>
</div>
