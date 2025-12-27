# ONE v1.1 Specification

ONE is a “one for all” programming language designed to unify backend and frontend development under a single syntax, a single compiler, and a single mental model.

ONE v1.1 is a production-hardening release that fixes remaining semantic gaps from v1.0, adds frontend–backend parity, introduces build artifacts, and defines a stable foundation for public release and tooling.

---

## 1. Files, Modes, and Targets

### 1.1 File Extensions

- Source files use the `.one` extension.
- Compiled backend artifacts use the `.onebc` extension.

### 1.2 Modes

A ONE file operates in one of two modes:

- `@backend` (default if omitted)
- `@frontend`

The mode is determined by the first non-whitespace line in the file. If no mode directive is present, the file is treated as `@backend`.

### 1.3 Backend Target

Backend compilation produces:
- In-memory bytecode for `one run file.one`
- A persisted bytecode artifact for `one build file.one`, written as `file.onebc`

Backend execution always occurs on the ONE Bytecode Virtual Machine.

### 1.4 Frontend Target

Frontend compilation produces two files:
- `file.html`
- `file.js`

The generated HTML loads the generated JavaScript and renders the application in a browser.

Frontend output must enforce ONE semantics strictly:
- Conditions must evaluate to `bool`
- Type errors must throw ONE runtime errors
- Scoping rules must match backend behavior
- Error handling must behave identically to backend execution

---

## 2. Lexical Structure

### 2.1 Whitespace

Whitespace consists of:
- Space
- Tab
- Carriage return

Newlines terminate statements unless a semicolon is used.

### 2.2 Comments

Single-line comments begin with `//` and continue until the end of the line.

### 2.3 Identifiers

Identifiers:
- Must start with a letter (`A–Z`, `a–z`) or underscore (`_`)
- May contain letters, digits, and underscores

Multi-word identifiers must use camelCase formatting.

### 2.4 Literals

Supported literal types:
- Numbers: `1`, `2.5`
- Strings: `"hello"`
- Booleans: `true`, `false`
- Null: `null`

Strings support escape sequences: `\n`, `\t`, `\"`, `\\`.

### 2.5 Operators and Symbols (Backend)

- Grouping and calls: `(` `)`
- Blocks and objects: `{` `}`
- Member access: `.`
- Separators: `,` `:` `;`
- Assignment: `=`
- Equality: `==` `!=`
- Comparison: `<` `<=` `>` `>=`
- Arithmetic: `+` `-` `*` `/`
- Unary: `!` `-`

### 2.6 Angled Brackets in Backend

In backend mode, `<` and `>` are valid only as comparison operators.

Any markup-like usage (e.g. `<div>`, `</x>`, `<logic>`) is a syntax error in backend mode.

---

## 3. Types and Values

### 3.1 Built-in Types

- `number`
- `string`
- `bool`
- `null`
- `object`
- `list`

### 3.2 Type Annotations

Type annotations are optional but strict if present.

If a value does not match its declared type at runtime, a type error is thrown.

Type annotations are supported on:
- Variable declarations
- Function parameters
- Function return values

### 3.3 Default Values

- Missing function arguments default to `null`
- Missing return statements default to `null`

---

## 4. Variables and Scope

### 4.1 Declarations

Variables are declared using `let`.  
Variables are block-scoped.  
Variables must be declared before use.

Example:
let x: number = 10
{
  let x: number = 20
  print(x)
}
print(x)

### 4.2 Assignment

Variables and object properties may be assigned:

x = 5  
obj.key = 10

---

## 5. Expressions

### 5.1 Operator Precedence (Highest to Lowest)

1. Member access and function calls  
2. Unary operators  
3. Multiplication and division  
4. Addition and subtraction  
5. Comparisons  
6. Equality checks  

### 5.2 Lists

Lists use tuple syntax:

let xs = (1, 2, 3)

An empty list is written as `()`.

List operations:
- xs.len
- xs.get(i)
- xs.set(i, value)

Square-bracket indexing is not used.

---

## 6. Control Flow

### 6.1 if / else

if (cond) { ... } else { ... }

Conditions must evaluate to `bool`.

### 6.2 while

while (cond) { ... }

### 6.3 for

for (init; cond; update) { ... }

Each clause may be empty. If present, the condition must be `bool`.

### 6.4 break / continue

`break` and `continue` may only appear inside loops.

### 6.5 return

return  
return expr  

---

## 7. Functions

### 7.1 Declarations

fn add(a: number, b: number): number {
  return a + b
}

Functions are hoisted within their scope.

### 7.2 Calls

let x = add(2, 3)

Closures and recursion are supported.

---

## 8. Objects

### 8.1 Object Literals

let user = { name: "Max", age: 19 }

### 8.2 Property Access

print(user.name)

### 8.3 Property Assignment

user.age = user.age + 1

Computed keys and method shorthand are not supported in v1.1.

---

## 9. Error Handling

### 9.1 throw

throw "error"  
throw 123  
throw { msg: "bad" }

### 9.2 try / catch

try {
  risky()
} catch (e) {
  print(e)
}

Thrown values propagate until caught.

---

## 10. Modules

### 10.1 Import Forms

Statement import:
import "./file.one"

Expression import:
let m = import("./file.one")

### 10.2 Export Forms

export let x = 1  
export fn f() { ... }

### 10.3 Resolution Rules

- Paths are resolved relative to the importing file
- `.one` is appended if missing
- Modules execute once and are cached
- Circular imports are errors

### 10.4 Frontend Module Bundling

In `@frontend`, all imported modules in `<logic>` are bundled into the generated JavaScript output.

---

## 11. Frontend Structure

A frontend file may contain:
- `<logic>` block with full backend code
- `<style>` block (raw CSS)
- Markup content

### 11.1 Text Nodes

Text must be quoted:
"Hello"

### 11.2 Interpolation

{value}

Expressions in interpolation must follow backend expression rules.

### 11.3 Events

<button onClick="handler()">
  "Click"
</button>

Handlers refer to functions defined in `<logic>`.

---

## 12. Compiler Pipeline

### 12.1 Backend Pipeline

1. Lexer  
2. Parser  
3. Analyzer  
4. Optimizer  
5. Bytecode Generation  
6. VM Execution or `.onebc` Output  

### 12.2 Frontend Pipeline

1. Extract `<logic>`, `<style>`, markup  
2. Compile `<logic>` via backend pipeline  
3. Bundle modules  
4. Emit strict-semantics JS runtime  
5. Emit HTML loader  

---

## 13. Optimizer Passes

- Constant folding  
- Dead code elimination  
- Branch pruning  
- Return-based dead code elimination  

All optimizations must preserve semantics.

---

## 14. CLI

Command: `one`

Supported commands:
- one run
- one build
- one fmt
- one serve
- one test
- one init

---

## 15. Compatibility Guarantees

- Backend VM and frontend JS enforce identical semantics
- `.onebc` artifacts are forward-compatible within 1.x
- Module behavior is identical in backend and frontend

---

## 16. Non-Goals (v1.1)

- Generics
- Classes
- Computed object keys
- Concurrency primitives
- FFI
- Self-hosting compiler

---

## 17. Status

ONE v1.1 defines a complete, strict, production-ready language specification suitable for public release, tooling integration, and future evolution toward self-hosting.

END OF SPEC
