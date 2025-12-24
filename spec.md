ONE Programming Language
Version 0.1 — Balanced Specification

ONE is a statically typed, strict, general-purpose programming language designed to be one for all: backend, frontend, and shared logic in a single coherent language.

This document defines the normative specification for ONE v0.1.
Anything not specified here is undefined behavior.

1. Design Goals

ONE v0.1 prioritizes:

Clarity over cleverness

Strict correctness (errors always surface)

Unified frontend + backend model

Predictable execution

Tooling-first design

Backward compatibility within a version is guaranteed.

2. Files & Modes
2.1 File Extension

ONE source files use:

.one

2.2 Mode Declaration

A file MAY begin with one of:

@backend

@frontend


Rules:

If omitted, the file defaults to @backend

The mode applies to the entire file

Mixing frontend-only constructs in backend mode is an error

3. Lexical Structure
3.1 Whitespace

Spaces, tabs, and newlines are allowed freely

Newlines MAY terminate statements

Semicolons (;) MAY terminate statements

Mixing is allowed

3.2 Comments
// single-line comment


Comments have no semantic meaning.

4. Tokens & Symbols
4.1 Brackets
Symbol	Meaning
()	Grouping, calls, tuples
{}	Blocks (required)
[]	Not used
<>	Frontend structure only
4.2 Operators
Operator	Meaning
=	Assignment
+ - * /	Arithmetic
== !=	Equality
< <= > >=	Comparison
!	Logical NOT
.	Member access
:	Type annotation
,	Separator
5. Keywords (Reserved)
let fn type if else while return
true false null import


Keywords may not be used as identifiers.

6. Types (Strict)
6.1 Built-in Types
Type	Description
number	Floating-point numeric
string	UTF-8 text
bool	true / false
null	Explicit null
6.2 Typing Rules

ONE is statically typed

Type inference is allowed

No implicit coercion is allowed

Any type mismatch is a compile-time or runtime error

Examples:

let x = 5          // inferred number
let y: string = "hi"
let z: number = "no"   // ERROR

7. Variables
7.1 Declaration
let name = expression
let name: Type = expression


Rules:

Variables are block-scoped

Redeclaration in the same scope is an error

Assignment to undeclared variables is an error

8. Expressions

Expressions include:

Literals

Variables

Arithmetic

Comparisons

Function calls

Member access

Tuples

Example:

(x + 3) * 2

9. Control Flow
9.1 If
if (condition) {
  ...
} else {
  ...
}


Rules:

Condition must be bool

Braces are required

9.2 While
while (condition) {
  ...
}

10. Functions
10.1 Declaration
fn name(param, param: Type): ReturnType {
  ...
}


Rules:

Return type may be inferred

Missing return defaults to null

Parameters are strictly typed if annotated

10.2 Call
name(arg1, arg2)

11. Tuples (Lists v0.1)

Tuples are ordered collections:

let t = (1, 2, 3)


Rules:

Tuples are immutable in v0.1

Indexing is not supported yet

tuple.len is supported

12. Frontend Mode (@frontend)

Frontend files support structured UI syntax.

12.1 Markup
<div>
  "Hello"
</div>


Rules:

Text must be quoted

Tags must be balanced

Attributes must be strings

12.2 Interpolation
{variable}


Rules:

Only identifiers or string literals allowed (v0.1)

13. <logic> Block
<logic>
  let name = "World"
</logic>


Rules:

Only let statements allowed

Values must be literals

Logic is evaluated at compile time

14. <style> Block
<style>
  body { background: black; }
</style>


Rules:

Content is raw text

No parsing in v0.1

15. Imports (v0.1)
import "path"


Rules:

Parsed but not executed in v0.1

Future versions define module resolution

16. Errors
16.1 Error Policy

All errors are fatal

Errors include:

Line number

Column number

Clear message

16.2 Undefined Behavior

Anything not defined in this spec is undefined behavior and may change.

17. Execution Model

Backend files execute top-to-bottom

Frontend files compile to JavaScript

No concurrency in v0.1

Deterministic execution

18. Compatibility Promise

ONE v0.1 code will run unchanged on all v0.1 implementations

Breaking changes require a version bump

19. Future Directions (Non-Normative)

Planned for future versions:

Module system

Formatter (one fmt)

LSP tooling

Bytecode VM

WASM output

Reactive frontend state

Async & concurrency