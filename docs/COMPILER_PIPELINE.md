# Zirco Compiler Pipeline Internals

This document provides a detailed explanation of each stage of the Zirco compiler pipeline, from source code to executable output.

## Table of Contents

1. [Overview](#overview)
2. [Stage 1: Lexical Analysis (Lexer)](#stage-1-lexical-analysis-lexer)
3. [Stage 2: Syntax Analysis (Parser)](#stage-2-syntax-analysis-parser)
4. [Stage 3: Semantic Analysis (Type Checker)](#stage-3-semantic-analysis-type-checker)
5. [Stage 4: Code Generation (Codegen)](#stage-4-code-generation-codegen)
6. [Stage 5: Diagnostics System](#stage-5-diagnostics-system)
7. [Pipeline Flow Diagram](#pipeline-flow-diagram)
8. [Example: Complete Compilation](#example-complete-compilation)

## Overview

The Zirco compiler (`zrc`) follows a traditional multi-stage compilation pipeline:

```
Source Code (.zr)
    ↓
[Lexer] → Tokens
    ↓
[Parser] → Abstract Syntax Tree (AST)
    ↓
[Type Checker] → Typed Abstract Syntax Tree (TAST)
    ↓
[Code Generator] → LLVM IR / Assembly / Object Code
    ↓
Output (stdout or file)
```

Each stage transforms the input into a more structured representation, ultimately producing executable machine code or intermediate representations. The compiler can halt at any stage to emit intermediate outputs (AST, TAST, LLVM IR, etc.) using the `--emit` flag.

**Key Implementation Files:**
- **Main driver:** `compiler/zrc/src/compile.rs`
- **Lexer:** `compiler/zrc_parser/src/lexer.rs`
- **Parser:** `compiler/zrc_parser/src/parser.rs`
- **Type Checker:** `compiler/zrc_typeck/src/typeck.rs`
- **Code Generator:** `compiler/zrc_codegen/src/program.rs`
- **Diagnostics:** `compiler/zrc_diagnostics/src/`

---

## Stage 1: Lexical Analysis (Lexer)

**Location:** `compiler/zrc_parser/src/lexer.rs`

### Purpose

The lexer (also called a tokenizer or scanner) is the first stage of compilation. It reads the raw source code character by character and groups them into meaningful tokens—the smallest units of syntax in the language.

### How It Works

The Zirco lexer is built using the [logos](https://github.com/maciejhirsz/logos) crate, a high-performance lexer generator. The lexer is implemented through the `ZircoLexer` wrapper around the auto-generated `Tok` enum.

**Process:**
1. **Input:** Raw source code string (e.g., `"fn main() { return 42; }"`)
2. **Tokenization:** Splits input into tokens using pattern matching rules
3. **Output:** Stream of `Spanned<Result<Tok, LexicalError>>` where each token includes:
   - The token type (e.g., `Tok::Fn`, `Tok::Identifier("main")`, `Tok::LeftParen`)
   - Span information (start and end positions in the source)

### Token Types

The `Tok` enum in `lexer.rs` defines all possible tokens:

- **Keywords:** `fn`, `let`, `if`, `else`, `while`, `for`, `return`, `struct`, etc.
- **Literals:**
  - Number literals: `42`, `0xFF` (hexadecimal), `0b1010` (binary)
  - String literals: `"hello"` with escape sequences (`\n`, `\t`, `\xFF`, etc.)
  - Boolean literals: `true`, `false`
- **Operators:** `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `&&`, `||`, etc.
- **Delimiters:** `(`, `)`, `{`, `}`, `[`, `]`, `,`, `;`, `:`
- **Identifiers:** Variable and function names

### Special Features

#### Comment Handling

The lexer automatically skips comments:

- **Single-line comments:** `// comment text` (skipped via regex pattern)
- **Block comments:** `/* comment */` with **nesting support**
  - Block comments can contain other block comments
  - Unterminated block comments produce a `LexicalError::UnterminatedBlockComment`

#### String Literals

String literals support various escape sequences:
- Standard escapes: `\n`, `\t`, `\r`, `\\`, `\"`
- Hex byte escapes: `\xFF` (outputs byte value 0xFF)
- Unicode escapes: `\u{1F600}` (emoji and Unicode characters)

Strings are tokenized as `Vec<StringTok>` to handle escape sequences efficiently.

#### Whitespace

Whitespace (spaces, tabs, newlines) is automatically skipped via the regex pattern `r"[ \t\r\n\f]+"`.

### Error Handling

Lexical errors are represented by the `LexicalError` enum:

- `UnknownToken(&str)`: Unrecognized character sequence
- `UnterminatedStringLiteral`: String not closed before EOF
- `UnterminatedBlockComment`: Block comment not closed (with nesting reminder)
- `UnknownEscapeSequence`: Invalid escape sequence in string (e.g., `\q`)
- `JavascriptUserDetected(&str)`: Special error for `===` or `!==` (suggests `==` or `!=`)

### Example

```rust
use zrc_parser::lexer::{ZircoLexer, Tok, NumberLiteral};

let source = "let x = 42;";
let mut lexer = ZircoLexer::new(source);

// Produces:
// Tok::Let at span 0..3
// Tok::Identifier("x") at span 4..5
// Tok::Equals at span 6..7
// Tok::NumberLiteral(Decimal("42")) at span 8..10
// Tok::Semicolon at span 10..11
```

### Usage in Pipeline

The lexer is called internally by the parser via `ZircoLexer::new(input)`. Users typically don't interact with the lexer directly—instead, they use `parser::parse_program()` which creates a lexer automatically.

---

## Stage 2: Syntax Analysis (Parser)

**Location:** `compiler/zrc_parser/src/parser.rs`, `compiler/zrc_parser/src/parser.lalrpop`

### Purpose

The parser takes the stream of tokens from the lexer and builds an **Abstract Syntax Tree (AST)**—a hierarchical representation of the program's syntactic structure.

### How It Works

The Zirco parser uses [LALRPOP](https://github.com/lalrpop/lalrpop), a Rust parser generator for LR(1) grammars. The grammar is defined in `parser.lalrpop`, and LALRPOP generates Rust code at build time.

**Process:**
1. **Input:** Token stream from `ZircoLexer`
2. **Parsing:** Applies grammar rules to build the AST
3. **Output:** `Vec<Spanned<Declaration>>` (AST root nodes)

### Grammar Structure

The grammar is defined in `compiler/zrc_parser/src/parser.lalrpop`:

- **Program:** Top-level declarations (functions, structs, type aliases)
- **Declarations:** `fn`, `struct`, `type`, `extern`
- **Statements:** `let`, `if`, `while`, `for`, `do-while`, `switch`, `return`, `break`, `continue`, etc.
- **Expressions:** Literals, identifiers, binary operations, function calls, struct construction, etc.
- **Types:** Primitives (`i32`, `bool`, `str`), pointers (`*T`), arrays (`[T; N]`), structs, functions

### Abstract Syntax Tree (AST)

The AST is defined in `compiler/zrc_parser/src/ast/`:

- **`ast/expr.rs`**: Expression nodes (`Expr` enum)
  - Literals: `NumberLiteral`, `StringLiteral`, `BooleanLiteral`
  - Operations: `Arithmetic`, `Comparison`, `Logical`, `Bitwise`
  - Complex expressions: `FunctionCall`, `StructConstruction`, `ArrayIndex`, `MemberAccess`
  - Control flow: `If`, `While`, `For`, `DoWhile`, `Switch`

- **`ast/stmt.rs`**: Statement nodes (`Stmt` and `Declaration` enums)
  - `LetDeclaration`: Variable declarations
  - `ExprStmt`: Expression statements
  - `Return`, `Break`, `Continue`: Control flow
  - `Declaration`: Top-level declarations (functions, structs, etc.)

- **`ast/ty.rs`**: Type nodes (`Type` enum)
  - Primitives: `I8`, `I16`, `I32`, `I64`, `U8`, `U16`, `U32`, `U64`, `Bool`, `Str`, `Void`
  - Compound: `Pointer`, `Array`, `Struct`, `Function`

### Error Handling

Parse errors are converted to diagnostics via `parser_error_to_diagnostic()`:

- `InvalidToken`: Unexpected character
- `UnrecognizedEof`: Unexpected end of file (shows expected tokens)
- `UnrecognizedToken`: Unexpected token (shows expected alternatives)
- `ExtraToken`: Extra token after complete parse

All parse errors include span information for precise error reporting.

### Public API

The parser exposes several entry points in `parser.rs`:

- **`parse_program(input: &str)`**: Parse a complete program (most common)
- **`parse_expr(input: &str)`**: Parse a single expression
- **`parse_stmt_list(input: &str)`**: Parse a list of statements
- **`parse_type(input: &str)`**: Parse a single type

### Example

```rust
use zrc_parser::parser::parse_program;

let source = "fn add(x: i32, y: i32) -> i32 { return x + y; }";
let ast = parse_program(source)?;

// AST structure:
// [Declaration::FunctionDeclaration {
//     name: "add",
//     parameters: [("x", Type::I32), ("y", Type::I32)],
//     return_type: Type::I32,
//     body: [Stmt::Return(Expr::Arithmetic(Add, 
//         Expr::Identifier("x"), 
//         Expr::Identifier("y")
//     ))]
// }]
```

### Build Process

The LALRPOP grammar is compiled during `cargo build` via a build script (`build.rs`). Changes to `parser.lalrpop` automatically trigger regeneration of the parser code.

---

## Stage 3: Semantic Analysis (Type Checker)

**Location:** `compiler/zrc_typeck/src/typeck/`

### Purpose

The type checker performs **semantic analysis**, verifying that the program is not only syntactically correct but also semantically valid. It checks types, resolves names, validates scopes, and produces a **Typed Abstract Syntax Tree (TAST)**.

### How It Works

The type checker traverses the AST and:
1. Resolves type annotations
2. Infers expression types
3. Checks type compatibility in assignments, function calls, and operations
4. Validates scoping rules (variable shadowing, redeclaration, etc.)
5. Produces a TAST with all type information attached

**Process:**
1. **Input:** `Vec<Spanned<ast::Declaration>>` (AST)
2. **Type Checking:** Validates semantics and attaches types
3. **Output:** `Vec<Spanned<tast::TypedDeclaration>>` (TAST)

### Key Components

The type checker is organized into several modules:

#### `typeck.rs` (Main Entry Point)

- **`type_program()`**: Main function that processes all top-level declarations
- Creates a `GlobalScope` to track all defined symbols
- Calls `process_declaration()` for each declaration

#### `typeck/scope.rs` (Scope Management)

Manages variable and function scopes:

- **`GlobalScope`**: Top-level scope for functions and types
- **`LocalScope`**: Function-local scope for variables
- Handles variable shadowing (allowed in Zirco)
- Detects redeclaration errors

#### `typeck/ty.rs` (Type Resolution)

- **`resolve_type()`**: Converts AST types to TAST types
- Resolves named types (structs, type aliases)
- Validates type existence and correctness

#### `typeck/expr.rs` (Expression Type Checking)

- **`type_expr()`**: Infers and validates expression types
- Handles type coercion (e.g., `i32` to `i64`)
- Checks operator compatibility
- Validates function calls (argument count and types)

#### `typeck/declaration.rs` (Declaration Processing)

- **`process_declaration()`**: Validates top-level declarations
- Checks function signatures
- Validates struct definitions
- Handles `extern` declarations

#### `typeck/block.rs` (Block Type Checking)

- **`type_block()`**: Validates statement blocks
- Tracks return statements and control flow
- Validates `break` and `continue` in loops
- Ensures all code paths return values (for non-void functions)

### Typed Abstract Syntax Tree (TAST)

The TAST is defined in `compiler/zrc_typeck/src/tast/`:

- **`tast/expr.rs`**: Typed expressions (`TypedExpr`)
  - Each expression includes an `inferred_type: Type`
  - Example: `TypedExpr { kind: Add(lhs, rhs), inferred_type: Type::I32 }`

- **`tast/stmt.rs`**: Typed statements (`TypedStmt`)
  - Variable declarations include resolved types
  - Function calls include type-checked arguments

- **`tast/ty.rs`**: Type representation (`Type` enum)
  - More detailed than AST types (includes struct layouts, function signatures)
  - Includes size information for code generation

### Type Inference

Zirco uses a relatively simple type inference system:

1. **Explicit types:** Variables with type annotations use the specified type
2. **Expression types:** Inferred from operands and operators
   - Binary operations: Both operands must be compatible
   - Function calls: Return type from function signature
   - Literals: Direct type (e.g., `42` is `i32`)
3. **Type coercion:** Implicit widening conversions (e.g., `i32` → `i64`)

### Error Detection

The type checker detects various semantic errors:

- **Type mismatch:** Incompatible types in assignment or operation
- **Undefined variable:** Use of undeclared identifier
- **Redeclaration:** Variable or function already declared in scope
- **Invalid operation:** Operator not defined for type (e.g., `"hello" + 5`)
- **Missing return:** Non-void function missing return statement
- **Type not found:** Reference to undefined struct or type alias
- **Argument mismatch:** Wrong number or types of function arguments

### Example

```rust
// Input AST for: let x: i32 = 42 + 10;
ast::LetDeclaration {
    name: "x",
    ty: Some(Type::I32),
    value: Some(Expr::Arithmetic(Add, 
        Expr::NumberLiteral(42),
        Expr::NumberLiteral(10)
    ))
}

// Output TAST:
tast::LetDeclaration {
    name: "x",
    ty: Type::I32,
    value: Some(TypedExpr {
        kind: Arithmetic(Add, 
            TypedExpr { kind: NumberLiteral(42), inferred_type: I32 },
            TypedExpr { kind: NumberLiteral(10), inferred_type: I32 }
        ),
        inferred_type: Type::I32
    })
}
```

### Global Scope

The `GlobalScope` tracks:
- Function signatures (for type checking calls before definition)
- Struct definitions (for field access and construction)
- Type aliases (for resolution)

This allows forward references and mutual recursion.

---

## Stage 4: Code Generation (Codegen)

**Location:** `compiler/zrc_codegen/src/`

### Purpose

The code generator translates the type-checked TAST into **LLVM Intermediate Representation (IR)**, which can then be optimized and compiled to machine code.

### How It Works

The code generator uses [inkwell](https://github.com/TheDan64/inkwell), Rust bindings for LLVM, to construct LLVM IR programmatically.

**Process:**
1. **Input:** `Vec<Spanned<tast::TypedDeclaration>>` (TAST)
2. **Code Generation:** Translates TAST to LLVM IR
3. **Optimization:** Runs LLVM optimization passes
4. **Output:** LLVM IR (text), assembly, or object code

### Key Components

#### `program.rs` (Main Entry Point)

- **`cg_program_to_string()`**: Generates LLVM IR as a string
- **`cg_program_to_buffer()`**: Generates assembly or object code as bytes
- **`cg_program()`**: Core code generation logic
  - Creates LLVM `Context` and `Module`
  - Declares all functions (forward declarations)
  - Generates code for each function body
  - Runs optimization passes

#### `stmt.rs` (Statement Code Generation)

- **`cg_block()`**: Generates IR for a block of statements
- **`cg_let_declaration()`**: Handles `let` bindings (allocates stack space)
- Generates code for control flow (`if`, `while`, `for`, `switch`)
- Handles `return`, `break`, `continue`

#### `expr.rs` (Expression Code Generation)

- **`cg_expr()`**: Generates IR for expressions
- Handles literals, arithmetic, comparisons, function calls
- Implements type coercion and casts
- Manages operator overloading (when available)

#### `expr/place.rs` (Lvalue Code Generation)

- **`cg_place()`**: Generates pointer to lvalues (assignable locations)
- Handles variables, dereferences, array indexing, struct field access

#### `scope.rs` (Code Generation Scope)

- **`CgScope`**: Tracks LLVM values for variables
- Maps variable names to LLVM `PointerValue` (for stack allocations)

#### `ty.rs` (Type Conversion)

- **`llvm_basic_type()`**: Converts Zirco types to LLVM types
- **`llvm_function_type()`**: Creates LLVM function types
- Maps primitives (e.g., `i32` → LLVM i32)
- Handles compound types (arrays, structs, pointers)

### LLVM IR Generation

**Example:** Generating IR for `fn add(x: i32, y: i32) -> i32 { return x + y; }`

1. **Function declaration:**
   ```llvm
   define i32 @add(i32 %x, i32 %y) {
   ```

2. **Allocate parameters on stack:**
   ```llvm
   %x.addr = alloca i32
   %y.addr = alloca i32
   store i32 %x, i32* %x.addr
   store i32 %y, i32* %y.addr
   ```

3. **Load and add:**
   ```llvm
   %1 = load i32, i32* %x.addr
   %2 = load i32, i32* %y.addr
   %3 = add i32 %1, %2
   ```

4. **Return:**
   ```llvm
   ret i32 %3
   }
   ```

### Debug Information

The code generator can emit DWARF debug information when `debug_mode` is enabled:
- Line number information
- Variable locations
- Function metadata
- Allows debugging with GDB/LLDB

### Optimization

After IR generation, LLVM optimization passes are applied via `optimize_module()`:
- **Optimization levels:** `None`, `Less`, `Default`, `Aggressive`
- Uses `PassManagerBuilder` to configure passes
- Applies standard LLVM optimizations (inlining, constant folding, dead code elimination, etc.)

### Output Formats

The code generator supports multiple output formats (via `FileType`):

- **LLVM IR (`--emit llvm`)**: Human-readable LLVM IR text
- **Assembly (`--emit asm`)**: Target-specific assembly code
- **Object (`--emit object`)**: Compiled object file (`.o`)

### Target Architecture

Code generation is target-aware:
- Uses `TargetTriple` to specify target platform
- Can cross-compile to different architectures (x86_64, ARM, etc.)
- Respects target-specific features (CPU, features)

### Example

```rust
// TAST input:
tast::FunctionDeclaration {
    name: "main",
    return_type: Type::I32,
    body: [TypedStmt::Return(TypedExpr {
        kind: NumberLiteral(42),
        inferred_type: Type::I32
    })]
}

// Generated LLVM IR:
// define i32 @main() {
//   ret i32 42
// }
```

### Testing

The code generator uses snapshot testing (via `insta` crate):
- Tests in `stmt.rs` and `expr.rs` verify LLVM IR output
- Snapshots stored in `src/snapshots/`
- Use `cargo insta review` to update snapshots

---

## Stage 5: Diagnostics System

**Location:** `compiler/zrc_diagnostics/src/`

### Purpose

The diagnostics system provides **user-friendly error reporting** throughout the compilation pipeline. It handles errors from all stages (lexing, parsing, type checking, code generation) and formats them for display.

### How It Works

The diagnostics system uses the [ariadne](https://github.com/zesterer/ariadne) crate to produce beautiful, colorful error messages with source context.

**Components:**

#### `Diagnostic` (Main Type)

Defined in `diagnostic.rs`:

```rust
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub severity: Severity,
    pub span: Span,
}
```

- **`kind`**: The specific error or warning type
- **`severity`**: Error, Warning, or Info
- **`span`**: Source code location (start and end positions)

#### `DiagnosticKind` (Error Types)

Defined in `diagnostic_kind.rs`:

- **Lexical errors:**
  - `UnknownToken(String)`
  - `UnterminatedStringLiteral`
  - `UnterminatedBlockComment`
  - `UnknownEscapeSequence`

- **Parse errors:**
  - `InvalidToken`
  - `UnrecognizedToken(String, Vec<String>)`
  - `UnexpectedEof(Vec<String>)`
  - `ExtraToken(String)`

- **Type errors:**
  - `TypeMismatch(Type, Type)`
  - `UndefinedVariable(String)`
  - `VariableRedeclaration(String)`
  - `InvalidOperation(Operator, Type)`
  - `MissingReturnStatement`
  - `ArgumentCountMismatch { expected: usize, found: usize }`

- **Code generation errors:**
  - (Most codegen errors are caught during type checking)

#### `Severity`

Defines the importance level:

- **`Error`**: Compilation cannot continue (red)
- **`Warning`**: Potential issue, but compilation proceeds (yellow)
- **`Info`**: Informational message (blue)

### Error Display

Diagnostics are displayed using `ariadne`:

```
error: type mismatch
  ┌─ main.zr:5:13
  │
5 │     let x: i32 = "hello";
  │             ^^^ expected `i32`, found `str`
  │
  = note: expected type `i32`
          found type `str`
```

Features:
- **Source context**: Shows the relevant source line
- **Caret highlighting**: Points to the exact error location
- **Color coding**: Red for errors, yellow for warnings
- **Helpful notes**: Additional context and suggestions

### Span Tracking

The `zrc_utils::span` module provides span tracking:

- **`Span`**: Represents a range in the source (`start..end`)
- **`Spanned<T>`**: Wraps a value with its span
- Used throughout the pipeline to maintain source locations

### Error Conversion

Each stage converts its errors to `Diagnostic`:

- **Lexer:** `LexicalError` → `Diagnostic` (via `DiagnosticKind`)
- **Parser:** `ParseError` → `Diagnostic` (via `parser_error_to_diagnostic()`)
- **Type checker:** Returns `Diagnostic` directly
- **Codegen:** Panics on internal errors (type checker should prevent all errors)

### Extension Traits

Defined in `ext.rs`:

- **`SpanExt`**: Utility methods for `Span`
- **`SpannedExt`**: Utility methods for `Spanned<T>`

### Example

```rust
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_utils::span::Span;

let diagnostic = DiagnosticKind::TypeMismatch(Type::I32, Type::Str)
    .error_in(Span::from_positions(10, 15));

// Creates an error diagnostic:
// Diagnostic {
//     kind: TypeMismatch(I32, Str),
//     severity: Error,
//     span: Span { start: 10, end: 15 }
// }
```

### User-Facing Output

Diagnostics are printed to stderr with:
1. Error severity and message
2. File name and line/column numbers
3. Source code excerpt with highlighting
4. Helpful notes and suggestions (when available)

---

## Pipeline Flow Diagram

```
┌─────────────────┐
│  Source Code    │
│   (main.zr)     │
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Stage 1: Lexer                 │
│  (zrc_parser::lexer)            │
│                                 │
│  Input:  "fn main() {}"         │
│  Output: [Fn, Identifier,       │
│           LeftParen, ...]       │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Stage 2: Parser                │
│  (zrc_parser::parser)           │
│                                 │
│  Input:  Token stream           │
│  Output: AST (Vec<Declaration>) │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Stage 3: Type Checker          │
│  (zrc_typeck::typeck)           │
│                                 │
│  Input:  AST                    │
│  Output: TAST (Typed AST)       │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Stage 4: Code Generator        │
│  (zrc_codegen)                  │
│                                 │
│  Input:  TAST                   │
│  Output: LLVM IR / ASM / Object │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────┐
│  Output File    │
│  or stdout      │
└─────────────────┘

     Diagnostics (Stage 5)
     ═══════════════════════
     Errors at any stage are
     converted to Diagnostic
     and displayed to user
```

---

## Example: Complete Compilation

Let's trace a simple program through the entire pipeline:

### Source Code

```zirco
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

### Stage 1: Lexer Output

```
Tok::Fn (span 0..2)
Tok::Identifier("factorial") (span 3..12)
Tok::LeftParen (span 12..13)
Tok::Identifier("n") (span 13..14)
Tok::Colon (span 14..15)
Tok::I32 (span 16..19)
Tok::RightParen (span 19..20)
Tok::Arrow (span 21..23)
Tok::I32 (span 24..27)
Tok::LeftBrace (span 28..29)
... (and so on)
```

### Stage 2: Parser Output (AST)

```rust
Declaration::FunctionDeclaration {
    name: "factorial",
    parameters: [
        ArgumentDeclaration { name: "n", ty: Type::I32 }
    ],
    return_type: Type::I32,
    body: [
        Stmt::If {
            condition: Expr::Comparison(
                LessOrEqual,
                Expr::Identifier("n"),
                Expr::NumberLiteral(1)
            ),
            then_block: [
                Stmt::Return(Expr::NumberLiteral(1))
            ],
            else_block: Some([
                Stmt::Return(
                    Expr::Arithmetic(
                        Multiply,
                        Expr::Identifier("n"),
                        Expr::FunctionCall(
                            "factorial",
                            [Expr::Arithmetic(
                                Subtract,
                                Expr::Identifier("n"),
                                Expr::NumberLiteral(1)
                            )]
                        )
                    )
                )
            ])
        }
    ]
}
```

### Stage 3: Type Checker Output (TAST)

```rust
TypedDeclaration::FunctionDeclaration {
    name: "factorial",
    parameters: [
        ArgumentDeclaration { name: "n", ty: Type::I32 }
    ],
    return_type: Type::I32,
    body: [
        TypedStmt::If {
            condition: TypedExpr {
                kind: Comparison(
                    LessOrEqual,
                    TypedExpr { kind: Identifier("n"), inferred_type: I32 },
                    TypedExpr { kind: NumberLiteral(1), inferred_type: I32 }
                ),
                inferred_type: Bool
            },
            then_block: [
                TypedStmt::Return(TypedExpr {
                    kind: NumberLiteral(1),
                    inferred_type: I32
                })
            ],
            else_block: Some([
                TypedStmt::Return(TypedExpr {
                    kind: Arithmetic(
                        Multiply,
                        TypedExpr { kind: Identifier("n"), inferred_type: I32 },
                        TypedExpr {
                            kind: FunctionCall("factorial", [...]),
                            inferred_type: I32
                        }
                    ),
                    inferred_type: I32
                })
            ])
        }
    ]
}
```

### Stage 4: Code Generator Output (LLVM IR)

```llvm
define i32 @factorial(i32 %n) {
entry:
  %n.addr = alloca i32
  store i32 %n, i32* %n.addr
  %0 = load i32, i32* %n.addr
  %1 = icmp sle i32 %0, 1
  br i1 %1, label %then, label %else

then:
  ret i32 1

else:
  %2 = load i32, i32* %n.addr
  %3 = load i32, i32* %n.addr
  %4 = sub i32 %3, 1
  %5 = call i32 @factorial(i32 %4)
  %6 = mul i32 %2, %5
  ret i32 %6
}
```

### Final Output

Depending on `--emit` flag:
- **LLVM IR:** The IR shown above (human-readable)
- **Assembly:** Target-specific assembly code
- **Object:** Binary object file (`.o`) ready for linking

---

## Additional Resources

- **Language Specification:** See [SPEC.md](./SPEC.md) for Zirco language details
- **API Documentation:** Run `cargo doc --open` for detailed API docs
- **Contributing Guide:** See [CONTRIBUTING.md](../.github/CONTRIBUTING.md)
- **Source Code:**
  - Lexer: `compiler/zrc_parser/src/lexer.rs`
  - Parser: `compiler/zrc_parser/src/parser.rs` and `parser.lalrpop`
  - Type Checker: `compiler/zrc_typeck/src/typeck/`
  - Code Generator: `compiler/zrc_codegen/src/`
  - Diagnostics: `compiler/zrc_diagnostics/src/`

---

**Last Updated:** 2024

For questions or clarifications, please open an issue on the [GitHub repository](https://github.com/zirco-lang/zrc).
