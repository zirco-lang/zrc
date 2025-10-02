# Zirco Language Specification

Version 0.1.0 (Draft)

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Type System](#3-type-system)
4. [Expressions](#4-expressions)
5. [Statements](#5-statements)
6. [Declarations](#6-declarations)
7. [Functions](#7-functions)
8. [Semantics and Behavior](#8-semantics-and-behavior)

---

## 1. Introduction

### 1.1 About Zirco

Zirco is a compiled programming language with a strong type system and modern syntax. It uses C-like semantics with Rust-inspired syntax. The language is designed to provide a clean, expressive syntax while maintaining compatibility with systems-level programming requirements.

### 1.2 Design Philosophy

- **Strong Type System**: Zirco aims for an extremely strong runtime type system
- **Modern Syntax**: Combines the familiarity of C with modern language design
- **Compiled**: Compiles to native code via LLVM
- **Systems Programming**: Suitable for low-level systems programming tasks

### 1.3 Stability Notice

Zirco is currently in active development (version 0.1.0). There are **NO STABILITY GUARANTEES**. The language syntax, semantics, and APIs may change between versions.

---

## 2. Lexical Structure

### 2.1 Character Set

Zirco source files are UTF-8 encoded text files. The language uses ASCII for keywords and operators, but identifiers and string literals may contain Unicode characters.

### 2.2 Whitespace

Whitespace characters include:
- Space (U+0020)
- Tab (U+0009)
- Newline (U+000A)
- Carriage return (U+000D)

Whitespace is used to separate tokens but is otherwise ignored.

### 2.3 Comments

Zirco supports single-line comments:

```zirco
// This is a single-line comment
```

Comments start with `//` and continue to the end of the line. They are treated as whitespace and ignored by the compiler.

**Note**: Multi-line comments are not currently supported.

### 2.4 Keywords

The following identifiers are reserved keywords and cannot be used as variable or function names:

```
as          break       continue    default     do
else        false       fn          for         if
let         return      sizeof      struct      switch
true        type        union       while
```

### 2.5 Identifiers

Identifiers are used to name variables, functions, types, and other program entities.

**Syntax**:
```
identifier ::= [a-zA-Z_][a-zA-Z0-9_]*
```

**Rules**:
- Must start with a letter (uppercase or lowercase) or underscore
- May contain letters, digits, and underscores
- Case-sensitive

**Examples**:
```zirco
x
my_variable
MyType
_private
value123
```

### 2.6 Literals

#### 2.6.1 Integer Literals

Integer literals can be written in decimal, hexadecimal, or binary notation:

**Decimal**: `[0-9][0-9_]*`
```zirco
42
1000
1_000_000  // underscores for readability
```

**Hexadecimal**: `0x[0-9a-fA-F_]+`
```zirco
0xFF
0x1A2B
0xDEAD_BEEF
```

**Binary**: `0b[01_]+`
```zirco
0b1010
0b1111_0000
```

Underscores may be used as digit separators for readability and are ignored by the compiler.

#### 2.6.2 Type Suffixes

Integer literals may optionally have a type suffix to specify their type:

```zirco
42i32
0xFFu64
100i8
```

#### 2.6.3 Boolean Literals

Boolean literals are `true` and `false`:

```zirco
let x = true;
let y = false;
```

#### 2.6.4 String Literals

String literals are enclosed in double quotes and may contain escape sequences:

```zirco
"Hello, World!"
"Line 1\nLine 2"
"Tab\tseparated"
```

**Supported Escape Sequences**:
- `\n` - Newline
- `\r` - Carriage return
- `\t` - Tab
- `\\` - Backslash
- `\"` - Double quote

String literals must be closed on the same line (multi-line strings are not supported).

#### 2.6.5 Character Literals

Character literals are enclosed in single quotes and represent a single character:

```zirco
'a'
'Z'
'0'
'\n'
'\t'
```

Character literals support the same escape sequences as string literals.

### 2.7 Operators and Punctuation

Zirco uses the following operators and punctuation:

**Arithmetic Operators**:
```
+   -   *   /   %
```

**Comparison Operators**:
```
==  !=  <   <=  >   >=
```

**Logical Operators**:
```
&&  ||  !
```

**Bitwise Operators**:
```
&   |   ^   ~   <<  >>
```

**Assignment Operators**:
```
=   +=  -=  *=  /=  %=
&=  |=  ^=  <<=  >>=
```

**Other Operators**:
```
.   ->  ::  ?   :   as
```

**Delimiters**:
```
(   )   {   }   [   ]
,   ;   =>  ...
```

---

## 3. Type System

### 3.1 Type Overview

Zirco has a static type system. Every expression has a type known at compile time.

### 3.2 Type Syntax

Types in Zirco are written using the following syntax:

```
type ::= identifier
       | "*" type
       | "struct" "{" (identifier ":" type ("," identifier ":" type)*)? "}"
       | "union" "{" (identifier ":" type ("," identifier ":" type)*)? "}"
       | "(" type ")"
```

### 3.3 Primitive Types

While Zirco does not have built-in primitive types in the language specification, the following types are conventionally used and recognized by the compiler:

**Signed Integers**:
- `i8` - 8-bit signed integer
- `i16` - 16-bit signed integer
- `i32` - 32-bit signed integer
- `i64` - 64-bit signed integer

**Unsigned Integers**:
- `u8` - 8-bit unsigned integer
- `u16` - 16-bit unsigned integer
- `u32` - 32-bit unsigned integer
- `u64` - 64-bit unsigned integer

**Note**: The actual primitive types are implementation-defined and may vary.

### 3.4 Pointer Types

Pointer types are formed by prefixing a type with `*`:

```zirco
*i32        // pointer to i32
**u8        // pointer to pointer to u8
*MyStruct   // pointer to MyStruct
```

**Syntax**: `*T` where `T` is any type.

### 3.5 Struct Types

Structs are product types that group named fields together:

**Inline Struct Syntax**:
```zirco
struct { x: i32, y: i32 }
```

**Named Struct Declaration**:
```zirco
struct Point { x: i32, y: i32 }
```

The second form is syntactic sugar for:
```zirco
type Point = struct { x: i32, y: i32 };
```

**Rules**:
- Field names must be unique within a struct
- Fields are accessed using the `.` operator
- Structs can be nested

**Example**:
```zirco
struct Person {
    name: *u8,
    age: i32
}

struct Company {
    ceo: Person,
    employee_count: i32
}
```

### 3.6 Union Types

Unions are sum types where a value can be one of several different types:

**Inline Union Syntax**:
```zirco
union { i: i32, f: f32 }
```

**Named Union Declaration**:
```zirco
union Value { i: i32, f: f32 }
```

The second form is syntactic sugar for:
```zirco
type Value = union { i: i32, f: f32 };
```

**Rules**:
- Only one field is active at a time
- Fields are accessed using the `.` operator
- Reading an inactive field is undefined behavior

### 3.7 Type Aliases

Type aliases create alternate names for existing types:

```zirco
type MyInt = i32;
type PointerToInt = *i32;
type ComplexStruct = struct { a: i32, b: *u8 };
```

**Syntax**: `type Name = Type;`

Type aliases are transparent - they create a new name but not a new type. The alias and the original type are interchangeable.

### 3.8 Type Inference

Currently, Zirco has limited type inference. Variables declared with `let` may omit their type if they have an initializer, but the type inference mechanism is implementation-defined and may be restricted.

---

## 4. Expressions

### 4.1 Expression Overview

Expressions are combinations of values, variables, operators, and function calls that evaluate to a value.

### 4.2 Operator Precedence

Operators are listed from highest to lowest precedence:

| Precedence | Operators | Description | Associativity |
|------------|-----------|-------------|---------------|
| 1 | `x()` `x[]` `x.y` `x->y` | Function call, array index, member access | Left-to-right |
| 2 | `!x` `-x` `~x` `&x` `*x` | Unary operators | Right-to-left |
| 3 | `as` | Type cast | Left-to-right |
| 4 | `*` `/` `%` | Multiplication, division, modulo | Left-to-right |
| 5 | `+` `-` | Addition, subtraction | Left-to-right |
| 6 | `<<` `>>` | Bitwise shift | Left-to-right |
| 7 | `<` `<=` `>` `>=` | Comparison | Left-to-right |
| 8 | `==` `!=` | Equality | Left-to-right |
| 9 | `&` | Bitwise AND | Left-to-right |
| 10 | `^` | Bitwise XOR | Left-to-right |
| 11 | `\|` | Bitwise OR | Left-to-right |
| 12 | `&&` | Logical AND | Left-to-right |
| 13 | `\|\|` | Logical OR | Left-to-right |
| 14 | `? :` | Ternary conditional | Right-to-left |
| 15 | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` | Assignment | Right-to-left |
| 16 | `,` | Comma | Left-to-right |

### 4.3 Primary Expressions

#### 4.3.1 Literals

See section 2.6 for literal syntax.

#### 4.3.2 Identifiers

An identifier expression evaluates to the value bound to that name:

```zirco
x
my_variable
```

#### 4.3.3 Parenthesized Expressions

Expressions can be wrapped in parentheses to control evaluation order:

```zirco
(1 + 2) * 3
```

### 4.4 Arithmetic Expressions

**Binary Arithmetic Operators**:
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `%` Modulo

**Example**:
```zirco
let x = 10 + 5;
let y = 20 * 3 - 4;
let z = 15 / 3;
let remainder = 17 % 5;
```

**Unary Arithmetic Operator**:
- `-` Negation

```zirco
let x = -5;
let y = -(10 + 3);
```

### 4.5 Comparison Expressions

**Comparison Operators**:
- `>` Greater than
- `>=` Greater than or equal
- `<` Less than
- `<=` Less than or equal

**Equality Operators**:
- `==` Equal to
- `!=` Not equal to

**Example**:
```zirco
let a = 5 > 3;      // true
let b = 10 <= 10;   // true
let c = 7 == 7;     // true
let d = 5 != 3;     // true
```

**Rules**:
- Operands must be of the same type
- Result is a boolean value

### 4.6 Logical Expressions

**Logical Operators**:
- `&&` Logical AND
- `||` Logical OR
- `!` Logical NOT (unary)

**Example**:
```zirco
let a = true && false;   // false
let b = true || false;   // true
let c = !true;           // false
```

**Rules**:
- Operands must be boolean values
- `&&` and `||` use short-circuit evaluation

### 4.7 Bitwise Expressions

**Binary Bitwise Operators**:
- `&` Bitwise AND
- `|` Bitwise OR
- `^` Bitwise XOR
- `<<` Left shift
- `>>` Right shift

**Unary Bitwise Operator**:
- `~` Bitwise NOT

**Example**:
```zirco
let a = 0b1010 & 0b1100;   // 0b1000
let b = 0b1010 | 0b1100;   // 0b1110
let c = 0b1010 ^ 0b1100;   // 0b0110
let d = 0b0001 << 3;       // 0b1000
let e = 0b1000 >> 2;       // 0b0010
let f = ~0b1010;           // bitwise negation
```

### 4.8 Assignment Expressions

**Simple Assignment**:
```zirco
x = 5;
```

**Compound Assignment Operators**:
```zirco
x += 5;    // x = x + 5
x -= 3;    // x = x - 3
x *= 2;    // x = x * 2
x /= 4;    // x = x / 4
x %= 3;    // x = x % 3
x &= 0xFF; // x = x & 0xFF
x |= 0x0F; // x = x | 0x0F
x ^= 0xAA; // x = x ^ 0xAA
x <<= 2;   // x = x << 2
x >>= 1;   // x = x >> 1
```

**Rules**:
- Left side must be an lvalue (assignable location)
- Assignment is an expression and returns the assigned value
- Assignment is right-associative

### 4.9 Pointer Expressions

#### 4.9.1 Address-of Operator

The `&` operator takes the address of a value:

```zirco
let x = 42;
let ptr = &x;  // ptr has type *i32
```

#### 4.9.2 Dereference Operator

The `*` operator dereferences a pointer:

```zirco
let x = 42;
let ptr = &x;
let value = *ptr;  // value is 42
```

### 4.10 Member Access Expressions

#### 4.10.1 Dot Operator

The `.` operator accesses a member of a struct or union:

```zirco
struct Point { x: i32, y: i32 }

let p: Point;
let x_val = p.x;
p.y = 10;
```

#### 4.10.2 Arrow Operator

The `->` operator is shorthand for dereferencing a pointer and accessing a member:

```zirco
let ptr: *Point;
let x_val = ptr->x;  // equivalent to (*ptr).x
```

### 4.11 Index Expressions

The `[]` operator indexes into an array or pointer:

```zirco
let arr: *i32;
let value = arr[0];
arr[5] = 42;
```

### 4.12 Function Call Expressions

Function calls use parentheses with comma-separated arguments:

```zirco
printf("Hello, %d\n", 42);
let result = add(5, 3);
```

**Rules**:
- Arguments are evaluated left-to-right (evaluation order is defined)
- Number and types of arguments must match the function signature

### 4.13 Cast Expressions

The `as` operator casts an expression to a different type:

```zirco
let x = 42 as u8;
let ptr = value as *i32;
```

**Note**: Type casting behavior is implementation-defined.

### 4.14 Ternary Conditional Expression

The ternary operator `? :` provides conditional evaluation:

```zirco
let max = a > b ? a : b;
let sign = x >= 0 ? 1 : -1;
```

**Syntax**: `condition ? true_expr : false_expr`

**Rules**:
- Condition must be a boolean expression
- Both branches must have compatible types
- Only the selected branch is evaluated

### 4.15 Sizeof Expressions

The `sizeof` operator returns the size of a type or expression:

**Sizeof Type**:
```zirco
let size = sizeof i32;
let ptr_size = sizeof *u8;
```

**Sizeof Expression**:
```zirco
let x = 42;
let size = sizeof(x);
```

### 4.16 Comma Expression

The comma operator evaluates multiple expressions and returns the last one:

```zirco
let x = (a = 5, b = 10, a + b);  // x is 15
```

**Rules**:
- Expressions are evaluated left-to-right
- Result is the value of the rightmost expression
- Primarily used for side effects

---

## 5. Statements

### 5.1 Statement Overview

Statements are executable instructions that perform actions but do not produce values (unlike expressions).

### 5.2 Expression Statements

An expression followed by a semicolon:

```zirco
printf("Hello\n");
x = 42;
x + 5;  // computed but result discarded
```

### 5.3 Empty Statement

A semicolon by itself:

```zirco
;
```

### 5.4 Block Statements

A sequence of statements enclosed in braces:

```zirco
{
    let x = 5;
    let y = 10;
    printf("%d\n", x + y);
}
```

**Rules**:
- Variables declared in a block are scoped to that block
- Blocks can be nested
- Blocks can appear anywhere a statement is expected

### 5.5 Let Declarations (Local Variables)

The `let` keyword declares local variables:

**Basic Syntax**:
```zirco
let x;              // declare without initialization
let y: i32;         // declare with type
let z = 42;         // declare with initializer
let w: i32 = 100;   // declare with type and initializer
```

**Multiple Declarations**:
```zirco
let a = 1, b = 2, c = 3;
let x: i32, y: i32, z: i32;
```

**Rules**:
- Variables must be declared before use
- Multiple variables can be declared in a single statement
- Type can be inferred from initializer (implementation-defined)
- Uninitialized variables have indeterminate values

### 5.6 If Statements

Conditional execution based on a boolean expression:

**Basic If**:
```zirco
if (x > 0) {
    printf("positive\n");
}
```

**If-Else**:
```zirco
if (x > 0) {
    printf("positive\n");
} else {
    printf("non-positive\n");
}
```

**If-Else If-Else Chain**:
```zirco
if (x > 0) {
    printf("positive\n");
} else if (x < 0) {
    printf("negative\n");
} else {
    printf("zero\n");
}
```

**Rules**:
- Condition must be enclosed in parentheses
- Condition must be a boolean expression
- Braces are optional for single statements but recommended
- Dangling else attaches to the nearest if

### 5.7 While Loops

Repeat a statement while a condition is true:

```zirco
while (i < 10) {
    printf("%d\n", i);
    i = i + 1;
}
```

**Rules**:
- Condition is evaluated before each iteration
- Loop body may execute zero or more times
- Condition must be enclosed in parentheses

### 5.8 Do-While Loops

Like while loops, but condition is checked after each iteration:

```zirco
do {
    printf("%d\n", i);
    i = i + 1;
} while (i < 10);
```

**Rules**:
- Loop body executes at least once
- Condition is evaluated after each iteration
- Must end with a semicolon after the condition

### 5.9 For Loops

Loops with initialization, condition, and increment:

```zirco
for (let i = 0; i < 10; i = i + 1) {
    printf("%d\n", i);
}
```

**Alternative Forms**:
```zirco
for (; i < 10; i = i + 1) {  // no initialization
    // ...
}

for (let i = 0; ; i = i + 1) {  // no condition (infinite loop)
    // ...
}

for (let i = 0; i < 10; ) {  // no increment
    // ...
}

for (;;) {  // infinite loop
    // ...
}
```

**Syntax**: `for (init; condition; post) body`

**Rules**:
- Initialization runs once before the loop starts
- Condition is checked before each iteration
- Post-expression runs after each iteration
- All three parts are optional
- Variables declared in init are scoped to the loop

### 5.10 Break Statement

Exit the innermost loop:

```zirco
while (true) {
    if (done) {
        break;
    }
}
```

### 5.11 Continue Statement

Skip to the next iteration of the innermost loop:

```zirco
for (let i = 0; i < 10; i = i + 1) {
    if (i % 2 == 0) {
        continue;  // skip even numbers
    }
    printf("%d\n", i);
}
```

### 5.12 Return Statement

Return from a function:

**Return Void**:
```zirco
return;
```

**Return Value**:
```zirco
return 42;
return x + y;
```

**Rules**:
- Return type must match function signature
- Void functions use `return;` or omit return at the end
- Functions with return types must return a value on all paths

### 5.13 Switch Statement

Multi-way branch based on a value:

```zirco
switch (x) {
    1 => printf("one\n");
    2 => printf("two\n");
    3 => {
        printf("three\n");
        printf("is the magic number\n");
    }
    default => printf("other\n");
}
```

**Rules**:
- Each case uses `=>` syntax (fat arrow)
- Cases can match expressions, not just constants
- `default` case handles all unmatched values
- No fall-through between cases
- Each case body is a single statement (use blocks for multiple statements)

---

## 6. Declarations

### 6.1 Declaration Overview

Declarations introduce new names into the program at the global scope.

### 6.2 Function Declarations

See section 7 for complete function declaration syntax and semantics.

### 6.3 Type Alias Declarations

Type aliases create new names for existing types:

```zirco
type Int32 = i32;
type StringPtr = *u8;
type Point2D = struct { x: i32, y: i32 };
```

**Syntax**: `type Name = Type;`

### 6.4 Struct Declarations

Named struct types:

```zirco
struct Point {
    x: i32,
    y: i32
}
```

This is equivalent to:
```zirco
type Point = struct { x: i32, y: i32 };
```

### 6.5 Union Declarations

Named union types:

```zirco
union Value {
    i: i32,
    f: f32,
    b: u8
}
```

This is equivalent to:
```zirco
type Value = union { i: i32, f: f32, b: u8 };
```

---

## 7. Functions

### 7.1 Function Declarations

**Basic Function**:
```zirco
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

**Void Function** (no return type):
```zirco
fn print_hello() {
    printf("Hello!\n");
}
```

**Syntax**: `fn name(parameters) -> return_type { body }`

### 7.2 Function Parameters

Parameters are declared with a name and type:

```zirco
fn multiply(x: i32, y: i32) -> i32 {
    return x * y;
}
```

**Multiple Parameters**:
```zirco
fn complex_function(a: i32, b: *u8, c: Point) -> i32 {
    // ...
}
```

**Rules**:
- Each parameter must have a type annotation
- Parameters are separated by commas
- Parameters are local to the function
- Trailing comma is allowed

### 7.3 Variadic Functions

Functions can accept a variable number of arguments using `...`:

```zirco
fn printf(format: *u8, ...) -> i32;
```

**Rules**:
- `...` must be the last parameter
- Must have at least one non-variadic parameter
- Accessing variadic arguments is implementation-defined

### 7.4 Function Calls

```zirco
let result = add(5, 3);
print_hello();
printf("Value: %d\n", x);
```

### 7.5 External Declarations

Functions can be declared without a body to indicate they are defined externally:

```zirco
fn printf(format: *u8, ...) -> i32;
fn malloc(size: u64) -> *u8;
```

These are typically used to interface with C libraries.

**Rules**:
- External declarations end with a semicolon instead of a body
- Must match the actual external function signature
- No body is provided

### 7.6 Return Types

**Explicit Return Type**:
```zirco
fn get_value() -> i32 {
    return 42;
}
```

**Void Return** (omit `-> type`):
```zirco
fn do_something() {
    printf("done\n");
}
```

### 7.7 Function Examples

**Simple Function**:
```zirco
fn square(x: i32) -> i32 {
    return x * x;
}
```

**Multiple Returns**:
```zirco
fn absolute(x: i32) -> i32 {
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}
```

**Struct Return**:
```zirco
fn make_point(x: i32, y: i32) -> Point {
    let p: Point;
    p.x = x;
    p.y = y;
    return p;
}
```

---

## 8. Semantics and Behavior

### 8.1 Memory Model

Zirco follows a C-like memory model:

- Variables are stored in memory locations
- Pointers hold memory addresses
- Memory can be accessed through pointers
- Memory safety is not guaranteed by the language

### 8.2 Type Compatibility

- Types must match exactly in most contexts
- Implicit conversions are not performed
- Explicit casts using `as` are required to convert between types

### 8.3 Evaluation Order

**Expression Evaluation**:
- Function arguments are evaluated left-to-right
- Most other evaluation orders are implementation-defined

**Short-Circuit Evaluation**:
- `&&` and `||` use short-circuit evaluation
- The right operand is not evaluated if the result is determined by the left operand

### 8.4 Undefined Behavior

The following behaviors are undefined:

- Reading uninitialized variables
- Dereferencing invalid pointers
- Accessing arrays out of bounds
- Reading an inactive union field
- Division by zero
- Integer overflow (behavior is implementation-defined)

### 8.5 Scope and Lifetime

**Scope Rules**:
- Variables are scoped to the block in which they are declared
- Function parameters are scoped to the function body
- Global declarations have file scope

**Lifetime**:
- Local variables exist from declaration until the end of their scope
- Dynamic lifetime management is implementation-defined

### 8.6 Name Resolution

- Names are resolved in the innermost scope first
- Outer scopes are searched if a name is not found in the current scope
- Global names can be accessed from any scope unless shadowed

### 8.7 Compilation Model

Zirco is compiled to native code via LLVM:

1. **Lexing**: Source code is tokenized
2. **Parsing**: Tokens are parsed into an Abstract Syntax Tree (AST)
3. **Type Checking**: Types are checked and a Typed AST (TAST) is generated
4. **Code Generation**: LLVM IR is generated from the TAST
5. **Optimization**: LLVM optimizes the IR
6. **Binary Generation**: Native code is produced

### 8.8 Linkage

- Functions can be declared as external to link with C libraries
- The linker combines compiled object files into executables
- Linking behavior follows the platform's standard

### 8.9 Program Entry Point

By convention, programs begin execution at a function named `main`:

```zirco
fn main() {
    // program starts here
}
```

The signature of `main` is implementation-defined.

### 8.10 Future Directions

The following features are planned or under consideration:

- Arrays with first-class syntax
- Generics/parametric polymorphism
- Pattern matching
- Modules and namespaces
- Enhanced type inference
- Traits/interfaces
- Enhanced safety features

---

## Appendix A: Grammar Summary

This section provides a high-level grammar summary. For complete details, refer to the full specification sections.

### A.1 Lexical Grammar

```
keyword ::= "as" | "break" | "continue" | "default" | "do" | "else" 
          | "false" | "fn" | "for" | "if" | "let" | "return" 
          | "sizeof" | "struct" | "switch" | "true" | "type" 
          | "union" | "while"

identifier ::= [a-zA-Z_][a-zA-Z0-9_]*

decimal_literal ::= [0-9][0-9_]*
hex_literal ::= "0x" [0-9a-fA-F_]+
binary_literal ::= "0b" [01_]+

string_literal ::= '"' ([^"\\\n] | escape_sequence)* '"'
char_literal ::= "'" ([^'\\\n] | escape_sequence) "'"

escape_sequence ::= "\n" | "\r" | "\t" | "\\" | "\"" | "\'"
```

### A.2 Expression Grammar

```
expr ::= primary
       | unary_op expr
       | expr binary_op expr
       | expr "?" expr ":" expr
       | expr "as" type
       | expr "(" argument_list? ")"
       | expr "[" expr "]"
       | expr "." identifier
       | expr "->" identifier

primary ::= literal
          | identifier
          | "(" expr ")"
          | "sizeof" type
          | "sizeof" "(" expr ")"
```

### A.3 Statement Grammar

```
stmt ::= expr ";"
       | ";"
       | "{" stmt* "}"
       | "let" let_declaration ("," let_declaration)* ";"
       | "if" "(" expr ")" stmt ("else" stmt)?
       | "while" "(" expr ")" stmt
       | "do" stmt "while" "(" expr ")" ";"
       | "for" "(" for_init? ";" expr? ";" expr? ")" stmt
       | "switch" "(" expr ")" "{" switch_case* "}"
       | "break" ";"
       | "continue" ";"
       | "return" expr? ";"
```

### A.4 Declaration Grammar

```
declaration ::= function_declaration
              | type_alias_declaration
              | struct_declaration
              | union_declaration

function_declaration ::= "fn" identifier "(" parameter_list? ")" ("->" type)? (block | ";")

type_alias_declaration ::= "type" identifier "=" type ";"

struct_declaration ::= "struct" identifier "{" field_list? "}"

union_declaration ::= "union" identifier "{" field_list? "}"
```

### A.5 Type Grammar

```
type ::= identifier
       | "*" type
       | "struct" "{" field_list? "}"
       | "union" "{" field_list? "}"
       | "(" type ")"

field_list ::= identifier ":" type ("," identifier ":" type)*
```

---

## Appendix B: Operator Precedence Table

See section 4.2 for the complete operator precedence table.

---

## Appendix C: Keywords Reference

See section 2.4 for the complete list of reserved keywords.

---

## Appendix D: Examples

### D.1 Hello World

```zirco
fn printf(format: *u8, ...) -> i32;

fn main() {
    printf("Hello, World!\n");
}
```

### D.2 Fibonacci

```zirco
fn fibonacci(n: i32) -> i32 {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

### D.3 Struct Example

```zirco
struct Point {
    x: i32,
    y: i32
}

fn distance_squared(p1: Point, p2: Point) -> i32 {
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;
    return dx * dx + dy * dy;
}
```

### D.4 Pointer Example

```zirco
fn swap(a: *i32, b: *i32) {
    let temp = *a;
    *a = *b;
    *b = temp;
}
```

### D.5 Loop Example

```zirco
fn print_numbers(n: i32) {
    for (let i = 1; i <= n; i = i + 1) {
        printf("%d\n", i);
    }
}
```

---

## Appendix E: Terminology

- **AST**: Abstract Syntax Tree - the tree representation of source code structure
- **TAST**: Typed Abstract Syntax Tree - AST with type information
- **LLVM**: Low Level Virtual Machine - the compiler backend used by Zirco
- **lvalue**: An expression that refers to a memory location
- **rvalue**: An expression that produces a value
- **Short-circuit evaluation**: Evaluation strategy where the second operand is not evaluated if the result is determined by the first

---

## Appendix F: Document History

- **Version 0.1.0 (Draft)**: Initial language specification

---

*This specification is subject to change as Zirco evolves. Last updated: 2024*
