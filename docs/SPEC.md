# Zirco Language Specification

Version 0.1.0 (Draft)

## Table of Contents

1. [Introduction](#1-introduction)
    - [About Zirco](#11-about-zirco)
    - [Design Philosophy](#12-design-philosophy)
    - [Stability Notice](#13-stability-notice)
2. [Lexical Structure](#2-lexical-structure)
    - [Character Set](#21-character-set)
    - [Whitespace](#22-whitespace)
    - [Comments](#23-comments)
    - [Preprocessor](#24-preprocessor)
        - [Include Directive](#241-include-directive)
        - [Pragma Once](#242-pragma-once)
    - [Keywords](#25-keywords)
    - [Identifiers](#26-identifiers)
    - [Literals](#27-literals)
    - [Operators and Punctuation](#28-operators-and-punctuation)
3. [Type System](#3-type-system)
    - [Type Overview](#31-type-overview)
    - [Type Syntax](#32-type-syntax)
    - [Primitive Types](#33-primitive-types)
    - [Pointer Types](#34-pointer-types)
    - [Struct Types](#35-struct-types)
    - [Union Types](#36-union-types)
    - [Enum Types](#37-enum-types)
    - [Type Aliases](#38-type-aliases)
    - [Type Inference and Implicit Conversions](#39-type-inference-and-implicit-conversions)
4. [Expressions](#4-expressions)
    - [Expression Overview](#41-expression-overview)
    - [Operator Precedence](#42-operator-precedence)
    - [Primary Expressions](#43-primary-expressions)
    - [Arithmetic Expressions](#44-arithmetic-expressions)
    - [Comparison Expressions](#45-comparison-expressions)
    - [Logical Expressions](#46-logical-expressions)
    - [Bitwise Expressions](#47-bitwise-expressions)
    - [Assignment Expressions](#48-assignment-expressions)
    - [Pointer Expressions](#49-pointer-expressions)
    - [Member Access Expressions](#410-member-access-expressions)
    - [Index Expressions](#411-index-expressions)
    - [Function Call Expressions](#412-function-call-expressions)
    - [Cast Expressions](#413-cast-expressions)
    - [Ternary Conditional Expression](#414-ternary-conditional-expression)
    - [Sizeof Expressions](#415-sizeof-expressions)
    - [Comma Expression](#416-comma-expression)
    - [Increment and Decrement Expressions](#417-increment-and-decrement-expressions)
5. [Statements](#5-statements)
    - [Statement Overview](#51-statement-overview)
    - [Expression Statements](#52-expression-statements)
    - [Empty Statement](#53-empty-statement)
    - [Block Statements](#54-block-statements)
    - [Let Declarations (Local Variables)](#55-let-declarations-local-variables)
    - [If Statements](#56-if-statements)
    - [While Loops](#57-while-loops)
    - [Do-While Loops](#58-do-while-loops)
    - [For Loops](#59-for-loops)
    - [Four Loops](#59b-four-loops)
    - [Break Statement](#510-break-statement)
    - [Continue Statement](#511-continue-statement)
    - [Return Statement](#512-return-statement)
    - [Switch Statement](#513-switch-statement)
    - [Match Statement](#514-match-statement]
    - [Optimization Hints](#515-optimization-hints)
        - [Unreachable Statement](#5151-unreachable-statement)
6. [Declarations](#6-declarations)
    - [Declaration Overview](#61-declaration-overview)
    - [Function Declarations](#62-function-declarations)
    - [Type Alias Declarations](#63-type-alias-declarations)
    - [Struct Declarations](#64-struct-declarations)
    - [Union Declarations](#65-union-declarations)
    - [Global Let Declarations](#66-global-let-declarations)
7. [Functions](#7-functions)
    - [Function Declarations](#71-function-declarations)
    - [Function Parameters](#72-function-parameters)
    - [Variadic Functions](#73-variadic-functions)
    - [Function Calls](#74-function-calls)
    - [External Declarations](#75-external-declarations)
    - [Return Types](#76-return-types)
    - [Function Examples](#77-function-examples)
8. [Semantics and Behavior](#8-semantics-and-behavior)
    - [Memory Model](#81-memory-model)
    - [Type Compatibility](#82-type-compatibility)
    - [Evaluation Order](#83-evaluation-order)
    - [Undefined Behavior](#84-undefined-behavior)
    - [Scope and Lifetime](#85-scope-and-lifetime)
    - [Name Resolution](#86-name-resolution)
    - [Linkage](#87-linkage)
    - [Program Entry Point](#88-program-entry-point)
    - [Future Directions](#89-future-directions)

---

## 1. Introduction

### 1.1 About Zirco

Zirco is a compiled programming language with a strong type system and modern syntax. It uses C-like semantics with Rust-inspired syntax. The language is designed to provide a clean, expressive syntax while maintaining compatibility with systems-level programming requirements.

### 1.2 Design Philosophy

-   **Strong Type System**: Zirco aims for an extremely strong runtime type system
-   **Modern Syntax**: Combines the familiarity of C with modern language design
-   **Compiled**: Compiles to native code via LLVM
-   **Systems Programming**: Suitable for low-level systems programming tasks

### 1.3 Stability Notice

Zirco is currently in active development (version 0.1.0). There are **NO STABILITY GUARANTEES**. The language syntax, semantics, and APIs may change between versions.

---

## 2. Lexical Structure

### 2.1 Character Set

Zirco source files are UTF-8 encoded text files. The language uses ASCII for keywords and operators, but identifiers and string literals may contain Unicode characters.

### 2.2 Whitespace

Whitespace characters include:

-   Space (U+0020)
-   Tab (U+0009)
-   Newline (U+000A)
-   Carriage return (U+000D)

Whitespace is used to separate tokens but is otherwise ignored.

### 2.3 Comments

Zirco supports single-line comments:

```zirco
// This is a single-line comment
```

Comments start with `//` and continue to the end of the line. They are treated as whitespace and ignored by the compiler.

**Note**: Multi-line comments are not currently supported.

### 2.4 Preprocessor

Zirco includes a preprocessor that processes directives before compilation. Preprocessor directives are lines that start with `#`.

#### 2.4.1 Include Directive

The `#include` directive inserts the contents of another file:

```zirco
#include "header.zr"
#include <system/header.zr>
```

-   Files enclosed in double quotes (`"file.zr"`) are searched relative to the current file's directory
-   Files enclosed in angle brackets (`<file.zr>`) follow the same search path as double quotes

#### 2.4.2 Pragma Once

The `#pragma once` directive prevents a file from being included multiple times:

```zirco
#pragma once

// File contents here
```

When a file with `#pragma once` is included, the preprocessor remembers it and skips subsequent includes of the same file.

**Example**:

**types.zr**:

```zirco
#pragma once

struct Point {
    x: i32,
    y: i32,
}
```

**main.zr**:

```zirco
#include "types.zr"
#include "types.zr"  // This include will be skipped

fn main() {
    let p: Point = Point { x: 0, y: 0 };
}
```

### 2.5 Keywords

The following identifiers are reserved keywords and cannot be used as variable or function names:

```
as          break       continue    default     do
else        false       fn          for         if
let         return      sizeof      struct      switch
true        type        union       while       four
```

### 2.6 Identifiers

Identifiers are used to name variables, functions, types, and other program entities.

**Syntax**:

```
identifier ::= [a-zA-Z_][a-zA-Z0-9_]*
```

**Rules**:

-   Must start with a letter (uppercase or lowercase) or underscore
-   May contain letters, digits, and underscores
-   Case-sensitive

**Examples**:

```zirco
x
my_variable
MyType
_private
value123
```

### 2.7 Literals

#### 2.8.1 Integer Literals

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

#### 2.8.2 Type Suffixes

Integer literals may optionally have a type suffix to specify their type:

```zirco
42i32
0xFFu64
100i8
```

#### 2.8.3 Boolean Literals

Boolean literals are `true` and `false`:

```zirco
let x = true;
let y = false;
```

#### 2.8.4 String Literals

String literals are enclosed in double quotes and may contain escape sequences:

```zirco
"Hello, World!"
"Line 1\nLine 2"
"Tab\tseparated"
```

**Supported Escape Sequences**:

-   `\n` - Newline
-   `\r` - Carriage return
-   `\t` - Tab
-   `\\` - Backslash
-   `\"` - Double quote

String literals must be closed on the same line (multi-line strings are not supported).

#### 2.8.5 Character Literals

Character literals are enclosed in single quotes and represent a single character:

```zirco
'a'
'Z'
'0'
'\n'
'\t'
```

Character literals support the same escape sequences as string literals.

### 2.8 Operators and Punctuation

Zirco uses the following operators and punctuation:

**Arithmetic Operators**:

```
+   -   *   /   %   ++  --
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

Zirco has the following built-in primitive types that are explicitly defined by the compiler:

**Signed Integers**:

-   `i8` - 8-bit signed integer
-   `i16` - 16-bit signed integer
-   `i32` - 32-bit signed integer
-   `i64` - 64-bit signed integer
-   `isize` - Pointer-sized signed integer

**Unsigned Integers**:

-   `u8` - 8-bit unsigned integer
-   `u16` - 16-bit unsigned integer
-   `u32` - 32-bit unsigned integer
-   `u64` - 64-bit unsigned integer
-   `usize` - Pointer-sized unsigned integer

**Boolean**:

-   `bool` - Boolean type (true or false)

**Unit/Void Type**:

-   `struct {}` - Empty struct, used as the unit/void type
-   `*struct {}` - Pointer to empty struct, used as a void pointer (analogous to `void*` in C)

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

-   Field names must be unique within a struct
-   Fields are accessed using the `.` operator
-   Structs can be nested

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

-   Only one field is active at a time
-   Fields are accessed using the `.` operator
-   Reading an inactive field is undefined behavior

### 3.7 Enum Types

Enums are tagged unions, or sum types where a value must be _verified_ to be one of
several types before use.

**Union Declaration:**

```zirco
union MyUnion {
   // name each variant here...
   Variant1: i32,
   Variant2: struct { x: i32, y: i64 },
}
```

These variants are used in the match statements.

### 3.8 Type Aliases

Type aliases create alternate names for existing types:

```zirco
type MyInt = i32;
type PointerToInt = *i32;
type ComplexStruct = struct { a: i32, b: *u8 };
```

**Syntax**: `type Name = Type;`

Type aliases are transparent - they create a new name but not a new type. The alias and the original type are interchangeable.

### 3.9 Type Inference and Implicit Conversions

**Type Inference**:

Currently, Zirco has limited type inference. Variables declared with `let` may omit their type if they have an initializer, but the type inference mechanism is implementation-defined and may be restricted.

**Implicit Conversions**:

Zirco allows the following implicit type conversions:

1. **Pointer to Void Pointer**: Any pointer type `*T` can be implicitly converted to a void pointer `*struct {}`. This is useful for generic pointer handling.

    ```zirco
    fn takes_void_ptr(ptr: *struct {});

    let x: i32;
    takes_void_ptr(&x);  // *i32 implicitly converts to *struct {}
    ```

2. **Untyped Integer Literals**: Integer literals without a type suffix can be implicitly converted to any integer type when used in contexts where the target type is known (implementation-defined behavior).

Note: Implicit conversions only apply in specific contexts such as function arguments. Most operations require explicit type matching or explicit casts using the `as` operator.

---

## 4. Expressions

### 4.1 Expression Overview

Expressions are combinations of values, variables, operators, and function calls that evaluate to a value.

### 4.2 Operator Precedence

Operators are listed from highest to lowest precedence:

| Precedence | Operators                                                | Description                                                            | Associativity |
| ---------- | -------------------------------------------------------- | ---------------------------------------------------------------------- | ------------- |
| 1          | `x()` `x[]` `x.y` `x->y` `x++` `x--`                     | Function call, array index, member access, postfix increment/decrement | Left-to-right |
| 2          | `!x` `-x` `~x` `&x` `*x` `++x` `--x`                     | Unary operators, prefix increment/decrement                            | Right-to-left |
| 3          | `as`                                                     | Type cast                                                              | Left-to-right |
| 4          | `*` `/` `%`                                              | Multiplication, division, modulo                                       | Left-to-right |
| 5          | `+` `-`                                                  | Addition, subtraction                                                  | Left-to-right |
| 6          | `<<` `>>`                                                | Bitwise shift                                                          | Left-to-right |
| 7          | `<` `<=` `>` `>=`                                        | Comparison                                                             | Left-to-right |
| 8          | `==` `!=`                                                | Equality                                                               | Left-to-right |
| 9          | `&`                                                      | Bitwise AND                                                            | Left-to-right |
| 10         | `^`                                                      | Bitwise XOR                                                            | Left-to-right |
| 11         | `\|`                                                     | Bitwise OR                                                             | Left-to-right |
| 12         | `&&`                                                     | Logical AND                                                            | Left-to-right |
| 13         | `\|\|`                                                   | Logical OR                                                             | Left-to-right |
| 14         | `? :`                                                    | Ternary conditional                                                    | Right-to-left |
| 15         | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` | Assignment                                                             | Right-to-left |
| 16         | `,`                                                      | Comma                                                                  | Left-to-right |

### 4.3 Primary Expressions

#### 4.3.1 Literals

See [section 2.6](#26-literals) for literal syntax.

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

-   `+` Addition
-   `-` Subtraction
-   `*` Multiplication
-   `/` Division
-   `%` Modulo

**Example**:

```zirco
let x = 10 + 5;
let y = 20 * 3 - 4;
let z = 15 / 3;
let remainder = 17 % 5;
```

**Unary Arithmetic Operator**:

-   `-` Negation

```zirco
let x = -5;
let y = -(10 + 3);
```

### 4.5 Comparison Expressions

**Comparison Operators**:

-   `>` Greater than
-   `>=` Greater than or equal
-   `<` Less than
-   `<=` Less than or equal

**Equality Operators**:

-   `==` Equal to
-   `!=` Not equal to

**Example**:

```zirco
let a = 5 > 3;      // true
let b = 10 <= 10;   // true
let c = 7 == 7;     // true
let d = 5 != 3;     // true
```

**Rules**:

-   Operands must be of the same type
-   Result is a boolean value

### 4.6 Logical Expressions

**Logical Operators**:

-   `&&` Logical AND
-   `||` Logical OR
-   `!` Logical NOT (unary)

**Example**:

```zirco
let a = true && false;   // false
let b = true || false;   // true
let c = !true;           // false
```

**Rules**:

-   Operands must be boolean values
-   `&&` and `||` use short-circuit evaluation

### 4.7 Bitwise Expressions

**Binary Bitwise Operators**:

-   `&` Bitwise AND
-   `|` Bitwise OR
-   `^` Bitwise XOR
-   `<<` Left shift
-   `>>` Right shift

**Unary Bitwise Operator**:

-   `~` Bitwise NOT

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

-   Left side must be an lvalue (assignable location)
-   Assignment is an expression and returns the assigned value
-   Assignment is right-associative

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

The `[]` operator indexes into an array or pointer, accepting a `usize` index:

```zirco
let arr: *i32;
let value = arr[0];
arr[5] = 42;
```

**Rules**:

-   The index must be of type `usize`
-   Indexing is equivalent to pointer arithmetic and dereferencing

### 4.12 Function Call Expressions

Function calls use parentheses with comma-separated arguments:

```zirco
printf("Hello, %d\n", 42);
let result = add(5, 3);
```

**Rules**:

-   Arguments are evaluated left-to-right (evaluation order is defined)
-   Number and types of arguments must match the function signature

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

-   Condition must be a boolean expression
-   Both branches must have compatible types
-   Only the selected branch is evaluated

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

-   Expressions are evaluated left-to-right
-   Result is the value of the rightmost expression
-   Primarily used for side effects

### 4.17 Increment and Decrement Expressions

Zirco supports both prefix and postfix increment and decrement operators for integer types.

**Postfix Operators**:

-   `x++` - Postfix increment: returns the current value of `x`, then increments `x`
-   `x--` - Postfix decrement: returns the current value of `x`, then decrements `x`

**Prefix Operators**:

-   `++x` - Prefix increment: increments `x`, then returns the new value
-   `--x` - Prefix decrement: decrements `x`, then returns the new value

**Example**:

```zirco
let x = 5;

let a = x++;  // a is 5, x is now 6
let b = x--;  // b is 6, x is now 5
let c = ++x;  // x is now 6, c is 6
let d = --x;  // x is now 5, d is 5

// Common use in loops
for (let i = 0; i < 10; i++) {
    printf("%d\n", i);
}
```

**Rules**:

-   Operand must be an integer lvalue (a modifiable variable)
-   Cannot be used on constants or expressions
-   Prefix operators have unary precedence (level 2)
-   Postfix operators have the highest precedence (level 1)
-   The value is incremented or decremented by 1

**Parsing Disambiguation**:

-   `a+++b` is parsed as `(a++) + b` (maximal munch rule)
-   Use spacing to disambiguate: `a+ ++b` parses as `a + (++b)`

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

-   Variables declared in a block are scoped to that block
-   Blocks can be nested
-   Blocks can appear anywhere a statement is expected

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

-   Variables must be declared before use
-   Multiple variables can be declared in a single statement
-   Type can be inferred from initializer (implementation-defined)
-   Uninitialized variables have indeterminate values

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

-   Condition must be enclosed in parentheses
-   Condition must be a boolean expression
-   Braces are optional for single statements but recommended
-   Dangling else attaches to the nearest if

### 5.7 While Loops

Repeat a statement while a condition is true:

```zirco
while (i < 10) {
    printf("%d\n", i);
    i = i + 1;
}
```

**Rules**:

-   Condition is evaluated before each iteration
-   Loop body may execute zero or more times
-   Condition must be enclosed in parentheses

### 5.8 Do-While Loops

Like while loops, but condition is checked after each iteration:

```zirco
do {
    printf("%d\n", i);
    i = i + 1;
} while (i < 10);
```

**Rules**:

-   Loop body executes at least once
-   Condition is evaluated after each iteration
-   Must end with a semicolon after the condition

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

-   Initialization runs once before the loop starts
-   Condition is checked before each iteration
-   Post-expression runs after each iteration
-   All three parts are optional
-   Variables declared in init are scoped to the loop

### 5.9b Four Loops

A specialized loop for iterating four times, and exactly four times:

```zirco
four printf("This will print four times.\n");
four {
    printf("This will print four times.\n");
}
```

**Rules**:

-   The loop body executes exactly four times
-   `break` and `continue` statements may be used within the loop to alter control flow
-   No loop variable is provided; use an external counter if needed

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

-   Return type must match function signature
-   Void functions use `return;` or omit return at the end
-   Functions with return types must return a value on all paths

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

-   Each case uses `=>` syntax (fat arrow)
-   Cases can match expressions, not just constants
-   `default` case handles all unmatched values
-   No fall-through between cases
-   Each case body is a single statement (use blocks for multiple statements)

### 5.14 Match Statement

Multi-way branch based on the discriminant of an enum:

```zirco
match (x) {
   // these represent variants of enum [typeof x]
   VariantA: x => f(x),
   VariantB: y => g(y),
   VariantC: x => h(x), // same names can be used
}
```

**Rules**:

-   Each case uses `=>` syntax (fat arrow)
-   No fall-through between cases
-   Each case body is a single statement (use blocks for multiple statements)
-   There must be exactly one case per variant

### 5.15 Optimization Hints

Optimization hints provide additional information to the compiler. If used incorrectly, they may lead to undefined behavior.

#### 5.15.1 Unreachable Statement

The `unreachable` statement indicates that a certain point in the code should never be reached:

```zirco
fn example(x: i32) -> i32 {
    if (x >= 0) {
        return x;
    } else if (x < 0) {
        return -x;
    } else {
        unreachable;  // x cannot be anything other than >0, <0, or ==0
    }
}
```

Reaching an `unreachable` statement results in undefined behavior.

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

### 6.6 Global Let Declarations

Global variables can be declared at the file scope using `let`:

```zirco
let MAX_SIZE: i32 = 1000;
let DEBUG_MODE: bool = true;
let VERSION: i8 = 1i8;
```

**Syntax**: `let name: type = initializer;`

**Rules**:

-   Global variables must be declared at file scope (not inside functions)
-   Type annotation is required (no type inference for globals)
-   Initializer must be a constant expression (literals only)
-   Supported constant expressions:
    -   Number literals: `42`, `0xFF`, `0b1010`, `42i8`
    -   Boolean literals: `true`, `false`
    -   Character literals: `'a'`, `'\n'`
    -   String literals: `"hello"`
-   Variables without initializers are zero-initialized

**Multiple Declarations**:

```zirco
let x: i32 = 10, y: i32 = 20, z: i32 = 30;
```

**Zero-Initialized Variable**:

```zirco
let counter: i32;  // initialized to 0
```

**Example with Usage**:

```zirco
let MAX_RETRIES: i32 = 5;
let error_count: i32;

fn increment_errors() {
    error_count = error_count + 1;
}

fn should_retry() -> bool {
    return error_count < MAX_RETRIES;
}
```

**Restrictions**:

-   Initializers cannot reference other variables
-   Initializers cannot call functions
-   Initializers cannot use arithmetic or other operations
-   Global variables are always mutable

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

-   Each parameter must have a type annotation
-   Parameters are separated by commas
-   Parameters are local to the function
-   Trailing comma is allowed

### 7.3 Variadic Functions

Functions can accept a variable number of arguments using `...`:

```zirco
fn printf(format: *u8, ...) -> i32;
```

**Rules**:

-   `...` must be the last parameter
-   Must have at least one non-variadic parameter
-   Accessing variadic arguments is implementation-defined

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

-   External declarations end with a semicolon instead of a body
-   Must match the actual external function signature
-   No body is provided

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

-   Variables are stored in memory locations
-   Pointers hold memory addresses
-   Memory can be accessed through pointers
-   Memory safety is not guaranteed by the language

### 8.2 Type Compatibility

-   Types must match exactly in most contexts
-   Implicit conversions are not performed
-   Explicit casts using `as` are required to convert between types

### 8.3 Evaluation Order

**Expression Evaluation**:

-   Function arguments are evaluated left-to-right
-   Most other evaluation orders are implementation-defined

**Short-Circuit Evaluation**:

-   `&&` and `||` use short-circuit evaluation
-   The right operand is not evaluated if the result is determined by the left operand

### 8.4 Undefined Behavior

The following behaviors are undefined:

-   Reading uninitialized variables
-   Dereferencing invalid pointers
-   Accessing arrays out of bounds
-   Reading an inactive union field
-   Division by zero
-   Integer overflow (behavior is implementation-defined)

### 8.5 Scope and Lifetime

**Scope Rules**:

-   Variables are scoped to the block in which they are declared
-   Function parameters are scoped to the function body
-   Global declarations have file scope

### 8.6 Name Resolution

-   Names are resolved in the innermost scope first
-   Outer scopes are searched if a name is not found in the current scope
-   Global names can be accessed from any scope unless shadowed

### 8.7 Linkage

-   Functions can be declared as external to link with C libraries
-   The linker combines compiled object files into executables
-   Linking behavior follows the platform's standard

### 8.8 Program Entry Point

By convention, programs begin execution at a function named `main`:

```zirco
fn main() {
    // program starts here
}
```

The signature of `main` follows C conventions and is compatible with what LLVM allows (typically `fn main()` for void or `fn main() -> i32` for returning an exit code).

### 8.9 Future Directions

The following features are planned or under consideration:

-   Arrays with first-class syntax
-   Generics/parametric polymorphism
-   Pattern matching
-   Modules and namespaces
-   Enhanced type inference
-   Traits/interfaces
-   Enhanced safety features

---

## Appendix A: Grammar Summary

This section provides a high-level grammar summary. For complete details, refer to the full specification sections.

### A.1 Lexical Grammar

```
keyword ::= "as" | "break" | "continue" | "default" | "do" | "else"
          | "false" | "fn" | "for" | "if" | "let" | "return"
          | "sizeof" | "struct" | "switch" | "true" | "type"
          | "union" | "while" | "four"

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
       | "four" stmt
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

See [section 4.2](#42-operator-precedence) for the complete operator precedence table.

---

## Appendix C: Keywords Reference

See [section 2.4](#24-keywords) for the complete list of reserved keywords.

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

-   **AST**: Abstract Syntax Tree - the tree representation of source code structure
-   **TAST**: Typed Abstract Syntax Tree - AST with type information
-   **LLVM**: Low Level Virtual Machine - the compiler backend used by Zirco
-   **lvalue**: An expression that refers to a memory location
-   **rvalue**: An expression that produces a value
-   **Short-circuit evaluation**: Evaluation strategy where the second operand is not evaluated if the result is determined by the first

---

## Appendix F: Document History

-   **Version 0.1.0 (Draft)**: Initial language specification

---

_This specification is subject to change as Zirco evolves. Last updated: 2025_
