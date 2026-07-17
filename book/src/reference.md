# Language Reference

## 1. Introduction

Zirco is a compiled programming language with a strong type system and modern syntax, with a focus
on clarity. The language is designed to provide a balance between expressiveness and compatibility
with systems programming tasks.

### 1.1. Stability

Zirco is a highly experimental language that is under active development. The language and its
semantics are not stable, and they may change without warning. This includes the language's
syntax, semantics, compiler API, and standard library. **Zirco is not intended for production use.**

This book is not a stable reference for the language. It is a best-effort attempt to document the
language as it evolves, but it may be out of date. The content of this book may change without
warning (along with the language itself), and it may not be a reliable reference for the language.

## 2. Preprocessor

The preprocessor is an initial pass over a Zirco source program that performs simple text substitution.

The following directives are supported:

### 2.1. `#include`

`#include` copy-pastes the contents of another file into the current source file.

There are two forms of `#include`:

#### 2.1.1. `#include <file>`

Form: `#include <[^>\n]+>`

This form of `#include` is used to include files from the provided **include paths**. The include
paths are specified to the compiler via command line flags or via the `$ZIRCO_INCLUDE_PATH` environment variable.

#### 2.1.2. `#include "file"`

Form: `#include "[^"\n]+"`

This form of `#include` is used to include files relative to the current source file. The path is
resolved relative to the directory of the current source file.

### 2.2. Historical: `#pragma once`

This directive is deprecated and should be ignored.

### 2.3. Behavior

Implementations MUST NOT ever include the same file more than once in a single compilation unit. If a file is included multiple times, the compiler MUST ignore all but the first inclusion.

## 3. Lexical Structure

The lexer follows the **maximal munch rule**: `a+++b` is lexed as `a ++ + b`.

### 3.1. Character Set

Zirco source files are UTF-8 encoded text files.

### 3.2. Whitespace

Whitespace is ignored by the lexer, except when it is used to separate tokens.

The following characters are considered whitespace:

- Space (U+0020)
- Horizontal tab (U+0009)
- Line feed (U+000A)
- Carriage return (U+000D)
- Form feed (U+000C)

### 3.3. Comments

#### 3.3.1. Line Comments

At any point on a line, the sequence `//` indicates that the rest of the line is a comment and
should be ignored by the lexer up until the following linefeed.

#### 3.3.2. Block Comments

The sequence `/*` indicates the start of a block comment, terminated by `*/`.

Block comments may be nested, and may span multiple lines. The contents are ignored by the lexer.

```
/* /* This is a block comment. */ */
```

### 3.4. Keywords

The following identifiers are **keywords** or **reserved words** and may not be used in the IDENTIFIER
position:

```
true     false    if       else     while
do       for      four     break    continue
return   let      const    fn       as
struct   union    enum     match    sizeof
type     switch   default  new      unreachable
```

### 3.5. Terminals

#### 3.5.1. `IDENTIFIER`

The `IDENTIFIER` terminal is used to represent user-provided identifiers.

Form: `[a-zA-Z_][a-zA-Z0-9_]*`

Implementations MAY reserve the following identifiers for their own use:

- Identifiers beginning with `__` (two underscores)
- Identifiers beginning with `_` (one underscore) followed by an uppercase letter (e.g. `_Z`)

#### 3.5.2. `NUMBER_LITERAL`

The `NUMBER_LITERAL` terminal is used to represent numeric literals.

There are three acceptable forms:

- Decimal: `[0-9][0-9\._]*` (multiple `.` is forbidden)
- Hexadecimal: `0x[0-9a-fA-F_]+`
- Binary: `0b[01_]+`

In any of these forms, `_` is allowed as a visual separator.

`NUMBER_LITERAL` may be followed by an optional `IDENTIFIER` "type suffix", such as `4u8` or `4 u8`. The lexer MUST handle this case properly even without whitespace.

#### 3.5.3. `BOOLEAN_LITERAL`

The `BOOLEAN_LITERAL` terminal has two possible keyword values: `true` and `false`.

#### 3.5.4. `STRING_LITERAL`

The `STRING_LITERAL` terminal is used to represent string literals.

Form: `"([^"\\]|\\.)*"`

The following escape sequences MUST be permitted within string literals:

- `\0`: Null character (U+0000)
- `\n`: Line feed (U+000A)
- `\r`: Carriage return (U+000D)
- `\t`: Horizontal tab (U+0009)
- `\\`: Backslash (U+005C)
- `\"`: Double quote (U+0022)
- `\xHH`: Hexadecimal byte value (e.g. `\xFF`)
- `\u{H+}`: Unicode code point (e.g. `\u{1F600}`)

Other escape characters are implementation defined.

#### 3.5.5. `CHAR_LITERAL`

The `CHAR_LITERAL` terminal is used to represent character literals.

Form: `'([^'\\]|\\.)'`

They support the same escape sequences as `STRING_LITERAL`, alongside:

- `\'`: Single quote (U+0027)

### 3.6. Punctuation

Zirco uses the following operators and punctuation:

#### 3.6.1. Arithmetic Operators

```
+   -   *   /   %   ++   --
```

#### 3.6.2. Comparison Operators

```
==  !=  <   >   <=  >=
```

#### 3.6.3. Logical Operators

```
&&  ||  !
```

#### 3.6.4. Bitwise Operators

```
&   |   ^   ~   <<  >>
```

#### 3.6.5. Assignment Operators

```
=   +=  -=  *=  /=  %=  &=
|=  ^=  <<=
```

#### 3.6.6. Other Punctuation & Delimiters

```
.   ->  <-  ::  ?   :   ,
:   ;   =>  ... (   )   [
]   {   }
```

## 4. Type System

Zirco has a static type system.

All types have the following form. The `identifier` variant includes the primitive types, which are
included in the global scope by default.

```
type ::= identifier
       | "*" type
       | "[" NUMBER_LITERAL "]" type
       | "struct" "{" field_list? "}"
       | "union" "{" field_list? "}"
       | "enum" "{" field_list? "}"
       | "fn" "(" field_list? ")" "->" type
       | "(" type ")"

field_list ::= field_decl ("," field_decl)*
field_decl ::= identifier ":" type
```

### 4.1. Primitive Types

Zirco has the following primitive types, available in any scope. Like any other scoped identifier,
they may be shadowed by user-defined types, but this is highly discouraged for obvious reasons.

| Ident   | Width (bytes)      | Signed | Description                                            |
| ------- | ------------------ | ------ | ------------------------------------------------------ |
| `u8`    | 1                  | No     | Unsigned 8-bit integer                                 |
| `u16`   | 2                  | No     | Unsigned 16-bit integer                                |
| `u32`   | 4                  | No     | Unsigned 32-bit integer                                |
| `u64`   | 8                  | No     | Unsigned 64-bit integer                                |
| `usize` | Platform-dependent | No     | Unsigned integer type with the same width as a pointer |
| `i8`    | 1                  | Yes    | Signed 8-bit integer                                   |
| `i16`   | 2                  | Yes    | Signed 16-bit integer                                  |
| `i32`   | 4                  | Yes    | Signed 32-bit integer                                  |
| `i64`   | 8                  | Yes    | Signed 64-bit integer                                  |
| `isize` | Platform-dependent | Yes    | Signed integer type with the same width as a pointer   |
| `bool`  | 1                  | No     | Boolean type                                           |

The following type aliases (see below) are also always available in the global scope:

```
struct void {}
enum never {}
```

### 4.2. Pointer Types

The `*` prefix type operator converts a type into a pointer type.

```
*i32      // pointer to i32
**i32     // pointer to pointer to i32
*struct{} // pointer to an anonymous struct
```

A function pointer is represented as a pointer to a function type:

```
*fn(a: i32, b: i32) -> i32
```

### 4.3. Array Types

Arrays are fixed-size homogenous sequences of elements. The array type is represented as `[N]T`,
where `N` is a `NUMBER_LITERAL` and `T` is a type.

```
[5]i32      // array of 5 i32s
[10]bool    // array of 10 booleans
[3][4]u8    // array of 3 arrays of 4 u8s
```

### 4.4. Struct Types

Structs are user-defined product types that group named fields together.

Structs may be defined inline:

```
*struct { x: i32 }
```

Or with a named struct:

```
struct Point {
    x: i32,
    y: i32,
}
```

See Declarations for more information on struct declarations.

### 4.5. Union Types

Unions are user-defined sum types that group named fields together, but only one field may be active
at a time. Accessing a field that is not active is undefined behavior.

Unions may be defined inline:

```
*union { left: i8, right: u8 }
```

Or with a named union:

```
union Either {
    left: i8,
    right: u8,
}
```

See Declarations for more information on union declarations.

### 4.6. Enum Types

Enums are user-defined sum types that group named fields together. They are similar to unions,
but they have a discriminant that indicates which field is active (tagged union, akin to Rust).

Enums may be defined inline:

```
*enum { A: i32, B: u32 }
```

Or with a named enum:

```
enum Option {
    Some: i32,
    None: void,
}
```

### 4.7. Type Aliases

Type aliases are user-defined types that are equivalent to another type. They are defined with the
`type` keyword.

```
type MyInt = i32;
type Complex = struct { ... };
```

### 4.8. Function Types

Function types are user-defined types that represent functions. They are defined with the `fn`
keyword. They are not valid values except when used in a pointer type.

### 4.9. Type Inference

Zirco has very limited type inference. All types of an expression must be inferred immediately
given the information of sub-expressions and program context - no higher-level analysis is performed.

The type inference mechanism MUST be capable of at least the following:

- Handling arbitrarily-sized integers (i.e. `4 + 4` is `(int)`, not a concrete `i32`)
- Inferring the type of a variable from its initializer (e.g. `let x = 4;` infers `x` to be of type `i32`)
- Performing implicit coercions as described below

### 4.10. Implicit Coercions

The following type coercions are permitted implicitly:

#### 4.10.1. `*T` -> `*void`

Any pointer `*T` can be implicitly coerced to a `*void` pointer, useful for generic programming.

```
fn f(x: *void);

let x: i32;
f(&x); // &x is of type *i32, but can be coerced to *void
```

#### 4.10.3. `[]T` -> `*T`

An array `[]T` can be implicitly coerced to a pointer `*T`, useful for passing arrays to functions.

```
fn f(x: *i32);

let x: [5]i32;
f(x); // x is of type [5]i32, but can be coerced to *i32
```

#### 4.10.4. Arbitrary Integer Literals

Any integer literal has the inferred type `(int)` until used in a position which forces it to have
a given width and signedness, otherwise inferred to be `i32`. This allows expressions like:

`4i32 + 2` to be valid, where `2` is inferred to be of type `i32` because it is used in an
expression with an `i32`.

### 4.11. Type Behavior

Zirco uses "duck typing," if a type has the same structure as another type, it is considered to be
the same type.

### 4.12. Field Ordering/Padding

Field ordering is in declaration order, and padding is implementation defined. Compilers MUST NOT
reorder fields.

## 5. Expressions

Expressions are anything that produces a value. They MAY have side effects.

This section defines their syntax, semantics, and type rules.

```
field_init ::= identifier ":" expr
type_in_constr := IDENTIFIER | struct ... | union ... | enum ...

expr ::= IDENTIFIER
       | NUMBER_LITERAL IDENTIFIER? # Number with optional type suffix
       | CHAR_LITERAL
       | STRING_LITERAL
       | "true" | "false"
       | "sizeof" type
       | "sizeof" "(" expr ")"
       | "[" (expr ("," expr)*)? "]"
       | type_in_constr "{" (field_init ("," field_init)*)? "}"
       | "(" expr ")"
       | expr "[" expr "]"
       | expr "." IDENTIFIER
       | expr "->" IDENTIFIER
       | expr "(" (expr ("," expr)*)? ")"
       | expr "++"
       | expr "--"
       | ("!" | "-" | "~" | "&" | "*" | "++" | "--") expr
       | IDENTIFIER "<-" expr
       | expr "as" type
       | expr ("/" | "*" | "%") expr
       | expr ("+" | "-") expr
       | expr (">" | ">=" | "<" | "<=") expr
       | expr ("==" | "!=") expr
       | expr ("&" | "|" | "^") expr
       | expr ("&&" | "||") expr
       | expr "?" expr ":" expr
       | expr ("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=") expr
       | expr "," expr
```

### 5.1. Operator Precedence

Operators are listed from highest to lowest precedence:

| Precedence | Operators                                    | Description                                                            | Associativity |
| ---------- | -------------------------------------------- | ---------------------------------------------------------------------- | ------------- |
| 1          | `x()` `x[]` `x.y` `x->y` `x++` `x--`         | Function call, array index, member access, postfix increment/decrement | Left-to-right |
| 2          | `!x` `-x` `~x` `&x` `*x` `++x` `--x` `y<-x`  | Unary operators, prefix increment/decrement, prefix member access      | Right-to-left |
| 3          | `as`                                         | Type cast                                                              | Left-to-right |
| 4          | `*` `/` `%`                                  | Multiplication, division, modulo                                       | Left-to-right |
| 5          | `+` `-`                                      | Addition, subtraction                                                  | Left-to-right |
| 6          | `<` `<=` `>` `>=`                            | Comparison                                                             | Left-to-right |
| 7          | `==` `!=`                                    | Equality                                                               | Left-to-right |
| 8          | `&`                                          | Bitwise AND                                                            | Left-to-right |
| 9          | `^`                                          | Bitwise XOR                                                            | Left-to-right |
| 10         | `\|`                                         | Bitwise OR                                                             | Left-to-right |
| 11         | `&&`                                         | Logical AND                                                            | Left-to-right |
| 12         | `\|\|`                                       | Logical OR                                                             | Left-to-right |
| 13         | `? :`                                        | Ternary conditional                                                    | Right-to-left |
| 14         | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` | Assignment                                                             | Right-to-left |
| 15         | `,`                                          | Comma                                                                  | Left-to-right |

### 5.2. Primary Expressions

#### 5.2.1. Literals

##### 5.2.1.1. `IDENTIFIER`

`IDENTIFIER`s assume the type of any variable in the value scope (see Statements) with the same name.

If no such name is found, an error MUST be produced.

##### 5.2.1.2. `NUMBER_LITERAL`

`NUMBER_LITERAL`s have the type of the literal's suffix, if present. If no suffix is present, the
type is inferred to be an arbitrary integer until used in a context where the type is forced to be
any concrete type, otherwise it is inferred to be `i32`.

##### 5.2.1.3. `BOOLEAN_LITERAL`

The keywords `true` and `false` are of type `bool`.

##### 5.2.1.4. `CHAR_LITERAL`

`CHAR_LITERAL`s are of type `u8`, and represent a single byte.

##### 5.2.1.5. `STRING_LITERAL`

`STRING_LITERAL`s are of type `*u8` and contain often-immutable data.

#### 5.2.2. `sizeof`

The `sizeof` operator returns the size of a type or expression as a `usize` in bytes.

Form: `sizeof type` or `sizeof (expr)`

The size of a type is implementation defined.

#### 5.2.3. Array Construction

The `[a, b, c, d]` syntax constructs an array of the given elements.

All elements must have the same type, and the array's type is inferred to be `[N]T`, where `N` is
the number of elements.

An empty array `[]` is not valid, as the type cannot be inferred.

#### 5.2.4. Struct Construction

Any identifier (named struct), inline struct/union/enum can be constructed.

```
type Point = struct { x: i32, y: i32 };
let p = Point { x: 1, y: 2 }; // p is of type Point
let q = struct { x: i32, y: i32 } { x: 3, y: 4 }; // q is of type struct { x: i32, y: i32 }
```

#### 5.2.5. Parenthesized Expressions

Parentheses can be used to group expressions and override operator precedence.

### 5.3. Postfix Expressions

#### 5.3.1. Array/Pointer Indexing

A pointer `x : *T` where `i: [ui]size` can be indexed with `x[i]`. This is equivalent to `*(x + i)`,
indexing `i * sizeof(x)` bytes into the pointer.

Arrays `[N]T` automatically coerce to `*T`, so they can also be indexed with `x[i]`.

#### 5.3.2. Member Access

A struct/union/enum `x` can have its members accessed with `x.y`, where `y` is the name of a field
in the struct/union/enum. The type of `x.y` is the type of that field.

#### 5.3.3. Pointer Member Access

A pointer `x : *T` where `T` is a struct/union/enum can have its members accessed with `x->y`,
where `y` is the name of a field in the struct/union/enum. The type of `x->y` is the type of that
field.

This can also be done in a "reversed" format, `y<-x`, which is equivalent to `x->y`.
This is a joke feature, but is a real part of Zirco.

#### 5.3.4. Function Calls

A function or function pointer `f` can be called with `f(a, b, c)`, where `a`, `b`, and `c` are the
arguments to the function. The number and types of the arguments must match. If the function has
a variadic `...` trailing in its type, an infinite number of arbitrary arguments may be passed.

#### 5.3.5. Postfix Increment/Decrement

The postfix increment `x++` and decrement `x--` operators increment or decrement the value of
`x` by 1, and return the original value of `x`. The type of `x` must be an integer type.

### 5.4. Unary Expressions

#### 5.4.1. Unary NOT

The unary NOT operator `!x` returns the logical negation of `x`. The type of `x` must be `bool`,
and the result is also of type `bool`.

#### 5.4.2. Unary Negation

The unary negation operator `-x` returns the arithmetic negation of `x`. The type of `x` must be
an integer type, and the result is also of the same integer type.

#### 5.4.3. Bitwise NOT

The bitwise NOT operator `~x` returns the bitwise negation of `x`. The type of `x` must be an
integer type, and the result is also of the same integer type.

#### 5.4.4. Address-of

The address-of operator `&x` returns a pointer to `x`. The type of `x` must be a variable, and the
result is of type `*T`, where `T` is the type of `x`.

#### 5.4.5. Dereference

The dereference operator `*x` returns the value pointed to by `x`. The type of `x` must be a
pointer type `*T`, and the result is of type `T`.

#### 5.4.6. Prefix Increment/Decrement

The prefix increment `++x` and decrement `--x` operators increment or decrement the value of
`x` by 1, and return the new value of `x`. The type of `x` must be an integer type.

### 5.5. Cast Expression (`as`)

The cast expression `a as b` (`a: A`, `b: B`) is valid on the following pairs of types, and has
the following semantics:

| `A`                     | `B`                     | Behavior                          |
| ----------------------- | ----------------------- | --------------------------------- |
| some type T             | T                       | no-op                             |
| signed integer size n   | signed integer size m   | sign-extend or truncate to size m |
| unsigned integer size n | unsigned integer size m | zero-extend or truncate to size m |
| signed integer size n   | unsigned integer size m | sign-extend or truncate to size m |
| unsigned integer size n | signed integer size m   | zero-extend or truncate to size m |
| pointer type `*T`       | pointer type `*U`       | pointer cast                      |
| pointer type `*T`       | integer type `usize`    | pointer to integer cast           |
| integer type `usize`    | pointer type `*T`       | integer to pointer cast           |

### 5.6. Arithmetic Expressions

Arithmetic expressions are always on two integer or pointer operands, and yield the same type as the
operands.

- `x + y`: addition
- `x - y`: subtraction
- `x * y`: multiplication
- `x / y`: division
- `x % y`: modulo

### 5.6. Comparison Expressions

Comparison expressions are always on two integer operands, and yield a `bool` result.

- `x == y`: equality
- `x != y`: inequality
- `x < y`: less than
- `x <= y`: less than or equal to
- `x > y`: greater than
- `x >= y`: greater than or equal to

### 5.7. Equality Expressions

Equality expressions are always on two integer operands, and yield a `bool` result.

- `x == y`: equality
- `x != y`: inequality

### 5.8. Bitwise Expressions

Bitwise expressions are always on two integer operands, and yield the same type as the operands.

- `x & y`: bitwise AND
- `x | y`: bitwise OR
- `x ^ y`: bitwise XOR

### 5.9. Logical Expressions

Logical expressions are always on two `bool` operands, and yield a `bool` result. They are short
circuiting (side effects are not evaluated if the result can be determined from the first operand).

- `x && y`: logical AND
- `x || y`: logical OR

### 5.10. Ternary Conditional Expressions

The ternary conditional expression `x ? y : z` evaluates `x`, and if it is true, evaluates and
returns `y`, otherwise evaluates and returns `z`. The type of the expression is the common type
of `y` and `z`, which must be the same type.

### 5.11. Compound Assignment Expressions

Compound assignment expressions are always on two integer operands, and yield the same type as the
operands. They are equivalent to the corresponding binary operator, but with the left operand being
modified in place.

### 5.12. Comma Expressions

The comma expression `x, y` evaluates `x`, discards the result, and then evaluates and returns
`y`. The type of the expression is the type of `y`.

## 6. Statements

Statements are _program flow control constructs_, not things that produce values.

```
stmt ::= ";"
       | expr ";"
       | "{" stmt* "}"
       | "if" "(" expr ")" stmt ("else" stmt)?
       | "while" "(" expr ")" stmt
       | "do" stmt "while" "(" expr ")" ";"
       | "for" "(" expr? ";" expr? ";" expr? ")" stmt
       | "four" stmt
       | "switch" "(" expr ")" "{" switch_case* "}"
       | "match" "(" expr ")" "{" match_case* "}"
       | ("let" | "const") let_decl_list ";"
       | "continue" ";"
       | "break" ";"
       | "return" expr? ";"
       | "unreachable" ";"

switch_case ::= expr "=>" stmt
match_case  ::= IDENTIFIER ":" IDENTIFIER "=>" stmt

let_decl_list ::= let_decl ("," let_decl)*
let_decl      ::= IDENTIFIER (":" type)? ("=" expr)?
```

### 6.1. Empty Statement

The statement `;` is a no-op.

### 6.2. Expression Statements

The expression statement `expr;` evaluates the expression `expr`, discards the result, and
proceeds to the next statement.

### 6.3. Block Statements

Block statements are a sequence of statements enclosed in braces `{}`. They create a new scope for
variables declared within the block.

### 6.4. If Statements

If statements have the form `if (condition) then_branch else else_branch`, where `condition`
is an expression of type `bool`, and `then_branch` and `else_branch` are statements. The `else`
branch is optional.

### 6.5. While Statements

While statements have the form `while (condition) body`, where `condition` is an expression of
type `bool`, and `body` is a statement. The loop continues as long as the condition evaluates to
`true`.

### 6.6. Do-While Statements

Do-while statements have the form `do body while (condition);`, where `body` is a statement, and
`condition` is an expression of type `bool`. The loop executes the body at least once, and then
continues as long as the condition evaluates to `true`.

### 6.7. For Statements

For statements have the form `for (init; condition; increment) body`, where `init` is an optional
expression statement, `condition` is an optional expression of type `bool`, `increment` is an
optional expression statement, and `body` is a statement. The loop executes the `init` statement
once, then continues to execute the `body` as long as the `condition` evaluates to `true`, executing
the `increment` statement after each iteration.

### 6.8. Four Statements

Four statements have the form `four body`, where `body` is a statement. The loop executes the
`body` statement four times.

### 6.9. Switch Statements

Switch statements have the form `switch (expr) { case1 => stmt1; case2 => stmt2; ... }`, where
`expr` is an expression, and each `case` is a pair of a constant integer expression and a statement.

`switch` is not valid for dynamic `case` values, as a jump table is commonly used.

### 6.10. Match Statements

Match statements have the form `match (expr) { case1: x => stmt1; case2: x => stmt2; ... }`,
where `expr` is an expression, and each `case` is a pair of an enum variant name and a captured
variable from the tagged union.

### 6.11. Variable Declarations

The `let` and `const` statements declare variables. The difference between the two is that `let`
declares a mutable variable, while `const` declares an immutable variable.

Either a type or an initializer must be provided, but not neither.

Variables must be initialized before they are used, and the type of the initializer must match the
declared type.

Multiple variables can be declared in a single statement, separated by commas.

```
let x: i32 = 5;
const y: i32 = 10;
let a: i32 = 1, b: i32 = 2, c = 4;
```

### 6.12. Continue Statements

The `continue;` statement causes the current iteration of the nearest enclosing loop to end, and
control to jump to the next iteration of that loop.

### 6.13. Break Statements

The `break;` statement causes the nearest enclosing loop to terminate, and control to jump to the
statement immediately following that loop.

### 6.14. Return Statements

The `return expr;` statement causes the current function to terminate, and control to jump to the
statement immediately following the function call. The value of `expr` is returned to the caller.

### 6.15. Unreachable Statements

Reaching a `unreachable;` statement is undefined behavior. It is used to indicate that a certain
code path should never be reached, and can be used to optimize code generation.

## 7. Declarations

```
program ::= decl*

decl ::= "struct" IDENTIFIER "{" field_decl_list? "}"
       | "union" IDENTIFIER "{" field_decl_list? "}"
       | "enum" IDENTIFIER "{" field_decl_list? "}"
       | "type" IDENTIFIER "=" type ";"
       | "fn" IDENTIFIER "(" param_list? ")" ("->" type)? block
```

Declarations introduce new names into the program at the global scope.

### 7.1. Struct, Union, and Enum Declarations

See Type System for more information on struct, union, and enum types.

### 7.2. Type Alias Declarations

See Type System for more information on type aliases.

### 7.3. Function Declarations

Functions are declared as follows with the `fn` keyword. The return type, arguments, etc may be
optionally specified. If the return type is not specified, it is inferred to be `void`.

```
fn foo(a: i32, b: i32) -> i32 {
    return a + b;
}
```

If the body is elided, it is assumed to be an extern declaration, using the C calling convention.
If the body is elided, you may also include `...` to indicate that the function is variadic.

## 8. Program Structure

### 8.1. Entry Point

All Zirco programs follow the `crt0.S` entry point of either of the following signatures:

```
fn main() -> i32;
fn main(argc: i32, argv: **u8) -> i32;
```

### 8.2. Compilation Units

The Zirco compiler compiles a single source file at a time, and each source file is a compilation
unit. Each compilation unit has its own global scope, and the contents of one compilation unit are
not visible to another compilation unit unless they are explicitly imported via `#include`.

## 9. Standard Library

Zirco's standard library, `libzr`, is currently under development. The correct API surface is as is
defined by the `libzr` project in the zrc monorepo.
