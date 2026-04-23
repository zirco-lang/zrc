# Hello, World!

Now that you've installed the Zirco toolchain, let's write our first Zirco program! As is tradition,
we'll write a simple program that simply prints "Hello, World!" to the screen.

## Project Setup

Zirco's compilation unit is the file, similar to C. There is no first-class concept of a "project".
This comes with the benefit of placing your source files wherever you want!

To create a Zirco source file, simply create a new file with the `.zr` extension. It is typical to
call the main source file `main.zr`.

Filename: `main.zr`

```
#include <libc/stdio.zh>
fn main() -> i32 {
    printf("Hello, World!\n");
    return 0;
}
```

Save the file and run the following commands to compile, link, and run the program:

```
$ zrc main.zr --emit object -o main.o
$ ld -lc -lzr main.o -o main
$ ./main
Hello, World!
```

| Command                               | Explanation                                                                                          |
| ------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `zrc main.zr --emit object -o main.o` | Compiles `main.zr` to an object file `main.o`.                                                       |
| `ld -lc -lzr main.o -o main`          | Links `main.o` with the C and Zirco standard libraries (`-lc -lzr`) to produce an executable `main`. |
| `./main`                              | Runs the `main` executable, which prints "Hello, World!" to the terminal.                            |

If you see "Hello, World!" printed to the terminal, congratulations! You've successfully written and
run your first Zirco program! If you don't see the expected output, take a look at the
[installation troubleshooting](./ch01-01-installation.md#troubleshooting) section.

## Anatomy of the Program

Let's break down the `main.zr` program to understand what each part does:

```
#include <libc/stdio.zh>
```

This line includes the Zirco header file for the C standard library (libc)'s Standard I/O functions.
This directive is used to "copy-paste" the contents of a file into another, similar to C's `#include`
directive. If you check your toolchain's `include/libc/stdio.zh` file, you'll see that it contains
"extern" declarations for a bunch of functions, including `printf` and `scanf` which we later use.

```
fn printf(a: *u8, ...) -> i32;
fn scanf(a: *u8, ...) -> i32;
```

These functions are not defined in Zirco, but they exist here so the compiler is aware of their
signatures and can generate the correct code to call them.

```
fn main() -> i32 {
```

This line defines a function named `main` that takes no parameters and returns an `i32` (32-bit
integer). The `main` function is the entry point of the program, where all Zirco programs start
execution. The function's return value represents its exit code, where `0` typically indicates
success.

```
    printf("Hello, World!\n");
```

This line calls the `printf` function to print the string "Hello, World!" followed by a newline
character (`\n`) to the terminal.

```
    return 0;
}
```

This line returns `0` from the `main` function, indicating that the program executed successfully.

## Using the `zrx` Tool

Oftentimes, for one-off programs (such as many of the examples in this book), it's more convenient
to compile and run the program in one step. The `zrx` tool does exactly this: it compiles, links,
and runs your program with a single command via a JIT.

To run your `main.zr` program with `zrx`, simply run the following command:

```
$ zrx -lzr main.zr
Hello, World!
```

Many larger Zirco projects choose to use a build system such as [make](<https://en.wikipedia.org/wiki/Make_(software)>).

## Congratulations!

In subsequent chapters, we'll cover the Zirco language in more depth, but for now, your toolchain
is working and you can write and run Zirco programs! In the next chapter, we'll cover the basics of
the Zirco language and syntax.
