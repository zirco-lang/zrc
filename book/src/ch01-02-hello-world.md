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
