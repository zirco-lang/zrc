# Programming a Guessing Game

Let's dive in deep by programming a simple guessing game together! This chapter introduces you to
the following Zirco concepts:

- Tools from the libc standard library for:
    - Random number generation
    - String formatting
    - Input handling
- Loops
- Variables

These topics will all be covered far more thoroughly in later chapters, but this classic game is
a perfect opportunity to build something tangible for the first time.

As with all projects, start by creating a project directory.

```
$ mkdir guessing-game
$ cd guessing-game
```

For this project, we will use `zrx` to execute our program, but you may also choose to use a build
system like `make` or `just` to compile and run your program.

Create a file named `main.zr` in your project directory with the "Hello, World!" program from the
previous chapter.

Filename: main.zr

```
#include <libc/stdio.zh>
fn main() -> i32 {
    printf("Hello, World!\n");
    return 0;
}
```

Execute the program to double check that everything is working:

```
$ zrx main.zr
Hello, World!
```

## Handling User Input

We've learned how to print text to the terminal, but for our guessing game it's just as important to
be able to read user input as well. The `scanf` function from the C standard library is perfect
for this, as it avoids many pitfalls common with raw input handling (such as buffer overflows) by
allowing us to specify an exact "format" for the input we expect (hence, "scan format", or `scanf`).

Enter the code below into your `main.zr` file to read a number from the user and print it back to
the terminal.

```
#include <libc/stdio.zh>
fn main() -> i32 {
    printf("Enter a number: ");
    let guess: i32;
    scanf("%d", &guess);
    printf("You entered: %d\n", guess);
    return 0;
}
```

This code prompts the user to enter a number, reads the input using `scanf`, and then prints it back
to the terminal. The `scanf` function takes a format string (in this case, `%d` for an integer) and
a pointer to the variable where the input should be stored (`&guess`).

Try to execute this code, and enter a number when prompted. You should see the number you entered
printed back to you!

## Error Handling

Try entering something that isn't a number, such as "hello". What happens?

The program will likely crash or behave unexpectedly, because `scanf` expects an integer input based
on our format string. To handle this gracefully, we can check the return value of `scanf`, which
indicates how many items were successfully read. If it returns anything other than `1`, we know the
input was invalid.

```
#include <libc/stdio.zh>
fn main() -> i32 {
    printf("Enter a number: ");
    let guess: i32;
    if (scanf("%d", &guess) != 1) { // <<<< notice that `scanf` is now inside an `if` statement
        printf("Invalid input! Please enter a valid number.\n");
        return 1; // Exit with an error code
    }
    printf("You entered: %d\n", guess);
    return 0;
}
```

This is a common paradigm in both C and Zirco for error handling where functions return a status
code to indicate success or failure. Checking the return value of many functions is **not optional**,
see below.

## Undefined Behavior?

Before we introduced validation, the program had "undefined behavior" when the user entered invalid
input because the value of `guess` was never initialized by `scanf` if anything non-numeric was
entered. This is a common source of **undefined behavior**, which is prevalent in many low level
languages.

The language assumes undefined behavior will never happen. This allows the compiler to make
aggressive optimizations, but it also means that if you do encounter undefined behavior, all bets
are off. The program could crash, it could print random garbage, or far, far worse.

> "Permissible undefined behavior ranges from ignoring the situation completely with unpredictable
> results, to having demons fly out of your nose."
> ~ [from comp.std.c](https://groups.google.com/g/comp.std.c/c/ycpVKxTZkgw/m/S2hHdTbv4d8J?hl=en&pli=1)

Some examples of undefined behavior include, but are not limited to:

- Accessing an array out of bounds
- Dereferencing a null pointer
- Using an uninitialized variable
- Integer overflow (in some cases)
- Violating type safety (e.g., treating a pointer to one type as a pointer to another type)
- Modifying a string literal
- Using a pointer after it has been freed

If you do not take care to avoid "UB", you will encounter it sooner or later. One form of mitigation
often necessary is checking the return values of functions (which may return null pointers, error
codes, or other indicators of failure) before using their results.

## Generating a Random Number

To make our guessing game actually fun, we need to generate a random number for the user to guess.
The C standard library provides the `random` function for this purpose, which generates a
pseudo-random number. To use it, we need to include the appropriate header, set the random seed,
and then call `random` to get a random number.

```
#include <libc/stdio.zh>
#include <libc/time.zh>
#include <libc/stdlib.zh>
fn main() -> i32 {
    // Set the random seed based on the current time
    srandom(time(0 as *TimeHandle) as u32);

    // Generate a random number between 1 and 100
    let secret_number = (random() % 100) + 1;

    printf("I have selected a number between 1 and 100. Can you guess it?\n");
    // ... rest of the game logic goes here ...
    return 0;
}
```

This code calls `time(null)` to get the current time as a 64-bit unsigned integer (`u64`). We then
cast it to a 32-bit unsigned integer (`u32`) to use as the seed for `srandom`. The `random` function
generates a random number, which we then take the modulus of 100 to get a number between 0 and 99,
and add 1 to shift it to the desired range of 1 to 100.

## Comparing the Guess

Now that we have a random number and user input, we can compare the user's guess to the secret
number and give feedback.

```
// ... after generating secret_number and reading guess from user
if (guess < secret_number) {
    printf("Too low! Try again.\n");
} else if (guess > secret_number) {
    printf("Too high! Try again.\n");
} else {
    printf("Congratulations! You guessed the number!\n");
}
```

This code compares the user's guess to the secret number and prints feedback accordingly.

## Looping Until the User Guesses Correctly

Finally, we want to allow the user to keep guessing until they get it right. We can achieve this
with a simple infinite (`while (true)`) loop that breaks when the user guesses correctly.

```
#include <libc/stdio.zh>
#include <libc/time.zh>
#include <libc/stdlib.zh>
fn main() -> i32 {
    srandom(time(0 as *TimeHandle) as u32);
    let secret_number = (random() % 100) + 1;
    printf("I have selected a number between 1 and 100. Can you guess it?\n");
    while (true) {
        printf("Enter your guess: ");
        let guess: i32;
        if (scanf("%d", &guess) != 1) {
            printf("Invalid input! Please enter a valid number.\n");
            continue; // Skip the rest of the loop and prompt again
        }
        if (guess < secret_number) {
            printf("Too low! Try again.\n");
        } else if (guess > secret_number) {
            printf("Too high! Try again.\n");
        } else {
            printf("Congratulations! You guessed the number!\n");
            break; // Exit the loop when the guess is correct
        }
    }
    return 0;
}
```

This code will keep prompting the user to enter a guess until they guess the correct number,
providing feedback on whether their guess is too high, too low, or correct. We use the `continue`
statement to skip the rest of the loop and prompt the user again if they enter invalid input, and
the `break` statement to exit the loop when the user guesses correctly.

Congratulations! You've just built a complete guessing game in Zirco!
