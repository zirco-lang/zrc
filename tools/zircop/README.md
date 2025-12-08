<div align="center">

![Zirco banner](https://github.com/zirco-lang/assets/blob/main/png/wide-light.png)

# zircop: a linter for Zirco

</div>

Zircop (Zirco Cop) is a linting tool for the Zirco programming language. It analyzes Zirco source
code to identify potential issues, enforce coding standards, and suggest improvements to enhance
code quality and maintainability.

## Usage

Run zircop on a Zirco source file:

```bash
zircop path/to/file.zr
```

## Ignoring Lints

Zircop supports two methods for ignoring specific lints:

### CLI Flags

Use the `--allow` or `-A` flag to ignore specific lints from the command line:

```bash
# Ignore a specific lint
zircop --allow unused_variable path/to/file.zr

# Ignore multiple lints
zircop --allow unused_variable --allow underscore_variable_used path/to/file.zr

# Ignore all lints
zircop --allow all path/to/file.zr
```

### In-Code Comments

Use special comments in your Zirco source code to ignore lints:

```zirco
fn f() -> i32 {
    // Ignore lint on the current line
    let unused = 10; // zircop-ignore: unused_variable

    // Ignore lint on the next line
    // zircop-ignore-next-line: unused_variable
    let also_unused = 20;

    // Ignore all lints on the current line
    let _var = 7; // zircop-ignore: all

    return 0;
}
```

### Available Lint Names

- `unused_variable` - Detects unused variable declarations
- `underscore_variable_used` - Detects usage of variables that start with underscore
- `unreachable_code` - Detects code that can never be reached
- `empty_struct_used` - Detects usage of empty structs
- `assignment_in_condition` - Detects assignments in condition expressions
- `empty_if_block` - Detects empty if blocks with else blocks present
- `empty_else_block` - Detects empty else blocks
- `empty_while_body` - Detects empty while loop bodies
- `all` - Ignores all lints
