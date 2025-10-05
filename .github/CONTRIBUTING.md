<div align="center">

# Contributing to Zirco

</div>

We greatly appreciate all contributions from the open source community. They're part of what keep Zirco possible.

If you have a suggestion for something that could improve the Zirco compiler or language, please [create an issue](https://github.com/zirco-lang/zrc/issues/new/choose). This can also be used for bug reports.

If you have a change you would like to make, please make a corresponding issue (unless it is trivial) and receive public feedback before beginning work on it. Then, fork our repository and make your changes — we appreciate them all. You may then create a Pull Request and the corresponding maintainer will provide feedback before possibly merging your PR!

## Table of Contents

- [General Guidelines](#general-guidelines)
- [Proposing New Language Features](#proposing-new-language-features)
- [Implementing Language Features](#implementing-language-features)
- [Code Style and Quality](#code-style-and-quality)
- [Testing Requirements](#testing-requirements)
- [Documentation Requirements](#documentation-requirements)

## General Guidelines

### Before You Start

1. **Search for existing issues** to avoid duplicate work
2. **Discuss major changes** in an issue before implementing
3. **Follow the Code of Conduct** at all times
4. **Keep changes focused** - one feature or fix per PR
5. **Write clear commit messages** using [Semantic Commit Messages](https://www.conventionalcommits.org/)
   - `feat:` for new features
   - `fix:` for bug fixes
   - `docs:` for documentation changes
   - `refactor:` for code refactoring
   - `test:` for test additions/changes
   - `chore:` for maintenance tasks

## Proposing New Language Features

New language features require careful consideration as they become part of Zirco's long-term design. Follow this process:

### 1. Create a Feature Proposal Issue

Use the [feature request template](https://github.com/zirco-lang/zrc/issues/new?template=feat.yml) and include:

- **Clear motivation**: Explain the problem your feature solves
- **Use cases**: Provide concrete examples of how it would be used
- **Syntax proposal**: Show what the feature would look like in Zirco code
- **Ecosystem benefits**: Describe how this helps the broader community
- **Potential drawbacks**: Consider downsides and compatibility concerns
- **Alternatives**: What other approaches did you consider?

### 2. Design Considerations

When proposing a feature, consider:

- **Parser compatibility**: Will it work with LALR(1) parsing? (See the `new` keyword example in struct construction)
- **Type system impact**: How does it interact with Zirco's strong type system?
- **LLVM codegen**: Can it be efficiently mapped to LLVM IR?
- **C-like semantics**: Does it align with Zirco's C-like semantic model?
- **Rust-inspired syntax**: Does it fit Zirco's modern syntax goals?
- **Future compatibility**: Does it leave room for planned features like generics, pattern matching, or modules?

### 3. Reference Existing Features

Study similar implemented features for guidance:

- **Struct construction** (`examples/struct_construction/`) - Shows the `new` keyword syntax
- **Pointers** (`examples/pointer_swap/`) - Low-level memory operations
- **Control flow** (`examples/loop_example/`) - Statement-level features

Refer to the [Language Specification](../docs/SPEC.md) for:
- Current syntax and semantics
- Type system rules
- Expression precedence
- Future directions (Section 8.9)

### 4. Community Discussion

- Engage with feedback on your proposal issue
- Be open to alternative designs
- Consider backwards compatibility (though note: Zirco is pre-1.0 with no stability guarantees)
- Wait for maintainer approval before starting implementation

## Implementing Language Features

Language features typically require changes across three compiler stages. Here's the recommended workflow:

### Stage 1: Parser (zrc_parser)

**Location**: `compiler/zrc_parser/`

1. **Update the grammar** (`src/parser.lalrpop`):
   - Add new tokens to the lexer if needed (`src/lexer.rs`)
   - Define grammar rules for your syntax
   - Ensure the grammar remains LALR(1) compatible

2. **Extend the AST** (`src/ast/`):
   - Add new AST node types for your feature
   - Implement `Display` trait for pretty-printing
   - Add `Debug` implementations

3. **Build and test**:
   ```bash
   cargo build -p zrc_parser
   cargo test -p zrc_parser
   ```

**Example**: The `new` keyword for struct construction required adding both a keyword token and grammar rules for struct initialization expressions.

### Stage 2: Type Checker (zrc_typeck)

**Location**: `compiler/zrc_typeck/`

1. **Extend the TAST** (`src/tast/`):
   - Create typed versions of your AST nodes
   - Include type information in the nodes

2. **Implement type checking** (`src/typeck/`):
   - Add type checking logic for your feature
   - Validate type correctness
   - Provide helpful error messages via `zrc_diagnostics`
   - Handle type inference if applicable

3. **Build and test**:
   ```bash
   cargo build -p zrc_typeck
   cargo test -p zrc_typeck
   ```

**Important**: Ensure your type checking catches all semantic errors early.

### Stage 3: Code Generator (zrc_codegen)

**Location**: `compiler/zrc_codegen/`

1. **Implement LLVM IR generation**:
   - Add codegen logic in appropriate files (`src/stmt.rs`, `src/expr.rs`, etc.)
   - Use `inkwell` bindings to generate LLVM IR
   - Handle all type variants

2. **Add snapshot tests**:
   - Create tests that verify generated IR
   - Use `insta` for snapshot testing
   - Review snapshots with `cargo insta review`

3. **Build and test**:
   ```bash
   cargo build -p zrc_codegen
   cargo test -p zrc_codegen
   ```

### Stage 4: End-to-End Integration

1. **Create a complete example** in `examples/`:
   - Create a new directory with a descriptive name
   - Include a `Makefile` following existing patterns
   - Add example `.zr` source files
   - Write a comprehensive `README.md` explaining:
     - What the feature does
     - Syntax demonstration
     - Implementation status
     - How to build and run

2. **Test the full pipeline**:
   ```bash
   cargo build
   cargo test
   make -C examples/<your-example> test
   ```

3. **Update the specification** (`docs/SPEC.md`):
   - Document the syntax
   - Explain semantics and behavior
   - Add to the appropriate section
   - Include grammar rules
   - Provide examples

## Code Style and Quality

### Required Checks

**All code must pass these checks before submission**:

```bash
# Build
cargo build

# Tests
cargo test

# Linting (requires nightly toolchain)
cargo +nightly clippy --all-targets -- -D warnings

# Formatting (requires nightly toolchain)
cargo +nightly fmt
```

### Clippy Standards

Zirco uses **extremely strict** clippy lints:
- All items (public and private) require documentation comments
- **No `.unwrap()`** - handle errors properly with `Result` or `?`
- **No `todo!()` or `unimplemented!()`** in committed code
- Follow all `clippy::pedantic` and `clippy::nursery` lints

### Documentation Standards

- Use `///` for item documentation
- Use `//!` for module documentation
- Document all parameters with `# Arguments`
- Document return values
- Document panics with `# Panics`
- Document safety with `# Safety` for unsafe code
- Use examples in doc comments where helpful

## Testing Requirements

### Unit Tests

- Add tests inline with `#[cfg(test)] mod tests { ... }`
- Test edge cases and error conditions
- Use descriptive test names

### Snapshot Tests (for codegen)

- Use `insta` crate for IR verification
- Review snapshots carefully with `cargo insta review`
- Commit snapshots with your changes

### Integration Tests

- Create full example programs in `examples/`
- Include `make test` targets
- Test compilation and execution

### Test Coverage

- Aim for comprehensive coverage of new code
- The project uses `cargo-tarpaulin` for coverage tracking
- CI will report coverage changes

## Documentation Requirements

### For All Changes

1. **Update relevant documentation** when changing behavior
2. **Add code comments** for complex logic (matching existing style)
3. **Update `README.md`** if user-facing behavior changes

### For Language Features

1. **Update `docs/SPEC.md`**:
   - Add to the appropriate section
   - Follow the existing structure
   - Include grammar, semantics, and examples
   - Update the Table of Contents

2. **Create an example** in `examples/`:
   - Full working code demonstrating the feature
   - Comprehensive README explaining the feature
   - Makefile for building and testing
   - Clear documentation of implementation status

3. **Update compiler help text** if adding CLI options

### Documentation Style

- Use clear, concise language
- Provide code examples
- Link to related features
- Explain the "why" not just the "what"

## Stability Notice

**Important**: Zirco is pre-1.0 with **NO STABILITY GUARANTEES**.

- Internal compiler APIs may change without warning
- Language syntax and semantics may evolve
- Breaking changes are expected
- All crates are version `0.1.0` until stability is maintained

This allows rapid iteration and improvement. Your contributions help shape Zirco's future!

## Need Help?

- **Questions?** Open a [GitHub Discussion](https://github.com/zirco-lang/zrc/discussions)
- **Stuck?** Comment on your PR or issue
- **Found a bug?** Open a [bug report](https://github.com/zirco-lang/zrc/issues/new?template=bug.yml)

Thank you for contributing to Zirco! Your efforts help make the language better for everyone.
