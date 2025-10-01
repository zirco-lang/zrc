# Error Formatting Investigation

This document investigates two popular Rust error formatting crates as alternatives to the current custom error formatting implementation in `zrc_diagnostics`.

## Background

The Zirco compiler currently uses a custom error formatting implementation that displays:
- Error severity and message
- Line numbers with source code
- Visual indicators (carets) pointing to the error location
- ANSI color styling for better readability

## Investigated Crates

### 1. codespan-reporting

**Crate:** https://crates.io/crates/codespan-reporting  
**Version:** 0.11.1 (latest compatible with current dependencies)

#### Features
- Rich diagnostic messages with multiple labels
- Support for primary and secondary labels
- Notes and help messages
- File location information (filename:line:column)
- Customizable rendering styles
- Well-maintained, used by many Rust projects

#### Advantages
- ✅ Professional, standardized error format
- ✅ Support for multiple spans in a single diagnostic (helpful for showing related locations)
- ✅ Built-in support for notes, warnings, and help messages
- ✅ Highly customizable rendering
- ✅ Clean, box-drawing character style
- ✅ Better handling of multi-line spans

#### Disadvantages
- ❌ Requires creating a file database (SimpleFiles) for each diagnostic
- ❌ More complex API compared to current implementation
- ❌ Slightly more verbose setup code
- ❌ Additional dependency (~50KB)

### 2. ariadne

**Crate:** https://crates.io/crates/ariadne  
**Version:** 0.4.1 (latest compatible with current dependencies)

#### Features
- Beautiful, colorful error messages inspired by Rust's own errors
- Compact and visually appealing output
- Support for labels with colors
- Character-based drawing for error indicators
- Simple API

#### Advantages
- ✅ Very visually appealing output
- ✅ Simpler API than codespan-reporting
- ✅ Compact error messages
- ✅ Unicode box-drawing characters for professional look
- ✅ Inspired by rustc's error format (familiar to Rust developers)
- ✅ Good for terminal output

#### Disadvantages
- ❌ Less flexible than codespan-reporting
- ❌ Focused primarily on terminal output
- ❌ Additional dependency (~40KB)
- ❌ Fewer options for customization

## Comparison

### Current Implementation

```
error: unable to resolve identifier `unknown_var`
 3 |     let y = unknown_var;
   |           ^^^^^^^^^^^
```

**Pros:**
- Lightweight, no external dependencies
- Simple and straightforward
- Full control over formatting
- Minimal overhead

**Cons:**
- Limited to single-span errors
- No built-in support for notes or related information
- Manual span calculation and formatting
- Less sophisticated than modern error formatters

### codespan-reporting Output

```
error: unable to resolve identifier `unknown_var`
  ┌─ example.zr:3:11
  │
3 │     let y = unknown_var;
  │           ^^^^^^^^^^^
```

**Visual improvements:**
- File path with line:column information
- Box-drawing characters for cleaner look
- More space for readability
- Professional appearance

### ariadne Output

```
Error: unable to resolve identifier `unknown_var`
   ╭─[example.zr:3:11]
   │
 3 │     let y = unknown_var;
───╯
```

**Visual improvements:**
- Compact but clear format
- Elegant box characters
- Good balance between information and visual noise
- Familiar to Rust developers

## Recommendations

### Short Term (Current Investigation)

The investigation shows that both crates provide significant improvements over the current implementation:

1. **For immediate adoption:** Consider **ariadne** due to:
   - Simpler integration
   - Beautiful output that Rust developers are familiar with
   - Less boilerplate code
   - Good balance of features vs complexity

2. **For advanced features:** Consider **codespan-reporting** if:
   - Need to show multiple related spans in one diagnostic
   - Want extensive customization options
   - Need to support different output formats
   - Plan to add notes, warnings, and help messages

### Implementation Strategy

Both formatters have been implemented as **optional features** in `zrc_diagnostics`:

```toml
[features]
codespan = ["codespan-reporting"]
ariadne-fmt = ["ariadne"]
```

This allows:
- Testing both formatters without commitment
- Backwards compatibility with current formatter
- Easy migration path
- User choice (if exposing as CLI option)

### Code Examples

**Current usage:**
```rust
diagnostic.print(source)
```

**With codespan-reporting:**
```rust
diagnostic.print_with_codespan(source, "filename.zr")
```

**With ariadne:**
```rust
diagnostic.print_with_ariadne(source, "filename.zr")
```

## Testing

All implementations have been tested and work correctly. Run the comparison example:

```bash
# Current formatter only
cargo run --example compare_formatters

# With codespan-reporting
cargo run --example compare_formatters --features codespan

# With ariadne
cargo run --example compare_formatters --features ariadne-fmt

# All formatters
cargo run --example compare_formatters --all-features
```

## Next Steps

1. **Gather feedback** from the team on which formatter to adopt
2. **Consider migration path:**
   - Start with feature flag
   - Gradually migrate error sites
   - Eventually deprecate old formatter
3. **Extend capabilities:**
   - Add support for multiple spans per diagnostic
   - Implement notes and help messages
   - Add warnings and hints
4. **Update documentation** once a choice is made

## Conclusion

Both `codespan-reporting` and `ariadne` offer significant improvements over the current custom implementation. The choice between them depends on priorities:

- **Choose ariadne** for: simplicity, beauty, and Rust-like familiarity
- **Choose codespan-reporting** for: advanced features and extensive customization

The current implementation remains available and can be used alongside either option during migration.
