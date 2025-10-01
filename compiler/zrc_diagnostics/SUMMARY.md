# Investigation Summary: Error Formatting Crates

## Executive Summary

This investigation evaluated two popular Rust error formatting crates - **codespan-reporting** and **ariadne** - as potential replacements or alternatives to the current custom error formatting implementation in the Zirco compiler.

## What Was Done

1. **Analyzed current implementation** - Reviewed the existing custom formatter in `zrc_diagnostics`
2. **Implemented codespan-reporting** - Added as optional feature with complete integration
3. **Implemented ariadne** - Added as optional feature with complete integration
4. **Created examples** - Built comparison and advanced multi-span examples
5. **Documented findings** - Comprehensive documentation of pros/cons and recommendations

## Key Findings

### Current Implementation
- ✅ Lightweight, no dependencies
- ✅ Simple and fast
- ❌ Limited to single-span errors
- ❌ No support for notes/help messages
- ❌ Basic visual presentation

### codespan-reporting
- ✅ Professional, standardized format
- ✅ Multi-span support with primary/secondary labels
- ✅ Extensive customization options
- ✅ Support for notes and help messages
- ✅ Clean box-drawing character style
- ❌ More complex API
- ❌ Additional ~50KB dependency

### ariadne
- ✅ Beautiful, Rust-inspired output
- ✅ Simpler API than codespan
- ✅ Multi-span support with colored labels
- ✅ Support for help and note messages
- ✅ Compact, elegant formatting
- ❌ Additional ~40KB dependency
- ❌ Less customization than codespan

## Implementation Details

All three formatters are now available in `zrc_diagnostics`:

### Default (Current)
```rust
diagnostic.print(source)
```

### With codespan-reporting (feature: `codespan`)
```rust
diagnostic.print_with_codespan(source, "file.zr")
```

### With ariadne (feature: `ariadne-fmt`)
```rust
diagnostic.print_with_ariadne(source, "file.zr")
```

## Visual Comparison

See [VISUAL_COMPARISON.md](./VISUAL_COMPARISON.md) for detailed side-by-side examples.

### Single-Span Error

**Current:**
```
error: unable to resolve identifier `unknown_var`
 3 |     let y = unknown_var;
   |           ^^^^^^^^^^^
```

**codespan-reporting:**
```
error: unable to resolve identifier `unknown_var`
  ┌─ example.zr:3:11
  │
3 │     let y = unknown_var;
  │           ^^^^^^^^^^^
```

**ariadne:**
```
Error: unable to resolve identifier `unknown_var`
   ╭─[example.zr:3:11]
   │
 3 │     let y = unknown_var;
───╯
```

### Multi-Span Error (showing both formatters' advantage)

**Current (limited):**
```
error: identifier `x` already in use
 2 | let x: string = "hello";
   | ^^
```

**codespan-reporting:**
```
error: identifier `x` is already in use
  ┌─ example.zr:2:5
  │
1 │ let x: i32 = 5;
  │     - first declared here
2 │ let x: string = "hello";
  │     ^ second declaration of `x`
  │
  = consider using a different name...
```

**ariadne:**
```
Error: identifier `x` is already in use
   ╭─[example.zr:2:5]
   │
 1 │ let x: i32 = 5;
   │     ╰── first declared here
 2 │ let x: string = "hello";
   │     ╰── second declaration of `x`
   │ Help: consider using a different name...
───╯
```

## Recommendations

### For Immediate Adoption

**Recommended: ariadne**

Reasons:
1. Simpler integration (less API complexity)
2. Beautiful output familiar to Rust developers
3. Good balance of features vs. complexity
4. Smaller dependency footprint
5. Excellent for terminal-first workflow

### For Advanced Use Cases

**Consider: codespan-reporting**

When you need:
1. Extensive customization options
2. Different output formats
3. More control over rendering
4. Industry-standard diagnostic format

### Migration Strategy

The implementation uses **optional feature flags**, allowing:

1. **Phase 1** (Current): Both formatters available as optional features for evaluation
2. **Phase 2** (Future): Choose one formatter as default, keep old for compatibility
3. **Phase 3** (Future): Fully migrate to chosen formatter, deprecate old implementation

## Testing

All implementations pass tests:

```bash
# Test default (current formatter)
cargo test

# Test with codespan-reporting
cargo test --features codespan

# Test with ariadne
cargo test --features ariadne-fmt

# Test all formatters
cargo test --all-features
```

## Examples

Two comprehensive examples demonstrate the formatters:

1. **compare_formatters.rs** - Basic comparison of all three
2. **advanced_formatting.rs** - Multi-span capabilities

Run with:
```bash
cargo run --example compare_formatters --all-features
cargo run --example advanced_formatting --all-features
```

## Files Added

1. `src/formatters.rs` - Module structure for alternative formatters
2. `src/formatters/codespan_fmt.rs` - codespan-reporting implementation
3. `src/formatters/ariadne_fmt.rs` - ariadne implementation
4. `examples/compare_formatters.rs` - Basic comparison example
5. `examples/advanced_formatting.rs` - Multi-span example
6. `ERROR_FORMATTING_INVESTIGATION.md` - Detailed analysis
7. `VISUAL_COMPARISON.md` - Visual side-by-side comparison
8. `SUMMARY.md` - This file

## Next Steps

1. **Gather team feedback** on which formatter to adopt
2. **Choose default formatter** based on project priorities
3. **Update error generation sites** to leverage multi-span capabilities
4. **Add helper functions** for common error patterns
5. **Consider CLI flag** to let users choose formatter
6. **Performance testing** if needed for large codebases

## Conclusion

Both codespan-reporting and ariadne offer significant improvements over the current custom implementation. The investigation successfully demonstrates:

- **Feasibility**: Both can be integrated with minimal changes
- **Compatibility**: Backward compatibility maintained via feature flags
- **Quality**: Professional error messages matching modern standards
- **Value**: Multi-span support enables better error messages

**Recommendation**: Adopt **ariadne** for its simplicity and beauty, with the option to switch to codespan-reporting later if more advanced features are needed.

---

*Investigation completed by GitHub Copilot*
*All code tested and documented*
*Ready for team review and decision*
