# Error Formatter Comparison - Visual Examples

This document shows side-by-side visual comparisons of the three error formatting approaches.

## Example 1: Unknown Identifier

### Current Formatter
```
error: unable to resolve identifier `unknown_var`
 3 |     let y = unknown_var;
   |           ^^^^^^^^^^^
```

### codespan-reporting
```
error: unable to resolve identifier `unknown_var`
  ┌─ example.zr:3:11
  │
3 │     let y = unknown_var;
  │           ^^^^^^^^^^^
```

### ariadne
```
Error: unable to resolve identifier `unknown_var`
   ╭─[example.zr:3:11]
   │
 3 │     let y = unknown_var;
───╯
```

## Example 2: Type Mismatch

### Current Formatter
```
error: expected `string`, got `i32`
 4 |     println("Hello {}", x);
   |                ^
```

### codespan-reporting
```
error: expected `string`, got `i32`
  ┌─ example.zr:4:16
  │
4 │     println("Hello {}", x);
  │                ^
```

### ariadne
```
Error: expected `string`, got `i32`
   ╭─[example.zr:4:16]
   │
 4 │     println("Hello {}", x);
───╯
```

## Example 3: Multi-Span Error (codespan-reporting advantage)

### Current Formatter (Can only show one location)
```
error: identifier `x` already in use
 1 | let x: i32 = 5;
   |                 
 2 | let x: string = "hello";
   | ^^
```

### codespan-reporting (Shows both locations + note)
```
error: identifier `x` is already in use
  ┌─ example.zr:2:5
  │
1 │ let x: i32 = 5;
  │     - first declared here
2 │ let x: string = "hello";
  │     ^ second declaration of `x`
  │
  = consider using a different name or removing one of the declarations
```

### ariadne (Shows both locations + help)
```
Error: identifier `x` is already in use
   ╭─[example.zr:2:5]
   │
 1 │ let x: i32 = 5;
   │     ┬  
   │     ╰── first declared here
 2 │ let x: string = "hello";
   │     ┬  
   │     ╰── second declaration of `x`
   │ 
   │ Help: consider using a different name or removing one of the declarations
───╯
```

## Key Differences

### Visual Style
- **Current**: Simple, minimal, uses `|` for line indicators
- **codespan-reporting**: Uses box-drawing characters (`┌─│`), includes file:line:col
- **ariadne**: Uses rounded box characters (`╭─│╯`), compact and elegant

### Information Density
- **Current**: Low - just error and single location
- **codespan-reporting**: High - file location, multiple spans, notes
- **ariadne**: Medium-High - multiple spans, help/note messages, colors

### Multi-Span Support
- **Current**: ❌ No (limited to single span)
- **codespan-reporting**: ✅ Yes (with primary/secondary labels)
- **ariadne**: ✅ Yes (with colored labels)

### Notes/Help Messages
- **Current**: ❌ No
- **codespan-reporting**: ✅ Yes (notes)
- **ariadne**: ✅ Yes (help, note)

## Recommendation Summary

Choose based on priorities:

1. **Keep Current** - If minimal dependencies and simple errors are sufficient
2. **Use ariadne** - For beautiful, Rust-like errors with good balance of features
3. **Use codespan-reporting** - For maximum flexibility and professional appearance

Both `ariadne` and `codespan-reporting` offer significant improvements over the current implementation, especially for:
- Multi-location errors (e.g., "defined here, used there")
- Providing helpful suggestions and notes
- Professional appearance matching modern compiler standards
