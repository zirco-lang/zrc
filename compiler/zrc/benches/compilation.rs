//! Compilation performance benchmarks for the Zirco compiler.
//!
//! This benchmark suite measures the performance of various compilation stages:
//! - Lexing and parsing
//! - Type checking
//! - Code generation
//! - End-to-end compilation
//!
//! Run with: `cargo bench --bench compilation`

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use zrc_codegen::{DebugLevel, OptimizationLevel};

/// Sample Zirco code for benchmarking - simple hello world
const SIMPLE_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

fn main() {
    printf("Hello, World!\n");
}
"#;

/// Sample Zirco code for benchmarking - fibonacci (recursive)
const FIBONACCI_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

fn fibonacci(n: i32) -> i32 {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn main() {
    let i = 0;
    while (i < 10) {
        printf("fibonacci(%d) = %d\n", i, fibonacci(i));
        i = i + 1;
    }
}
"#;

/// Sample Zirco code for benchmarking - struct example
const STRUCT_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

struct Point {
    x: i32,
    y: i32
}

fn distance_squared(p1: Point, p2: Point) -> i32 {
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;
    return dx * dx + dy * dy;
}

fn main() {
    let p1: Point;
    p1.x = 0;
    p1.y = 0;

    let p2: Point;
    p2.x = 3;
    p2.y = 4;

    let dist_sq = distance_squared(p1, p2);
    printf("Distance squared: %d\n", dist_sq);
}
"#;

/// Sample Zirco code for benchmarking - pointer operations
const POINTER_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

fn swap(a: *i32, b: *i32) {
    let temp = *a;
    *a = *b;
    *b = temp;
}

fn main() {
    let x = 42;
    let y = 17;

    printf("Before swap: x=%d, y=%d\n", x, y);
    swap(&x, &y);
    printf("After swap: x=%d, y=%d\n", x, y);
}
"#;

/// Sample Zirco code for benchmarking - global variables
const GLOBAL_VARS_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

// Global constants with initializers
let MAX_COUNT: i32 = 100;
let ENABLED: bool = true;
let VERSION: i8 = 1i8;

// Global variable without initializer (zero-initialized)
let counter: i32;

fn increment_counter() {
    counter = counter + 1;
}

fn get_counter() -> i32 {
    return counter;
}

fn is_below_max() -> bool {
    return counter < MAX_COUNT;
}

fn main() {
    printf("Initial counter: %d\n", counter);
    printf("Max count: %d\n", MAX_COUNT);
    printf("Version: %d\n", VERSION);
    printf("Enabled: %d\n", ENABLED);

    increment_counter();
    increment_counter();
    increment_counter();

    printf("After 3 increments: %d\n", get_counter());

    if (is_below_max()) {
        printf("Still below maximum\n");
    }
}
"#;

/// Sample Zirco code for benchmarking - loops
const LOOP_CODE: &str = r#"
fn printf(format: *u8, ...) -> i32;

fn print_numbers(n: i32) {
    for (let i = 1; i <= n; i = i + 1) {
        printf("%d\n", i);
    }
}

fn main() {
    print_numbers(5);
}
"#;

/// Benchmark parsing performance on simple code
fn bench_parse_simple(c: &mut Criterion) {
    c.bench_function("parse_simple", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(SIMPLE_CODE));
        });
    });
}

/// Benchmark parsing performance on fibonacci code
fn bench_parse_fibonacci(c: &mut Criterion) {
    c.bench_function("parse_fibonacci", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(FIBONACCI_CODE));
        });
    });
}

/// Benchmark parsing performance on struct code
fn bench_parse_struct(c: &mut Criterion) {
    c.bench_function("parse_struct", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(STRUCT_CODE));
        });
    });
}

/// Benchmark parsing performance on pointer operations
fn bench_parse_pointer(c: &mut Criterion) {
    c.bench_function("parse_pointer", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(POINTER_CODE));
        });
    });
}

/// Benchmark parsing performance on global variables
fn bench_parse_globals(c: &mut Criterion) {
    c.bench_function("parse_globals", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(GLOBAL_VARS_CODE));
        });
    });
}

/// Benchmark parsing performance on loops
fn bench_parse_loop(c: &mut Criterion) {
    c.bench_function("parse_loop", |b| {
        b.iter(|| {
            let _ = zrc_parser::parser::parse_program(black_box(LOOP_CODE));
        });
    });
}

/// Benchmark type checking performance on simple code
fn bench_typeck_simple(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(SIMPLE_CODE).unwrap();
    c.bench_function("typeck_simple", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark type checking performance on fibonacci code
fn bench_typeck_fibonacci(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(FIBONACCI_CODE).unwrap();
    c.bench_function("typeck_fibonacci", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark type checking performance on struct code
fn bench_typeck_struct(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(STRUCT_CODE).unwrap();
    c.bench_function("typeck_struct", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark type checking performance on pointer operations
fn bench_typeck_pointer(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(POINTER_CODE).unwrap();
    c.bench_function("typeck_pointer", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark type checking performance on global variables
fn bench_typeck_globals(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(GLOBAL_VARS_CODE).unwrap();
    c.bench_function("typeck_globals", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark type checking performance on loops
fn bench_typeck_loop(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(LOOP_CODE).unwrap();
    c.bench_function("typeck_loop", |b| {
        b.iter(|| {
            let _ = zrc_typeck::typeck::type_program(black_box(ast.clone()));
        });
    });
}

/// Benchmark code generation performance on simple code
fn bench_codegen_simple(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(SIMPLE_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_simple", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(SIMPLE_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark code generation performance on fibonacci code
fn bench_codegen_fibonacci(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(FIBONACCI_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_fibonacci", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(FIBONACCI_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark code generation performance on struct code
fn bench_codegen_struct(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(STRUCT_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_struct", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(STRUCT_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark code generation performance on pointer operations
fn bench_codegen_pointer(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(POINTER_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_pointer", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(POINTER_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark code generation performance on global variables
fn bench_codegen_globals(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(GLOBAL_VARS_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_globals", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(GLOBAL_VARS_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark code generation performance on loops
fn bench_codegen_loop(c: &mut Criterion) {
    let ast = zrc_parser::parser::parse_program(LOOP_CODE).unwrap();
    let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();

    c.bench_function("codegen_loop", |b| {
        b.iter(|| {
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(LOOP_CODE),
                black_box(typed_ast.clone()),
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark end-to-end compilation on simple code
fn bench_e2e_simple(c: &mut Criterion) {
    c.bench_function("e2e_simple", |b| {
        b.iter(|| {
            let ast = zrc_parser::parser::parse_program(black_box(SIMPLE_CODE)).unwrap();
            let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(SIMPLE_CODE),
                typed_ast,
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark end-to-end compilation on fibonacci code
fn bench_e2e_fibonacci(c: &mut Criterion) {
    c.bench_function("e2e_fibonacci", |b| {
        b.iter(|| {
            let ast = zrc_parser::parser::parse_program(black_box(FIBONACCI_CODE)).unwrap();
            let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(FIBONACCI_CODE),
                typed_ast,
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark end-to-end compilation on struct code
fn bench_e2e_struct(c: &mut Criterion) {
    c.bench_function("e2e_struct", |b| {
        b.iter(|| {
            let ast = zrc_parser::parser::parse_program(black_box(STRUCT_CODE)).unwrap();
            let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(STRUCT_CODE),
                typed_ast,
                OptimizationLevel::None,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

/// Benchmark end-to-end compilation with optimization
fn bench_e2e_optimized(c: &mut Criterion) {
    c.bench_function("e2e_fibonacci_optimized", |b| {
        b.iter(|| {
            let ast = zrc_parser::parser::parse_program(black_box(FIBONACCI_CODE)).unwrap();
            let typed_ast = zrc_typeck::typeck::type_program(ast).unwrap();
            let _ = zrc_codegen::cg_program_to_string(
                black_box("bench"),
                black_box("."),
                black_box("bench.zr"),
                black_box("zrc bench.zr"),
                black_box(FIBONACCI_CODE),
                typed_ast,
                OptimizationLevel::Aggressive,
                DebugLevel::None,
                &zrc_codegen::get_native_triple(),
                black_box("generic"),
            );
        });
    });
}

criterion_group!(
    parsing,
    bench_parse_simple,
    bench_parse_fibonacci,
    bench_parse_struct,
    bench_parse_pointer,
    bench_parse_globals,
    bench_parse_loop
);

criterion_group!(
    typechecking,
    bench_typeck_simple,
    bench_typeck_fibonacci,
    bench_typeck_struct,
    bench_typeck_pointer,
    bench_typeck_globals,
    bench_typeck_loop
);

criterion_group!(
    codegen,
    bench_codegen_simple,
    bench_codegen_fibonacci,
    bench_codegen_struct,
    bench_codegen_pointer,
    bench_codegen_globals,
    bench_codegen_loop
);

criterion_group!(
    end_to_end,
    bench_e2e_simple,
    bench_e2e_fibonacci,
    bench_e2e_struct,
    bench_e2e_optimized
);

criterion_main!(parsing, typechecking, codegen, end_to_end);
