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
    bench_parse_struct
);

criterion_group!(
    typechecking,
    bench_typeck_simple,
    bench_typeck_fibonacci,
    bench_typeck_struct
);

criterion_group!(
    codegen,
    bench_codegen_simple,
    bench_codegen_fibonacci,
    bench_codegen_struct
);

criterion_group!(
    end_to_end,
    bench_e2e_simple,
    bench_e2e_fibonacci,
    bench_e2e_struct,
    bench_e2e_optimized
);

criterion_main!(parsing, typechecking, codegen, end_to_end);
