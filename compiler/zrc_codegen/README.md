# LLVM code generator for Zirco

This module contains the LLVM code generator for the Zirco compiler. It translates the type-checked
abstract syntax tree (TAST) into LLVM intermediate representation (IR), which can then be
optimized and compiled to machine code.

The main entry point is the [`program::cg_program_to_buffer`] function, which takes a TAST and
produces an LLVM module. The code generator handles various language constructs such as
expressions, statements, functions, and control flow.
