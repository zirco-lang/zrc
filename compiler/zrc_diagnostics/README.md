# Diagnostics for the Zirco compiler

This module defines the diagnostics system for the Zirco compiler. It includes
various types of diagnostics such as errors and warnings that can be produced
during the compilation process.

The main types are:

-   [`Diagnostic`]: Represents a single diagnostic message, including its severity,
    message, and location (span).
-   [`DiagnosticKind`]: An enumeration of all possible kinds of diagnostics that
    can be produced, each with its own associated data.
-   [`Severity`]: An enumeration representing the severity level of a diagnostic
    (e.g., Error, Warning, Info).

These types are used throughout the compiler to report issues encountered
during parsing, type checking, and code generation.
