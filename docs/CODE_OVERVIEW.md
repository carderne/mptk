# Code Overview

MPTK (MathProg Translation Kit) is a Rust-based parser for GLPK's MathProg (GMPL) language, which is used to define linear and mixed-integer programming problems. The project parses `.mod` model files and `.dat` data files into a strongly-typed intermediate representation, with the long-term goal of converting them to MPS (Mathematical Programming System) format.

## Architecture

The codebase follows a clean three-layer architecture. At the bottom layer sits the Pest grammar definition in grammar.pest, which specifies the complete syntax of GMPL including variables, parameters, sets, objectives, and constraints. This grammar handles complex language features like multi-dimensional domains, conditional expressions, aggregation functions (sum, min, max), and mathematical operators.

The middle layer is the parser in loader.rs, which operates in two stages. First, the `parse()` function uses Pest to validate input files against the grammar and generate an abstract syntax tree. Second, the `consume()` function walks this tree and converts it into typed Rust structures. This two-stage approach separates syntactic validation from semantic interpretation.

The top layer is the intermediate representation defined in data.rs, which contains comprehensive Rust types representing every GMPL construct. The main types include `Var` for decision variables, `Param` for parameters, `Set` for sets, `Objective` for optimization objectives, and `Constraint` for constraints. Each type captures the full semantics of its GMPL counterpart, including domains, bounds, conditions, and mathematical expressions. All types implement the Display trait for pretty-printing, which is currently the only output mechanism.

## Dependencies

The project relies on five core dependencies. Pest and pest_derive provide the parser generator framework using Parsing Expression Grammar, allowing the grammar to be defined declaratively in a separate .pest file. Clap handles command-line argument parsing using derive macros, providing a clean interface for the single `check` command. The log and env_logger crates provide structured logging throughout the parsing pipeline. Development dependencies include assert_cmd for CLI integration tests and predicates for assertion helpers.

## Workflow

When a user runs `mptk check model.mod data.dat`, the CLI in main.rs invokes the library functions. The loader reads the model file and calls `parse()`, which applies the Pest grammar rules to validate syntax and build a parse tree. The `consume()` function then traverses this tree, pattern-matching on grammar rules and constructing the corresponding IR types. For data files, the loader automatically prepends the `data;` keyword if missing, since standalone data files require this separator in GMPL.

The parser handles sophisticated GMPL features including nested domains with conditions, subscripted variables with index arithmetic, if-then-else expressions within constraints, and parameter tables with multiple dimensions. Each grammar rule maps to a specific IR type, with helper functions handling common patterns like expression trees and logical conditions.

## Current State and Limitations

The project successfully parses complex real-world models like OSeMOSYS, a 1,428-line energy system optimization model with hundreds of parameters and constraints. However, the implementation is incomplete in several ways. Error handling uses unwrap() in many places rather than proper error propagation. The parser only supports three aggregation functions (sum, min, max) and does not handle mathematical functions like abs or tan. Most critically, there is no MPS output yet, so the tool can only parse and display the IR, not convert models to a usable format.

The codebase is well-structured for future development, with clear separation between grammar, parsing, and data representation. The comprehensive type system in data.rs provides a solid foundation for adding semantic validation and format conversion. The test suite covers both CLI integration and library functionality, with examples ranging from simple production planning problems to complex multi-dimensional energy models. The project uses GitHub Actions for continuous integration and automatically publishes to crates.io when version tags are pushed.
