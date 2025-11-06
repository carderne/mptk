# Grammar description

This Pest grammar parses GMPL (GNU MathProg Language), the modeling language used by GLPK for linear and mixed-integer programming problems.

## Covered Elements

### Model Section (.mod files)
- **Variables** (`var`): Supports bounds and type constraints
- **Parameters** (`param`): Handles attributes (integer/binary/symbolic), conditions, assignments, and defaults
- **Sets** (`set`): Basic set declarations
- **Constraints** (`s.t.`, `subject to`): Multiple constraint expressions with domain specifications
- **Objectives** (`minimize`, `maximize`): Single objective with expressions

### Data Section (.dat files)
- **Set data**: Value assignments to declared sets
- **Parameter data**: Supports multi-dimensional parameter tables with row/column layouts and default values

### Language Features
- **Domains**: Indexing sets with filtering conditions (`{i in SET: condition}`)
- **Expressions**: Arithmetic operations, conditional expressions (`if-then-else`)
- **Functions**: `sum`, `min`, `max` with domain iteration
- **Subscripting**: Multi-dimensional array indexing with shifts (`var[i,j+1]`)
- **Logical expressions**: Comparisons and boolean operators for constraints and conditionals
- **Comments**: Single-line (`#`) and multi-line (`/* */`)

## Not Covered

The grammar **ignores** several GMPL features:
- `for` loops (parsed but not processed)
- `table` statements (output formatting)
- `printf` statements (display)
- `check` statements (validation)
- `solve` and `end` directives

**Missing features:**
- Advanced functions (abs, ceil, floor, log, exp, etc.)
- Set operations (union, intersection, difference)
- String manipulation
- Complex parameter dependencies
- Display and write statements

## Caveats

1. **Incomplete expression parsing**: The grammar handles basic arithmetic and function calls but may fail on complex nested expressions or less common operators.

2. **Data format limitations**: The parameter data parsing assumes a specific table format. Other GMPL data layouts (transposed tables, sparse formats) may not parse correctly.

3. **Validation vs parsing**: This grammar ensures syntactic correctness only. It doesn't validate semantic rules like variable/set existence, type compatibility, or domain consistency.

4. **Comment handling**: While comments are recognized, they're silently dropped during parsing.

5. **String literals**: Handled minimally—mainly for ignored statements like `table` paths.

## Approach

The grammar prioritizes **core optimization modeling** over auxiliary features. It focuses on extracting the mathematical model structure (variables, constraints, objectives, parameters) needed for optimization solvers, while gracefully ignoring procedural elements (loops, printing) that don't affect the LP/MIP formulation.

The design separates model and data sections, reflecting GMPL's typical split between `.mod` and `.dat` files, though both can coexist in one file.
