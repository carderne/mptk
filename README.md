# MPTK (MathProg Translation Kit)

## Objectives:
- [x] A grammar capable of parsing [GMPL](https://en.wikibooks.org/wiki/GLPK/GMPL_(MathProg)) .mod and .dat files
- [x] Complete internal representation (IR) of parsed models and data
- [ ] Collate model and data sections
- [ ] Interpret functions, domains etc in the model 
- [ ] Output to [MPS](https://en.wikipedia.org/wiki/MPS_(format))

## Quickstart
Install
```bash
cargo install mptk
```

Usage
```bash
mptk osemosys.mod atlantis.dat
```

## GMPL
Language reference: https://en.wikibooks.org/wiki/GLPK/GMPL_(MathProg)

## Questions
1. MathProg has built-in functions like `abs(x)`, `max(x1, x2, ..., xn)`. These will need to be manually linearized, and not all will be supported (eg `tan(x)`).


## Development
Please install [cargo-make](https://github.com/sagiegurari/cargo-make):
```bash
cargo install cargo-make
```

The most useful dev commands are listed in `Makefile.toml`.

You can view available commands by running `cargo make`.

Run fmt, lint, check, test:
```bash
cargo make ci
```

Run against the full Osemosys model and Atlantic data:
```bash
cargo make run
```
