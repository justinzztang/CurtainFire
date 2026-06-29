# CurtainFire
A domain-specific language for describing and generating bullet patterns in SHMUP (Bullet Hell) games.

CurtainFire requires relatively few engine features to implement; anything that supports pointers should work.

This repository contains an implementation of the tokenizer, parser, evaluator, and engine, written in OCaml.

## Usage
### Build & Run
```bash
dune build
dune exec bin/main.exe
```
### Load Custom Pattern
```bash
dune exec bin/main.exe <filename>
```
Files are formatted as follows:
```
Bullet pattern here...
<end>
Enemy behavior here...
<end>
Spawn X coordinate
<end>
Spawn Y coordinate
<end>
Enemy HP
```
Sample patterns are available in [`assets/patterns`](assets/patterns).

To turn on deaths (clearing the screen after a collision), add `DEATH_ON` as another argument:
```bash
dune exec bin/main.exe <filename> DEATH_ON
```

# Language Specification
The specification for the language is provided in [`Specification.md`](Specification.md).

# Acknowledgements
This project is an expansion of a school assignment. Thanks to my partners!