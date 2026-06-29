Ensure OCaml is installed and opam is up to date. If not, run:
`bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"`
to install Ocaml and initialize opam.

Run `opam init` in the root directory of this project and install the following libraries:
`opam install dune raylib batteries mtime core`

Run the game with:
```
dune build
dune exec bin/main.exe
```
