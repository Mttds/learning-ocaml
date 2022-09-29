# learning-ocaml

Repo to test Ocaml out.

# Ocaml ecosystem installation

```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

opam init          # Can take some time
eval $(opam env)

opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release

```

# ocamlformat

```
opam install ocamlformat
```

.ocamlformat is necessary in the prj folder.

# VS Code extension

OCaml Platform