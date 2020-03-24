# Conan, a detective which wants to identify your file

**NOTE**: this project still is experimental and does not implement all things
available into the `file` command. You probably got an `assert false` or an
`invalid_arg`. It's because the `libmagic`'s DSL is a bit complex.

`conan` wants to be an other implementation of `file` in OCaml. The final goal
is to be able to use it into a [MirageOS][mirageos]. By this fact, it must be
system-agnostic (no UNIX _syscall_).

As an re-implementation of `file`, it trusts on the `libmagic`'s DSL to be able
to identity your file. You can see how to describe a _format_ into the
`example/` directory or you can read the man (`man 5 magic`).

Of course, as an implementation in OCaml, it wants to provide a type-safe way to
identify your file. By this fact, `conan` provides a _type-safe_ little OCaml
DSL and show a nice play with GADTs - API is not fixed yet, but the core seems
good enough to complete `conan` with this implementation.

## Dependency

The only dependencies of the _core_ is `re` and `ocamllex`. Then, binary uses
the `unix` module of course. The goal is to provide a widely-used tool with
smallest dependencies.
