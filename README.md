# Conan, a detective which wants to identify your file

**NOTE**: this project still is experimental and we continue to improve/develop
it. However, `file` is an old/huge project where it's hard to find a true
specification. So, many parts are not yet implemented - some `assert false` 
or `failwith`/`invalid_arg` exist and can be raise for many non-obvious
reasons.

`conan` is a library which wants to handle the [_magic_][magic] specification
to be able to recognize files and specially MIME types. The library wants to
abstract _syscalls_ to be able to use it into a [MirageOS][mirage].

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

## How to use it?

`conan

## Dependency

The only dependencies of the _core_ is `re` and `ocamllex`. Then, binary uses
the `unix` module of course. The goal is to provide a widely-used tool with
smallest dependencies.

[mirageos]: https://mirage.io/
