### v0.0.7 2026-03-02 (Paris) France

- Delete useless dependencies for the mirage support (@dinosaure, #41)
- Fix our `lseek` usage on `conan.file` (@dinosaure, #40)
- Improve our search command and our pretty-printer (@dinosaure, #43)
- Fix infinite loop on our lwt support and use `bstr` instead of `bigstringaf` (@dinosaure, spotted by @zoggy, #44, #45)
- Fix the reproducibility of our database (@hannesm, #46)

### v0.0.6 2024-04-08 (Paris) France

- Fix for `re.1.12.0` (@hannesm, #34)

### v0.0.5 2023-09-11 (Paris) France

- Update the distribution with re.1.11.0 (@dinosaure, #29)
- Generate a map between MIME types and extensions (@dinosaure, #31)
  A new package is available `conan-database.bindings`
- Update the database with the last version of file/file (@dinosaure, #32)

### v0.0.4 2023-07-09 (Paris) France

- Fix the OCaml 5.1 support (@Octachron, #27)

### v0.0.3 2023-04-24 (Espagne) Madrid

- Fix the OCaml 5 support (@dinosaure, @Leonidas-from-XIV, #21)
- Apply `ocamlformat.0.24.1` (@dinosaure, #22)
- Fix lower bounds (@gridbugs, #23)
- Add `-p` name to dune install invocation (@reynir, #24, #19)
- Fix the MacOS support (@dinosaure, #25)

### v0.0.2 2022-04-12 (France) Paris

- Delete tests about unikernels (@dinosaure, #12)
- Prepend generated files by `conan_` (@dinosaure, #13)
- Fix HTML recognition (@dinosaure, #14)
- Fix the read syscall for `conan.string` (@dinosaure, #15)

### v0.0.1 2021-10-19 (France) Paris

- First release of `conan`
