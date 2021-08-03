  $ conan.serialize ../examples/
  $ test -f ../examples/elf.ml
  $ cd ../examples/
  $ cat >dune <<EOF
  > (executable
  >  (name conan_database)
  >  (libraries conan conan.unix))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ dune build ./conan_database.exe
