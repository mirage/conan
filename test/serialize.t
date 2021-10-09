  $ conan.serialize ../database/ -o .
  $ test -f elf.ml
  $ cat >dune <<EOF
  > (executable
  >  (name conan_database)
  >  (libraries conan conan.unix))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ dune build ./conan_database.exe
