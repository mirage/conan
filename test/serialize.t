  $ conan.serialize ../examples/
  $ test -f ../examples/elf.ml
  $ cat >../examples/dune <<EOF
  > (executable
  >  (name conan_database)
  >  (libraries conan.unix conan))
  > EOF
  $ cat >../examples/dune-project <<EOF
  > (lang dune 2.0)
  > EOF
  $ cd ../examples/
  $ ulimit -s 16384
  $ dune build ./conan_database.exe
