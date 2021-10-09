Tests the file command
  $ echo "int main() { return 0; }" > main.c
  $ cc -c main.c
  $ CONAN=../database/ conan.file --mime main.o
  application/x-object
  $ cc main.o
  $ CONAN=../database/ conan.file --mime a.out
  application/x-sharedlib
  $ echo "foo" | gzip -c - > foo.gzip
  $ CONAN=../database/ conan.file --mime foo.gzip
  application/gzip
  $ mkdir tarball
  $ echo "foo" > tarball/foo
  $ tar cf tarball.tar tarball
  $ CONAN=../database/ conan.file --mime tarball.tar
  application/x-gtar
