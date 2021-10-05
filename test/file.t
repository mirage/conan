Tests the file command
  $ echo "int main() { return 0; }" > main.c
  $ cc -c main.c
  $ CONAN=../examples/ conan.file --mime main.o
  application/x-object
  $ cc main.o
  $ CONAN=../examples/ conan.file --mime a.out
  application/x-sharedlib
  $ echo "foo" | gzip -c - > foo.gzip
  $ CONAN=../examples/ conan.file --mime foo.gzip
  application/gzip
  $ mkdir tarball
  $ echo "foo" > tarball/foo
  $ tar cf tarball.tar tarball
  $ CONAN=../examples/ conan.file --mime tarball.tar
  application/x-gtar
