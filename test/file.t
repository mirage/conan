Tests the file command
  $ echo "int main() { return 0; }" > main.c
  $ cc -c main.c
  $ RES=$(CONAN=../database/ conan.file --mime main.o)
application/x-matlab-data is a bug only on MacOS
Indeed, it seems that a compiled object in MacOS is not recognized only for MacOS homebrew and OCaml 5 into our CI
  $ test "$RES" = "application/x-object" || test "$RES" = "application/x-mach-binary" || test "$RES" = "application/x-matlab-data"
  $ cc main.o
  $ RES=$(CONAN=../database/ conan.file --mime a.out)
See the note about application/x-matlab-data
  $ test "$RES" = "application/x-pie-executable" || test "$RES" = "application/x-executable" || test "$RES" = "application/x-mach-binary" || test "$RES" = "application/x-matlab-data"
  $ echo "foo" | gzip -c - > foo.gzip
  $ CONAN=../database/ conan.file --mime foo.gzip
  application/gzip
  $ mkdir tarball
  $ echo "foo" > tarball/foo
  $ tar cf tarball.tar tarball
  $ RES=$(CONAN=../database/ conan.file --mime tarball.tar)
  $ test "$RES" = "application/x-gtar" || test "$RES" = "application/x-ustar"
  $ echo "<html><h1>Hello World!</h1></html>" > index.html
  $ CONAN=../database/ conan.file --mime index.html
  text/html
