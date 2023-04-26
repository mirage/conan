# Conan, a detective which aggregate some clues to recognize MIME type

`conan` is a re-implementation of the famous command `file`:
```sh
$ file --mime image.png
image/png
$ conan.file --mime image.png
image/png
```

This program/library (see `libmagic`) is widely used on protocols to transmit
the MIME type of a file. It permits then to call the right program to
manipulate the given file.

For instance, the HTTP protocol transmits _via_ the `Content-Type` field the
MIME type of the body:
```http
HTTP/1.1 200 OK
Content-Type: image/png
Content-Length: 4096

<your image>
```

You can find the usage of `file` into many places such as your web browser (to
be able to execute the right application to interpret the given file) or your
Mail User Agent.

However, `file` is pretty old (1987), its implementation is in C, it was not
formalized and it does not have a standard. Take `file` as an engine for the
file recognition can be a risk (segmentation fault, undefined behavior,
unstable release process, etc.)

But `file` was involved for several years and it contains a great extensible
database which can be reliable due to its _seniority_. So, some famous
softwares decided to re-implement a subset of `file`/`libmagic` which is less
expressive/powerful but it does the job in a certain expectation.

## The DSL - `libmagic`

The `file`'s database use a certain language described by `man magic`:
- a line describe an operation
- an operation is:
  + a test of a certain value at a certain position into the given file
  + an anchor
  + a _jump_ instruction to an anchor
  + a MIME value
  + a _strength_ value

These operations are organized as a _tree_. An operation is prepended by a
_level_ (`>`) and, from it, we are able to construct the _decision tree_ which
describes multiple paths to recognize the MIME type of the given file.

For instance:
```magic
[0]
> [1]
>> [2]
> [3]
>> [4]
>>> [5]
```

produces this _decision tree_:
```
 [0]
  | \
 [1][3]
  |  |
 [2][4]
     |
    [5]
```

### The test operation

An operation is usually a test which compares the data starting at a particular
offset in the file with a byte value, a string or a numeric value. If the test
succeeds, we continue along the path according to your _decision tree_. For
instance, if `operation-0` succeeds, we will try `[1]` **and** `[3]`.

Along the process, we will aggregate multiple solutions which have a priority -
see the _strength_ value - and we will choose the highest one.

The test operations of the following fields:
- offset: A number specifying the offset (in bytes) into the file of the data
  which is to be tested. This offset can be _relative_ from the previous
  operation's offset if it begins with `&`.
- type: The type of the data to be tested. We implemented many types such as
  `byte`, `short`, `long`, `string` or `date`.
- test: the value to be compared with the value from the file.
- message: the message to be printed if the comparison succeeds.

### Let's play!

An example is more intersting than the _theory_. Let's try to recognize a
`zlib` archive. According to [RFC1950][] (where CMF and FLG are the first bytes
in MSB order of an `zlib` archive):

> The FCHECK value must be such that CMF and FLG, when viewed as
> a 16-bit unsigned integer stored in MSB order (CMF\*256 + FLG),
> is a multiple of 31.

Then, the first byte should have a CM = 8:
> CM (Compression method)
>    This identifies the compression method used in the file. CM = 8
>    denotes the "deflate" compression method with a window size up
>    to 32K.

And the RFC precises that CM should not be equal to `15` (as a reserved value),
so we can consider that `CM & 0x80` (the most significant bit) should not be
equal to 1.

Finally, we have 3 tests to do:
1) the 16-bits number (big-endian order) must be a multiple of 31
2) CM which is the 4 most significant bits of the first byte must be equal to 8
3) CM should not be equal to 15 and its most significant bit should not be
   equal to 1

In our syntax and according to the idea of a _decision tree_, we must **test**
step by step these assertions. At the end, we can say that the file is
probably an `application/zlib`:
```magic
0	beshort%31	=0
>0	byte&0xf	=8
>>0	byte&0x80	=0
!:mime	application/zlib
```

Now, let's play with `conan`:
```ocaml
open Rresult

let zlib =
  {file|0	beshort%31	=0
>0	byte&0xf	=8
>>0	byte&0x80	=0
!:mime	application/zlib
|file}

let tree = R.failwith_error_msg @@ Conan_unix.tree_of_string zlib
let () =
  if Array.length Sys.argv >= 2
  then
    let m = R.failwith_error_msg @@
      Conan_unix.run_with_tree tree Sys.argv.(1) in
    match Conan.Metadata.mime m with
    | Some v -> Fmt.pr "%s\n%!" v
    | None -> Fmt.epr "MIME type not found.\n%!"
  else Fmt.epr "%s <filename>" Sys.argv.(0)
```

This little program will only recognize "application/zlib" according to our
description above. Of course, the DSL can be more complex than that!

### Complex recognition

#### Indirect offset

Offsets do not need to be constant, but can also be read from the file being
examined. If the first character following the last `>` is a parenthesis then
the string inner is interpreted as an indirect offset. value at that offset is 
read, and is used again as an offset in the file.

For instance, such _tree_ will do an indirection from the unsigned long number
(little-endian) value available at the offset `0x3c`:
```magic
0		string	MZ
>0x18		leshort >0x3f
>>(0x3c.l)	string	PE\0\0	PE executable (MS-Windows)
>>(0x3c.l)	string	LE\0\0	LX executable (OS/2)
```

You should check the `man magic` to see the syntax and available types. You are
able to apply a calculation if the indirect offset can not be used directly
such as this example when we multiple the indirect offset with 512:
```magic
>0x18		leshort	<0x40
>>(4.s*512)	leshort	0x014c	COFF executable (MS-DOS, DJGPP)
>>(4.s*512)	leshort !0x014c	MZ executable (MS-DOS)
```

#### Relative offset

Moreover you can specify an offset relative to the end of the last up-level
field using `&` as a prefix to the offset:
```magic
0		string		MZ
>0x18		leshort		>0x3f
>>(0x3c.l)	string	PE\0\0	PE executable (MS-Windows)
>>>&0		leshort 0x14c	for Intel 80386
>>>&0		leshort 0x184	for DEC Alpha
```

And, of course, indirect and relative offsets can be combined.

#### Jump and recursion

It is possible to define a "named" magic instance that can be called from
another use magic entry, like a subroutine call. The offset of the subroutine
is relative to the caller.

To be able to call a subroutine, we use the `use` operation with the name of
the subroutine. You don't need to define the subroutine before the caller.
Indeed, `file` and `conan` collects all subroutines first and process then the
_decision tree_.

This is a simple example to determine if a length of the given file is odd or
even:
```magic
0	name		even
>0	byte		x	even
>>1	use		odd

0	name		odd
>0	byte		x	odd
>>1	use		even

0	byte		x
>0	use 		odd
```

#### Other operations

The `libmagic` DSL implements many things but as we said, a standard of it does
not exist. We mostly tried to do a reverse engineering on it to implement
operations. Some of them are not implemented - due to the lack of definitions
or just because we did not find them into the `file`'s database. Some others are
explicitely not implemented because we judge them as a hack instead of an
_homogene_ feature.

Then, we are mostly focus to deliver the MIME type instead of a full
description of the given file. `file` shows you many things such as the size
of the image, the bitrate of the sound, etc. We tried to implement them but
we are more focused on the MIME recognition.

### Experimental

According to what we said above, `conan` is experimental and for the usage
point of view, it can leak exceptions such as `Unimplemented feature`.

Then, even if a big work was done about types where we try to unify type of the
expected value and type of the test, the type expected by the message still is
weak (for many reasons). In other words, even if we can parse and process the
_decision tree_, we still are able to fail when we print out messages (because
we can not unify the type of the value and the expected type from the given
message).

Finally, `file` does not describe any standards about the database and `man`
pages are a bit obsolete according to what the `file` command do. For these
reasons, it's hard to prove/and say that we have the same behavior than `file`.
We try to be close to what it does, but in some edge cases, we can not ensure
that we will produce the same result as `file`.

Also, we did not discovered everything from the given database. Even if we can
parse and generate a decision tree from the database, some specific execution
paths can lead to an unexpected failure. We are prompted to fix them step by
step of course. Feel free to test and write an issue!

## MirageOS support

The other goal of `conan` is to be able to integrate the database into an
unikernel and to give an opportunity for an application (such as a web server)
to recognize MIME types of files.

### _syscalls_

As any MirageOS projects, `conan` abstracts required _syscalls_ to introspect
a file. In this way, `conan.string` exists and it is able to recognize the MIME
type of a given `string` (instead of a file). `lwt` support exists too which
manipulate a _stream_.

## Database

`conan` is able to parse a database and serialize it as a full OCaml value. The
distribution provides 2 databases:
- the `file`'s database
- the previous database without extra paths which don't not tag the MIME type

The second is lighter than the first and should be used only to get the MIME
type. Indeed, any information such as the size of the image or the bitrate of
the sound are deleted.

For instance, an unikernel for Solo5 with the ligher database is around 6 MB.

You can also build your own special database. If you know that you want to
recognize only few objects, you can merge `tree` values for these objects and
make a smaller database:

```ocaml
#require "conan-unix" ;;
#require "conan-database" ;;

let tree0 = Conan_compress.tree
let tree1 = Conan_ocaml.tree
let tree2 = Conan_audio.tree

let tree = List.fold_left Conan.Tree.merge Conan.tree.empty
  [ tree0; tree1; tree2 ]

let recognize_ocaml_or_archive_or_audio filename =
  Conan_unix.run_with_tree tree filename
```
