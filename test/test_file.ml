open Rresult

let make_simple_test ~name database contents expected =
  Alcotest.test_case name `Quick @@ fun () ->
  let tree = R.failwith_error_msg @@ Conan_string.tree_of_string database in
  let database = Conan.Process.database ~tree in
  match Conan_string.run ~database contents with
  | Ok m ->
      Alcotest.(check (option string))
        "metadata" (Conan.Metadata.output m) expected
  | Error (`Msg err) -> Alcotest.failf "%s." err

let test00 =
  make_simple_test ~name:"test00"
    {file|
0       byte    0x66
>1      byte    0x6f
>>1     byte    0x6f    foo header
|file}
    "foo" (Some "foo header")

let test01 =
  make_simple_test ~name:"test01"
    {file|
0       byte    0x62
>1      byte    0x61
>>1     byte    0x72    bar header
|file}
    "foo" None

let odd_even =
  {file|
0	name		even
>0	byte		x	even
>>1	use		odd

0	name		odd
>0	byte		x	odd
>>1	use		even

0	byte		x
>0	use 		odd
|file}

let test02 =
  make_simple_test ~name:"test02 (odd/even)" odd_even (String.make 0 '\000')
    None

let test03 =
  make_simple_test ~name:"test03 (odd/even)" odd_even (String.make 1 '\000')
    (Some "odd")

let test04 =
  make_simple_test ~name:"test04 (odd/even)" odd_even (String.make 2 '\000')
    (Some "odd even")

let test05 =
  make_simple_test ~name:"test05 (odd/even)" odd_even (String.make 3 '\000')
    (Some "odd even odd")

let zlib =
  {file|
0       string/b        x
>0      beshort%31      =0
>>0     byte&0xf        =8
>>>0    byte&0x80       =0      zlib compressed data
!:mime  application/zlib
|file}

let test06 =
  make_simple_test ~name:"test06 (zlib)" zlib
    "\x78\x9c\x4b\xcb\xcf\x07\x00\x02\x82\x01\x45" (Some "zlib compressed data")

(* XXX(dinosaure):
   $> ocamlopt --version
   4.12.0
   $> touch caml.ml
   $> ocamlopt -c caml.ml
   $> hxd.caml caml.cmx
*)
let simple_caml_object =
  [
    "\x43\x61\x6d\x6c\x31\x39\x39\x39\x59\x30\x32\x39\x84\x95\xa6\xbe";
    "\x00\x00\x00\x77\x00\x00\x00\x13\x00\x00\x00\x4a\x00\x00\x00\x3f";
    "\x08\x00\x00\x28\x00\x24\x43\x61\x6d\x6c\x04\x01\xa0\x04\x02\x40";
    "\xa0\xa0\x26\x53\x74\x64\x6c\x69\x62\x90\x30\x4b\x04\xb4\xed\xa1";
    "\x9a\xa7\x22\xdf\x36\x51\x41\x89\x5f\xb3\x47\xa0\xa0\x38\x43\x61";
    "\x6d\x6c\x69\x6e\x74\x65\x72\x6e\x61\x6c\x46\x6f\x72\x6d\x61\x74";
    "\x42\x61\x73\x69\x63\x73\x90\x30\xb6\xc6\x69\x49\x55\xe1\x00\x01";
    "\xae\xd2\x67\x57\x11\x04\xa9\x61\xa0\xa0\x04\x0e\x90\x30\x84\x7b";
    "\xeb\xfe\x3c\xb8\x46\x8d\x5d\x47\x1f\xf3\x62\xaf\xc3\x70\x40\x40";
    "\x40\x40\x40\x90\x91\x80\x40\x33\x61\x55\x01\xde\xc3\xf2\xa2\x26";
    "\x84\xfc\x9e\x41\xae\xa9\xfe";
  ]

let caml =
  {file|
0	string	Caml1999	OCaml
>8	string	X		exec file
>8	string	I		interface file (.cmi)
>8	string	O		object file (.cmo)
>8	string	A		library file (.cma)
>8	string	Y		native object file (.cmx)
>8	string	Z		native library file (.cmxa)
>8	string	M		abstract syntax tree implementation file
>8	string	N		abstract syntax tree interface file
>9	string	>\0		(Version %3.3s)
|file}

let test07 =
  make_simple_test ~name:"test07 (caml)" caml
    (String.concat "" simple_caml_object)
    (Some "OCaml native object file (.cmx) (Version 029)")

let simple_gzip =
  [
    "\x1f\x8b\x08\x08\xc6\x5c\x5c\x61\x00\x03\x66\x6f\x6f\x00\x4b\xcb";
    "\xcf\x07\x00\x21\x65\x73\x8c\x03\x00\x00\x00";
  ]

let gzip =
  {file|
0       string          \037\213
>3	byte&0x18	=0
>>10	default		x		gzip compressed data
!:mime  application/gzip
>>>0	use	        gzip-info
>3      byte&0x18       >0              gzip compressed data
!:mime  application/gzip
>>0     use             gzip-info

0	name				gzip-info
>2	byte		<8		\b, reserved method
>2	byte		>8		\b, unknown method
>3	byte		&0x01		\b, ASCII
>3	byte		&0x02		\b, has CRC
>3	byte		&0x04		\b, extra field
>3	byte		&0x10		\b, has comment
>3	byte		&0x20		\b, encrypted
>4	ledate		>0		\b, last modified: %s
>8	byte		2		\b, max compression
>8	byte		4		\b, max speed
>9	byte		=0x03		\b, from Unix
|file}

(* XXX(dinosaure): this test is interesting to check that we extract correctly
 * the POSIX time of the gzipped file. *)

let test08 =
  make_simple_test ~name:"test08 (gzip)" gzip
    (String.concat "" simple_gzip)
    (Some
       "gzip compressed data, last modified: 2021-10-05T14:10:14-00:00, from \
        Unix")

let () =
  Alcotest.run "file"
    [
      ( "simple",
        [
          test00; test01; test02; test03; test04; test05; test06; test07; test08;
        ] );
    ]
