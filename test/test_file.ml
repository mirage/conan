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

(* XXX(dinosaure): even if this magic file is correct, it results on an
 * infinite loop with `file` where the semantic with [use] and relative offset
 * is not so clear on its side.
 *
 * [conan] did the choice to apply the relative offset as the new absolute
 * offset when we compute the associated tree. By this way, we only go further
 * along the file instead of to be stick on the absolute offset [0] as [file]
 * seems to do. *)
let odd_even =
  {file|
0        name    foo
>&0      byte    x      odd
>>&1     byte    x      even
>>>&1    use     foo

0        byte    x
>&0      use     foo
|file}

let test02 = make_simple_test ~name:"test02 (odd/even)" odd_even "" None

let test03 =
  make_simple_test ~name:"test03 (odd/even)" odd_even " " (Some "odd")

let test04 =
  make_simple_test ~name:"test04 (odd/even)" odd_even "  " (Some "odd even")

let test05 =
  make_simple_test ~name:"test05 (odd/even)" odd_even "   "
    (Some "odd even odd")

let () =
  Alcotest.run "file"
    [ ("simple", [ test00; test01; test02; test03; test04; test05 ]) ]
