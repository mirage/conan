open Rresult

let database = Conan.Process.database ~tree:Conan_light.tree

let make_simple_test ~name contents expected =
  Alcotest.test_case name `Quick @@ fun () ->
  match Conan_string.run ~database contents with
  | Ok m ->
      Alcotest.(check (option string))
        "metadata" (Conan.Metadata.mime m) expected
  | Error (`Msg err) -> Alcotest.failf "%s." err

let simple_gzip =
  [
    "\x1f\x8b\x08\x08\xc6\x5c\x5c\x61\x00\x03\x66\x6f\x6f\x00\x4b\xcb";
    "\xcf\x07\x00\x21\x65\x73\x8c\x03\x00\x00\x00";
  ]

let test00 =
  make_simple_test ~name:"test00 (gzip)"
    (String.concat "" simple_gzip)
    (Some "application/gzip")

let test01 =
  make_simple_test ~name:"test01 (zlib)"
    "\x78\x9c\x4b\xcb\xcf\x07\x00\x02\x82\x01\x45" (Some "application/zlib")

let test02 =
  make_simple_test ~name:"test02 (html)"
    {html|<!doctype html>

<html lang="en">
<head>
  <meta charset="utf-8">
  <title>unipi</title>
</head>

<body>
<h1>Hello MirageOS!</h1>
</body>
</html>
|html}
    (Some "text/html")

let () = Alcotest.run "file" [ ("simple", [ test00; test01; test02 ]) ]
