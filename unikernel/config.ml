open Mirage

let main = foreign
  ~packages:[ package "conan" ~sublibs:[ "light"; "string" ] ]
  "Unikernel.Make" (console @-> job)

let () =
  register "main" [ main $ default_console ]
