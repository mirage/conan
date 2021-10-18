open Mirage

let main = foreign
  ~packages:[ package "conan-database" ~sublibs:[ "light"; ]
            ; package "conan" ~sublibs:[ "string" ] ]
  "Unikernel.Make" (console @-> job)

let () =
  register "main" [ main $ default_console ]
