open Mirage

let main = foreign
  ~packages:[ package "conan.light"
            ; package "conan.string" ]
  "Unikernel.Make" (console @-> job)

let () =
  register "main" [ main $ default_console ]
