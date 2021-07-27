val database : directory:string -> Conan.Tree.t

val run :
  database:string -> string -> (Conan.Metadata.t, [> `Msg of string ]) result
