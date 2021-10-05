val tree : directory:string -> Conan.Tree.t

val tree_of_string : string -> (Conan.Tree.t, [> `Msg of string ]) result

val run_with_tree :
  Conan.Tree.t -> string -> (Conan.Metadata.t, [> `Msg of string ]) result

val run :
  database:string -> string -> (Conan.Metadata.t, [> `Msg of string ]) result
