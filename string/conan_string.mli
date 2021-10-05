val tree_of_string : string -> (Conan.Tree.t, [> `Msg of string ]) result

val run :
  database:Conan.Process.database ->
  string ->
  (Conan.Metadata.t, [> `Msg of string ]) result
