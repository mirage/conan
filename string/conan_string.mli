val run :
  database:Conan.Process.database ->
  string ->
  (Conan.Metadata.t, [> `Msg of string ]) result
