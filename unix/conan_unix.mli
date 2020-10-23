val run :
  database:string -> string -> (Conan.Metadata.t, [> `Msg of string ]) result
