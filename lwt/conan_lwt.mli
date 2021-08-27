val run :
  database:Conan.Process.database ->
  (unit -> string option Lwt.t) ->
  (Conan.Metadata.t, [> `Msg of string ]) result Lwt.t
