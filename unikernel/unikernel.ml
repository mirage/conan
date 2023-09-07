let database = Conan.Process.database ~tree:Conan_light.tree

let mime contents =
  match Conan_string.run ~database contents with
  | Ok m -> Conan.Metadata.mime m
  | Error _ -> None

module Make (Console : Mirage_console.S) = struct
  let logf console fmt = Fmt.kstr (Console.log console) fmt
  let zlib_contents = "\x78\x9c\x4b\xcb\xcf\x07\x00\x02\x82\x01\x45"

  let start console =
    logf console "MIME type: %a" Fmt.(option string) (mime zlib_contents)
end
