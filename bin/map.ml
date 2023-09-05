module Map = Map.Make (String)

let ( / ) = Filename.concat
let mime_to_extension = ref true
let database = ref None

let add ~mime ~exts m =
  if !mime_to_extension then
    match Map.find_opt mime m with
    | Some exts' ->
        let exts = List.merge String.compare exts exts' in
        let exts = List.sort_uniq String.compare exts in
        Map.add mime exts m
    | None -> Map.add mime exts m
  else List.fold_left (fun m ext -> Map.add ext [ mime ] m) m exts

let aggregate database acc filename =
  let ic = open_in (database / filename) in
  let rs = Conan.Parse.parse_in_channel ic in
  close_in ic;
  match rs with
  | Ok lines ->
      let _, tree =
        List.fold_left
          (fun (line, tree) v ->
            (succ line, Conan.Tree.append ~filename ~line tree v))
          (1, Conan.Tree.empty) lines
      in
      Conan.Process.mimes_and_extensions ~f:add acc
        (Conan.Process.database ~tree)
  | Error _err -> acc

let run database =
  let files = Sys.readdir database in
  let files = Array.to_list files in
  let map = List.fold_left (aggregate database) Map.empty files in
  Fmt.pr "%a\n%!"
    Fmt.(
      Dump.iter_bindings Map.iter (const string "map") string (Dump.list string))
    map

let anonymous_argument v =
  match !database with None -> database := Some v | Some _ -> ()

let usage =
  Format.asprintf "%s [--mime-to-extension] <database>\n%!" Sys.argv.(0)

let spec =
  [
    ( "--mime-to-extension",
      Arg.Set mime_to_extension,
      "Generate a map from extensions to MIME types." );
  ]

let exit_success = 0
let exit_failure = 1

let () =
  Arg.parse spec anonymous_argument usage;
  match !database with
  | None ->
      Format.eprintf "%s" usage;
      exit exit_failure
  | Some database ->
      run database;
      exit exit_success
