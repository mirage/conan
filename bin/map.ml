module Map = Map.Make (String)

let ( / ) = Filename.concat
let mime_to_extension = ref true
let extension_to_mime = ref false
let database = ref None
let output = ref None

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

let run database ppf =
  let files = Sys.readdir database in
  let files = Array.to_list files in
  let map = List.fold_left (aggregate database) Map.empty files in
  Format.fprintf ppf "module Map = Map.Make (String)\n%!";
  Format.fprintf ppf "let map = Map.empty\n%!";
  Map.iter
    (fun k vs ->
      Format.fprintf ppf "@[<2>let map = Map.add %S %a map@]\n%!" k
        Conan.Serialize.(list string)
        vs)
    map

let anonymous_argument v =
  match !database with None -> database := Some v | Some _ -> ()

let usage =
  Format.asprintf "%s [--mime-to-extension] <database>\n%!" Sys.argv.(0)

let spec =
  [
    ("-o", Arg.String (fun str -> output := Some str), "The output filename.");
    ( "--mime-to-extension",
      Arg.Unit
        (fun () ->
          mime_to_extension := true;
          extension_to_mime := false),
      "Generate a map from MIME types to extensions." );
    ( "--extension-to-mime",
      Arg.Unit
        (fun () ->
          extension_to_mime := true;
          mime_to_extension := false),
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
      let output, close =
        match !output with
        | None -> (Format.std_formatter, ignore)
        | Some output when not (Sys.file_exists output) ->
            let oc = open_out output in
            (Format.formatter_of_out_channel oc, fun () -> close_out oc)
        | Some output ->
            Format.eprintf "%s already exists\n%!" output;
            exit exit_failure
      in
      run database output;
      close ();
      exit exit_success
