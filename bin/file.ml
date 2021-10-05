open Conan_unix

let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err

let database =
  match Sys.getenv "CONAN" with v -> v | exception _ -> "examples"

let run ?(fmt = `Usual) filename =
  run ~database filename >>= fun result ->
  match fmt with
  | `Usual -> (
      match Conan.Metadata.output result with
      | Some output ->
          Format.printf "%s: %s\n%!" filename output;
          Ok ()
      | None -> Error `Not_found)
  | `MIME -> (
      match Conan.Metadata.mime result with
      | None -> Error `Not_found
      | Some mime ->
          Format.printf "%s\n%!" mime;
          Ok ())

let pp_error ppf = function
  | #Conan.Parse.error as err -> Conan.Parse.pp_error ppf err
  | `Msg err -> Format.fprintf ppf "%s" err
  | `Not_found -> Format.fprintf ppf "Not found"

let mime = ref false

let filename = ref None

let anonymous_argument v =
  match !filename with None -> filename := Some v | Some _ -> ()

let usage = Format.asprintf "%s [--mime] filename\n%!" Sys.argv.(0)

let spec =
  [
    ( "--mime",
      Arg.Set mime,
      "Causes the file command to output mime type strings rather than the \
       more traditional human readable ones. Thus it may say 'text/plain; \
       charset=ascii' rather than 'ASCII text'" );
  ]

let exit_success = 0

let exit_failure = 1

let () =
  Arg.parse spec anonymous_argument usage;
  match !filename with
  | None ->
      Format.eprintf "%s" usage;
      exit exit_failure
  | Some filename -> (
      let fmt = if !mime then `MIME else `Usual in
      match run ~fmt filename with
      | Ok () -> exit exit_success
      | Error err -> Format.eprintf "%s: %a.\n%!" Sys.argv.(0) pp_error err)
