open Conan_unix

let previous = ref (Mtime.of_uint64_ns 0L)

let pp_span ppf v =
  let span = Mtime.Span.to_uint64_ns v in
  Format.fprintf ppf "%12Ld" span

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      let now = Mtime_clock.now () in
      let diff = Mtime.span !previous now in
      previous := now ;
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue pp_span)
        diff Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()

let () = Logs.set_reporter (reporter Fmt.stderr)

let () = Logs.set_level ~all:true (Some Logs.Debug)

let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err

let run ?(fmt = `Usual) filename =
  run ~database:"examples" filename >>= fun result ->
  match fmt with
  | `Usual -> (
      match Conan.Metadata.output result with
      | Some output ->
          Format.printf "%s: %s\n%!" filename output ;
          Ok ()
      | None -> Error `Not_found)
  | `MIME ->
  match Conan.Metadata.mime result with
  | None -> Error `Not_found
  | Some mime ->
      Format.printf "%s\n%!" mime ;
      Ok ()

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
  Arg.parse spec anonymous_argument usage ;
  match !filename with
  | None ->
      Format.eprintf "%s" usage ;
      exit exit_failure
  | Some filename -> (
      let fmt = if !mime then `MIME else `Usual in
      match run ~fmt filename with
      | Ok () -> exit exit_success
      | Error err -> Format.eprintf "%s: %a.\n%!" Sys.argv.(0) pp_error err)
