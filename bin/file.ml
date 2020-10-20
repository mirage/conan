open Conan
open Sigs

module Make (S : sig
  type +'a t
end) =
struct
  type t

  type +'a s = 'a S.t

  external prj : ('a, t) io -> 'a S.t = "%identity"

  external inj : 'a S.t -> ('a, t) io = "%identity"
end

module Unix_scheduler = Make (struct
  type +'a t = 'a
end)

let unix =
  let open Unix_scheduler in
  { bind = (fun x f -> f (prj x)); return = (fun x -> inj x) }

external get_uint16 : string -> int -> int = "%caml_string_get16"

external get_uint32 : string -> int -> int32 = "%caml_string_get32"

external get_uint64 : string -> int -> int64 = "%caml_string_get64"

module MFile = struct
  type t = {
    table : (int64 * int * string) Weak.t;
    fd : Unix.file_descr;
    mutable cur : int;
    mutable seek : int64;
    max : int64;
  }

  let openfile filename =
    let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
    let { Unix.LargeFile.st_size = max; _ } = Unix.LargeFile.fstat fd in
    let table = Weak.create (0xff + 1) in
    { table; seek = 0L; fd; cur = 0; max }

  let close t = Unix.close t.fd

  let seek t offset = function
    | Sigs.SET ->
        if offset < t.max
        then (
          t.seek <- offset ;
          Ok ())
        else Error `Out_of_bound
    | Sigs.CUR ->
        if Int64.add t.seek offset < t.max
        then (
          t.seek <- Int64.add t.seek offset ;
          Ok ())
        else Error `Out_of_bound
    | Sigs.END ->
        if Int64.add t.max offset > 0L
        then (
          t.seek <- Int64.sub t.max offset ;
          Ok ())
        else Error `Out_of_bound

  let load ~seek t =
    let off = Int64.(mul (div seek 4096L) 4096L) in
    let off = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
    let len = min (Int64.sub t.max off) 4096L in
    let len = Int64.to_int len in
    if off < 0L || len = 0
    then None
    else
      try
        let buf = Bytes.create len in
        let _ = Unix.read t.fd buf 0 len in
        let cell = (off, len, Bytes.unsafe_to_string buf) in
        Weak.set t.table (t.cur land 0xff) (Some cell) ;
        t.cur <- t.cur + 1 ;
        Some cell
      with _ -> None

  exception Found of (int64 * int * string)

  let find ~seek t =
    if seek >= t.max || seek < 0L
    then None
    else
      try
        for i = 0 to Weak.length t.table - 1 do
          match Weak.get t.table i with
          | Some (off, len, payload) ->
              if seek >= off && (len - Int64.(to_int (sub seek off))) > 0
              then raise_notrace (Found (off, len, payload))
          | _ -> ()
        done ;
        load ~seek t
      with Found (off, len, payload) -> Some (off, len, payload)

  let read t required =
    let rec go acc ~seek required =
      match find ~seek t with
      | Some (off, len, payload) ->
          let sub_off = Int64.to_int (Int64.sub seek off) in
          let sub_len = len - sub_off in

          if sub_len >= required
          then List.rev ((sub_off, sub_len, payload) :: acc)
          else
            go
              ((sub_off, sub_len, payload) :: acc)
              ~seek:Int64.(add seek (of_int sub_len))
              (required - sub_len)
      | None -> List.rev acc in
    let ps = go [] ~seek:t.seek required in
    let buf = Buffer.create 0x100 in
    let rec concat = function
      | [] ->
          if Buffer.length buf > 0
          then Some (0, Buffer.length buf, Buffer.contents buf)
          else None
      | (off, len, payload) :: rest ->
          Buffer.add_substring buf payload off len ;
          concat rest in
    concat ps

  let read_int8 t =
    match read t 1 with
    | Some (off, _, payload) -> Ok (Char.code payload.[off])
    | None -> Error `Out_of_bound

  let read_int16_ne t =
    match read t 2 with
    | Some (off, _, payload) -> Ok (get_uint16 payload off)
    | None -> Error `Out_of_bound

  let read_int32_ne t =
    match read t 4 with
    | Some (off, _, payload) -> Ok (get_uint32 payload off)
    | None -> Error `Out_of_bound

  let read_int64_ne t =
    match read t 8 with
    | Some (off, _, payload) -> Ok (get_uint64 payload off)
    | None -> Error `Out_of_bound

  let read t required =
    match read t required with
    | Some (off, len, payload) ->
        if len >= required
        then Ok (String.sub payload off required)
        else Error `Out_of_bound
    | None -> Error `Out_of_bound

  let line t =
    let buf = Buffer.create 0x100 in
    let rec go () =
      let len = min Int64.(sub t.max t.seek) 4096L in
      let len = Int64.to_int len in
      if len = 0
      then Error `Out_of_bound
      else
        match read t len with
        | Error _ as err -> err
        | Ok payload ->
        try
          let pos = String.index payload '\n' in
          Buffer.add_substring buf payload pos (String.length payload - pos) ;
          t.seek <- Int64.(add t.seek (of_int pos)) ;
          Ok (Buffer.contents buf)
        with _ ->
          Buffer.add_string buf payload ;
          t.seek <- Int64.(add t.seek (of_int (String.length payload))) ;
          go () in
    go ()

  let syscall =
    let open Unix_scheduler in
    {
      seek = (fun f p w -> inj (seek f p w));
      read = (fun f l -> inj (read f l));
      read_int8 = (fun f -> inj (read_int8 f));
      read_int16_ne = (fun f -> inj (read_int16_ne f));
      read_int32_ne = (fun f -> inj (read_int32_ne f));
      read_int64_ne = (fun f -> inj (read_int64_ne f));
      line = (fun f -> inj (line f));
    }
end

module SFile = struct
  let openfile filename = Unix.openfile filename Unix.[ O_RDONLY ] 0o644

  let seek fd offset where =
    match where with
    | SET -> Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET
    | CUR -> Unix.lseek fd (Int64.to_int offset) Unix.SEEK_CUR
    | END -> Unix.lseek fd (Int64.to_int offset) Unix.SEEK_END

  let seek fd offset where =
    try
      let ret = seek fd offset where in
      assert (ret = Int64.to_int offset) ;
      Unix_scheduler.inj (Ok ())
    with uerror -> Unix_scheduler.inj (Error uerror)

  let read fd len =
    let rs = Bytes.create len in
    let ic = Unix.in_channel_of_descr fd in
    try
      really_input ic rs 0 len ;
      Unix_scheduler.inj (Ok (Bytes.unsafe_to_string rs))
    with uerror -> Unix_scheduler.inj (Error uerror)

  let line fd =
    let ic = Unix.in_channel_of_descr fd in
    match input_line ic with
    | line -> Unix_scheduler.inj (Ok line)
    | exception uerror -> Unix_scheduler.inj (Error uerror)

  let read_int8 fd =
    let ic = Unix.in_channel_of_descr fd in
    try
      let res = input_byte ic in
      Unix_scheduler.inj (Ok res)
    with uerror -> Unix_scheduler.inj (Error uerror)

  let read_int16_ne fd =
    let ic = Unix.in_channel_of_descr fd in
    let rs = Bytes.create 2 in
    try
      really_input ic rs 0 2 ;
      Unix_scheduler.inj (Ok (get_uint16 (Bytes.unsafe_to_string rs) 0))
    with uerror -> Unix_scheduler.inj (Error uerror)

  let read_int32_ne fd =
    let ic = Unix.in_channel_of_descr fd in
    let rs = Bytes.create 4 in
    try
      really_input ic rs 0 4 ;
      Unix_scheduler.inj (Ok (get_uint32 (Bytes.unsafe_to_string rs) 0))
    with uerror -> Unix_scheduler.inj (Error uerror)

  let read_int64_ne fd =
    let ic = Unix.in_channel_of_descr fd in
    let rs = Bytes.create 8 in
    try
      really_input ic rs 0 8 ;
      Unix_scheduler.inj (Ok (get_uint64 (Bytes.unsafe_to_string rs) 0))
    with uerror -> Unix_scheduler.inj (Error uerror)

  let syscall =
    { seek; read; read_int8; read_int16_ne; read_int32_ne; read_int64_ne; line }

  let close fd = Unix.close fd
end

let exit_success = 0

let exit_failure = 1

let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err

type fmt = [ `MIME | `Usual ]

let parse ic = Parse.parse_in_channel ic

let ( / ) = Filename.concat

let fill_tree () =
  let files = Sys.readdir "examples" in
  let files = Array.to_list files in
  let rec go tree = function
    | [] -> tree
    | filename :: rest -> (
        let ic = open_in ("examples" / filename) in
        let rs = parse ic in
        close_in ic ;
        match rs with
        | Ok lines ->
            let _, tree =
              List.fold_left
                (fun (line, tree) v ->
                  (succ line, Tree.append ~filename ~line tree v))
                (1, tree) lines in
            go tree rest
        | _ -> go tree rest) in
  go Tree.Done files

let run ?(memoize = false) ?(fmt = `Usual) filename =
  let tree = fill_tree () in
  let db = Hashtbl.create 0x10 in
  Process.fill_db db tree ;
  let result =
    if memoize
    then (
      let fd = MFile.openfile filename in
      let rs =
        Unix_scheduler.prj
          (Process.descending_walk ~db unix MFile.syscall fd tree) in
      MFile.close fd ;
      rs)
    else
      let fd = SFile.openfile filename in
      let rs =
        Unix_scheduler.prj
          (Process.descending_walk ~db unix SFile.syscall fd tree) in
      SFile.close fd ;
      rs in
  match fmt with
  | `Usual -> (
      match Metadata.output result with
      | Some output ->
          Format.printf "%s: %s\n%!" filename output ;
          Ok ()
      | None -> Error `Not_found)
  | `MIME ->
  match Metadata.mime result with
  | None -> Error `Not_found
  | Some mime ->
      Format.printf "%s\n%!" mime ;
      Ok ()

let pp_error ppf = function
  | #Parse.error as err -> Parse.pp_error ppf err
  | `Not_found -> Format.fprintf ppf "Not found"

let mime = ref false

let memoize = ref false

let filename = ref None

let anonymous_argument v =
  match !filename with None -> filename := Some v | Some _ -> ()

let usage = Format.asprintf "%s [--mime] [--memoize] filename\n%!" Sys.argv.(0)

let spec =
  [
    ( "--mime",
      Arg.Set mime,
      "Causes the file command to output mime type strings rather than the \
       more traditional human readable ones. Thus it may say 'text/plain; \
       charset=ascii' rather than 'ASCII text'" );
    ("--memoize", Arg.Set memoize, "Memoize access to the given file.");
  ]

let () =
  Arg.parse spec anonymous_argument usage ;
  match !filename with
  | None ->
      Format.eprintf "%s" usage ;
      exit exit_failure
  | Some filename -> (
      let fmt = if !mime then `MIME else `Usual in
      match run ~memoize:!memoize ~fmt filename with
      | Ok () -> exit exit_success
      | Error err -> Format.eprintf "%s: %a.\n%!" Sys.argv.(0) pp_error err)
