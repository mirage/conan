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

external get_uint16 : bytes -> int -> int = "%caml_bytes_get16"

external get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

external get_uint64 : bytes -> int -> int64 = "%caml_bytes_get64"

let read_int16 fd =
  let ic = Unix.in_channel_of_descr fd in
  let rs = Bytes.create 2 in
  try
    really_input ic rs 0 2 ;
    Unix_scheduler.inj (Ok (get_uint16 rs 0))
  with uerror -> Unix_scheduler.inj (Error uerror)

let read_int32 fd =
  let ic = Unix.in_channel_of_descr fd in
  let rs = Bytes.create 4 in
  try
    really_input ic rs 0 4 ;
    Unix_scheduler.inj (Ok (get_uint32 rs 0))
  with uerror -> Unix_scheduler.inj (Error uerror)

let read_int64 fd =
  let ic = Unix.in_channel_of_descr fd in
  let rs = Bytes.create 8 in
  try
    really_input ic rs 0 8 ;
    Unix_scheduler.inj (Ok (get_uint64 rs 0))
  with uerror -> Unix_scheduler.inj (Error uerror)

let syscall =
  {
    seek;
    read;
    read_int8;
    read_int16_ne = read_int16;
    read_int32_ne = read_int32;
    read_int64_ne = read_int64;
    line;
  }

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
    | file :: rest -> (
        let ic = open_in ("examples" / file) in
        let rs = parse ic in
        close_in ic ;
        match rs with
        | Ok lines -> go (List.fold_left Tree.append tree lines) rest
        | _ -> go tree rest) in
  go Tree.Done files

let run ?(fmt = `Usual) filename =
  let tree = fill_tree () in
  let db = Hashtbl.create 0x10 in
  Process.fill_db db tree ;
  Format.printf "[+] database filled.\n%!" ;
  let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
  let result =
    Unix_scheduler.prj (Process.descending_walk ~db unix syscall fd tree) in
  match fmt with
  | `Usual ->
      Format.printf "%s:%s\n%!" filename (Metadata.output result) ;
      Ok ()
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
