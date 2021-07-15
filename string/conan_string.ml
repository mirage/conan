open Conan
open Sigs

module Make (S : sig
  type +'a t
end) =
struct
  type t

  external prj : ('a, t) io -> 'a S.t = "%identity"

  external inj : 'a S.t -> ('a, t) io = "%identity"
end

module Caml_scheduler = Make (struct
  type +'a t = 'a
end)

let caml =
  let open Caml_scheduler in
  { bind = (fun x f -> f (prj x)); return = (fun x -> inj x) }

external get_uint16 : string -> int -> int = "%caml_string_get16"

external get_uint32 : string -> int -> int32 = "%caml_string_get32"

external get_uint64 : string -> int -> int64 = "%caml_string_get64"

module Str = struct
  type t = { mutable seek : int; contents : string; tmp : bytes }

  let openfile str = { seek = 0; contents = str; tmp = Bytes.create 80 }

  let _max_int = Int64.of_int max_int

  let _min_int = Int64.of_int min_int

  let seek t offset seek =
    if offset > _max_int || offset < _min_int
    then Error `Out_of_bound
    else
      let offset = Int64.to_int offset in
      match seek with
      | Sigs.SET ->
          if offset < String.length t.contents
          then (
            t.seek <- offset ;
            Ok ())
          else Error `Out_of_bound
      | Sigs.CUR ->
          if t.seek + offset < String.length t.contents
          then (
            t.seek <- t.seek + offset ;
            Ok ())
          else Error `Out_of_bound
      | Sigs.END ->
          if String.length t.contents + offset > 0
          then (
            t.seek <- String.length t.contents + offset ;
            Ok ())
          else Error `Out_of_bound

  let read t required =
    let len = min required (String.length t.contents - t.seek) in
    if len = 0 then None else Some (String.sub t.contents t.seek len)

  let read_int8 t =
    match read t 1 with
    | Some str -> Ok (Char.code str.[0])
    | _ -> Error `Out_of_bound

  let read_int16_ne t =
    match read t 2 with
    | Some str when String.length str >= 2 -> Ok (get_uint16 str 0)
    | _ -> Error `Out_of_bound

  let read_int32_ne t =
    match read t 4 with
    | Some str when String.length str >= 4 -> Ok (get_uint32 str 0)
    | _ -> Error `Out_of_bound

  let read_int64_ne t =
    match read t 8 with
    | Some str when String.length str >= 8 -> Ok (get_uint64 str 0)
    | _ -> Error `Out_of_bound

  let rec index str chr pos limit =
    if pos >= limit then raise Not_found ;
    if str.[pos] = chr then pos else index str chr (succ pos) limit

  let index str chr ~off ~len = index str chr off (off + len) - off

  let line t =
    try
      let len = min (String.length t.contents - t.seek) 80 in
      let off = t.seek in
      let pos = index t.contents '\n' ~off ~len in
      t.seek <- t.seek + (pos - off) ;
      Ok (off, pos, t.contents)
    with _ -> Error `Out_of_bound

  let read t required =
    match read t required with
    | Some str when String.length str >= required -> Ok str
    | _ -> Error `Out_of_bound

  let syscall =
    let open Caml_scheduler in
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

let run ~database:tree contents =
  let db = Hashtbl.create 0x10 in
  Process.fill_db db tree ;
  let result =
    let fd = Str.openfile contents in
    let rs =
      Caml_scheduler.prj (Process.descending_walk ~db caml Str.syscall fd tree)
    in
    rs in
  Ok result
