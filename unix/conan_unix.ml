open Conan
open Sigs

let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

module Make (S : sig
  type +'a t
end) =
struct
  type t

  external prj : ('a, t) io -> 'a S.t = "%identity"
  external inj : 'a S.t -> ('a, t) io = "%identity"
end

module Unix_scheduler = Make (struct
  type +'a t = 'a
end)

let unix =
  let open Unix_scheduler in
  { bind = (fun x f -> f (prj x)); return = (fun x -> inj x) }

module File = struct
  type t = {
    cache : Unix.file_descr Cachet.t;
    fd : Unix.file_descr;
    mutable seek : int64;
    max : int64;
    tmp : bytes;
  }

  let openfile filename =
    let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
    let { Unix.LargeFile.st_size = max; _ } = Unix.LargeFile.fstat fd in
    let map fd ~pos len =
      let pos = Int64.of_int pos in
      let max = Int64.sub max pos in
      let max = Int64.to_int max in
      let len = Int.min len max in
      let barr =
        Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
      in
      Bigarray.array1_of_genarray barr
    in
    let cache = Cachet.make ~map fd in
    { cache; fd; seek = 0L; max; tmp = Bytes.create 80 }

  let close t = Unix.close t.fd

  let seek t offset = function
    | Sigs.SET ->
        if offset < t.max then begin
          t.seek <- offset;
          Ok ()
        end
        else Error `Out_of_bound
    | Sigs.CUR ->
        if Int64.add t.seek offset < t.max then begin
          t.seek <- Int64.add t.seek offset;
          Ok ()
        end
        else Error `Out_of_bound
    | Sigs.END ->
        if Int64.sub t.max offset >= 0L then begin
          t.seek <- Int64.sub t.max offset;
          Ok ()
        end
        else Error `Out_of_bound

  let read_int8 t =
    try
      let pos = Int64.to_int t.seek in
      Ok (Cachet.get_uint8 t.cache pos)
    with _ -> Error `Out_of_bound

  let read_int16_ne t =
    try
      let pos = Int64.to_int t.seek in
      Ok (Cachet.get_uint16_ne t.cache pos)
    with _ -> Error `Out_of_bound

  let read_int32_ne t =
    try
      let pos = Int64.to_int t.seek in
      Ok (Cachet.get_int32_ne t.cache pos)
    with _ -> Error `Out_of_bound

  let read_int64_ne t =
    try
      let pos = Int64.to_int t.seek in
      Ok (Cachet.get_int64_ne t.cache pos)
    with _ -> Error `Out_of_bound

  let line t =
    let max = Int64.sub t.max t.seek in
    let len = Int64.min 80L max in
    let len = Int64.to_int len in
    let buf = Bytes.create len in
    let src_off = Int64.to_int t.seek in
    Cachet.blit_to_bytes t.cache ~src_off buf ~dst_off:0 ~len;
    let str = Bytes.unsafe_to_string buf in
    match String.index str '\n' with
    | len -> Ok (0, len, String.sub str 0 len)
    | exception _exn -> Error `Out_of_bound

  let read t len =
    try
      let buf = Bytes.create len in
      let src_off = Int64.to_int t.seek in
      Cachet.blit_to_bytes t.cache ~src_off buf ~dst_off:0 ~len;
      Ok (Bytes.unsafe_to_string buf)
    with _exn -> Error `Out_of_bound

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

let ( / ) = Filename.concat

let fill_tree database =
  let files = Sys.readdir database in
  let files = Array.to_list files in
  let files = List.sort String.compare files in
  let rec go tree = function
    | [] -> tree
    | filename :: rest -> (
        let ic = open_in (database / filename) in
        let rs = Parse.parse_in_channel ic in
        close_in ic;
        match rs with
        | Ok lines ->
            let _, tree =
              List.fold_left
                (fun (line, tree) v ->
                  (succ line, Tree.append ~filename ~line tree v))
                (1, tree) lines
            in
            go tree rest
        | _ -> go tree rest)
  in
  go Tree.empty files

let tree ~directory = fill_tree directory

let tree_of_string str =
  let lines = String.split_on_char '\n' str in
  let lines =
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | line :: r -> (
          match Parse.parse_line line with
          | Ok v -> go (v :: acc) r
          | Error _ as err -> err)
    in
    go [] lines
  in
  match lines with
  | Ok lines ->
      let _, tree =
        List.fold_left
          (fun (line, tree) v -> (succ line, Tree.append ~line tree v))
          (1, Tree.empty) lines
      in
      Ok tree
  | Error err -> Error (`Msg (Format.asprintf "%a" Parse.pp_error err))

let run_with_tree tree filename =
  let database = Process.database ~tree in
  let result =
    let fd = File.openfile filename in
    let rs =
      Unix_scheduler.prj (Process.descending_walk unix File.syscall fd database)
    in
    File.close fd;
    rs
  in
  Ok result

let run ~database filename =
  if Sys.file_exists filename then
    let tree = fill_tree database in
    let database = Process.database ~tree in
    let result =
      let file = File.openfile filename in
      let finally () = Unix.close file.File.fd in
      Fun.protect ~finally @@ fun () ->
      Unix_scheduler.prj
        (Process.descending_walk unix File.syscall file database)
    in
    Ok result
  else if Sys.file_exists filename then error_msgf "%s does not exist" database
  else error_msgf "%s does not exist" filename
