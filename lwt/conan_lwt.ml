open Conan.Sigs

module Make (S : sig
  type +'a t
end) : sig
  type t

  val prj : ('a, t) io -> 'a S.t

  val inj : 'a S.t -> ('a, t) io
end = struct
  type t

  external prj : ('a, t) io -> 'a S.t = "%identity"

  external inj : 'a S.t -> ('a, t) io = "%identity"
end

module Lwt_scheduler = Make (struct
  type +'a t = 'a Lwt.t
end)

let ( <.> ) f g x = f (g x)

let lwt =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  let bind x f = inj (prj x >>= (prj <.> f)) in
  { bind; return = (fun x -> inj (Lwt.return x)) }

external get_uint16 : string -> int -> int = "%caml_string_get16"

external get_uint32 : string -> int -> int32 = "%caml_string_get32"

external get_uint64 : string -> int -> int64 = "%caml_string_get64"

module Stream = struct
  type t = {
    mutable buffer : Bigstringaf.t;
    mutable save : int;
    mutable seek : int;
    stream : unit -> string option Lwt.t;
  }

  let openfile stream =
    { buffer = Bigstringaf.create 0x1000; save = 0; seek = 0; stream }

  let resize t more =
    let new_len = ref t.save in
    while t.save + more > !new_len do
      new_len := 2 * !new_len
    done ;
    (* TODO(dinosaure): overflow! *)
    let buffer = Bigstringaf.create !new_len in
    Bigstringaf.blit t.buffer ~src_off:0 buffer ~dst_off:0 ~len:t.save ;
    t.buffer <- buffer

  let _max_int = Int64.of_int max_int

  open Lwt.Infix

  let ( >>? ) = Lwt_result.bind

  let rec consume_and_save_to ~abs_offset t =
    if abs_offset < t.save
    then Lwt.return_ok ()
    else
      t.stream () >>= function
      | None -> Lwt.return_error `Out_of_bound
      | Some str ->
          let max = Bigstringaf.length t.buffer - t.save in
          if String.length str > max then resize t (String.length str - max) ;
          Bigstringaf.blit_from_string str ~src_off:0 t.buffer ~dst_off:t.save
            ~len:(String.length str) ;
          t.save <- t.save + String.length str ;
          consume_and_save_to ~abs_offset t

  let rec save_all t =
    t.stream () >>= function
    | None -> Lwt.return_unit
    | Some str ->
        let max = Bigstringaf.length t.buffer - t.save in
        if String.length str > max then resize t (String.length str - max) ;
        Bigstringaf.blit_from_string str ~src_off:0 t.buffer ~dst_off:t.save
          ~len:(String.length str) ;
        t.save <- t.save + String.length str ;
        save_all t

  let seek t offset seek =
    if offset > _max_int || offset < 0L
    then Lwt.return_error `Out_of_bound
    else
      let offset = Int64.to_int offset in
      match seek with
      | Conan.Sigs.SET ->
          consume_and_save_to ~abs_offset:offset t >>? fun () ->
          t.seek <- offset ;
          Lwt.return_ok ()
      | Conan.Sigs.CUR ->
          let abs_offset = t.seek + offset in
          consume_and_save_to ~abs_offset t >>? fun () ->
          t.seek <- t.seek + offset ;
          Lwt.return_ok ()
      | Conan.Sigs.END ->
          save_all t >>= fun () ->
          let abs_offset = t.save + offset in
          if abs_offset >= 0 && abs_offset < t.save
          then (
            t.seek <- abs_offset ;
            Lwt.return_ok ())
          else Lwt.return_error `Out_of_bound

  let read t required =
    consume_and_save_to ~abs_offset:(t.seek + required) t >>= fun _ ->
    let len = min required (t.save - t.seek) in
    if len <= 0
    then Lwt.return_none
    else Lwt.return_some (Bigstringaf.substring t.buffer ~off:t.seek ~len)

  let read_int8 t =
    read t 1 >>= function
    | Some str -> Lwt.return_ok (Char.code str.[0])
    | None -> Lwt.return_error `Out_of_bound

  let read_int16_ne t =
    read t 2 >>= function
    | Some str when String.length str >= 2 -> Lwt.return_ok (get_uint16 str 0)
    | _ -> Lwt.return_error `Out_of_bound

  let read_int32_ne t =
    read t 4 >>= function
    | Some str when String.length str >= 4 -> Lwt.return_ok (get_uint32 str 0)
    | _ -> Lwt.return_error `Out_of_bound

  let read_int64_ne t =
    read t 8 >>= function
    | Some str when String.length str >= 8 -> Lwt.return_ok (get_uint64 str 0)
    | _ -> Lwt.return_error `Out_of_bound

  let rec index buf chr pos limit =
    if pos >= limit then raise Not_found ;
    if Bigstringaf.get buf pos = chr
    then pos
    else index buf chr (succ pos) limit

  let index str chr ~off ~len = index str chr off (off + len) - off

  let line t =
    read t 80 >>= fun _ ->
    let len = min (t.save - t.seek) 80 in
    let off = t.seek in
    match index t.buffer '\n' ~off ~len with
    | pos ->
        t.seek <- t.seek + (pos - off) ;
        let str = Bigstringaf.substring t.buffer ~off ~len:(off - pos) in
        Lwt.return_ok (0, off - pos, str)
    | exception Not_found -> Lwt.return_error `Out_of_bound

  let read t required =
    read t required >>= function
    | Some str when String.length str >= required -> Lwt.return_ok str
    | _ -> Lwt.return_error `Out_of_bound

  let syscall =
    let open Lwt_scheduler in
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

let msgf fmt = Format.kasprintf (fun msg -> `Msg msg) fmt

open Conan

let run ~database stream =
  let open Lwt.Infix in
  let fd = Stream.openfile stream in
  Lwt.catch (fun () ->
      Lwt_scheduler.prj (Process.descending_walk lwt Stream.syscall fd database)
      >>= fun x -> Lwt.return_ok x)
  @@ fun exn ->
  Lwt.return_error (msgf "Internal error: %s" (Printexc.to_string exn))
