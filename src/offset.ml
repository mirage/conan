let ( <.> ) f g x = f (g x)
let ok x = Ok x

type t =
  | Relative of t
  | Absolute of t
  | Value of int64
  | Read of t * Size.t
  | Calculation of t * t Arithmetic.t

let rec serialize ppf = function
  | Relative v ->
      Format.fprintf ppf "Conan.Offset.Relative@ @[%a@]"
        (Serialize.parens serialize)
        v
  | Absolute v ->
      Format.fprintf ppf "Conan.Offset.Absolute@ @[%a@]"
        (Serialize.parens serialize)
        v
  | Value v -> Format.fprintf ppf "Conan.Offset.Value@ %a" Serialize.int64 v
  | Read (v, s) ->
      Format.fprintf ppf "Conan.Offset.Read@ @[%a@]"
        (Serialize.pair (Serialize.parens serialize) Size.serialize)
        (v, s)
  | Calculation (v, arithmetic) ->
      Format.fprintf ppf "Conan.Offset.Calculation@ @[%a@]"
        (Serialize.pair
           (Serialize.parens serialize)
           (Arithmetic.serialize (Serialize.parens serialize)))
        (v, arithmetic)

let pf = Format.fprintf

let rec pp ppf = function
  | Relative v -> pf ppf "&%a" pp v
  | Absolute v -> pf ppf "%a" pp v
  | Value v -> pf ppf "%Ld" v
  | Read (Calculation (v, c), s) ->
      pf ppf "(%a.%a[%a])" pp v Size.pp s (Arithmetic.pp pp) c
  | Read (v, s) -> pf ppf "(%a.%a)" pp v Size.pp s
  | Calculation (v, c) -> pf ppf "%a[%a]" pp v (Arithmetic.pp pp) c

open Sigs

let process : type s fd error.
    s scheduler ->
    (fd, error, s) syscall ->
    fd ->
    t ->
    int64 ->
    ((int64, error) result, s) io =
 fun ({ bind; return } as scheduler) syscall fd offset abs_offset ->
  let ( >>= ) = bind in
  let ( >?= ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)
  in
  (* XXX(dinosaure): we want to calculate at any steps the absolute offset
     instead to trust on [CUR] and we try to use only [SET] with [seek].
     However, in some case (eg. [offset use name]), we must lie about the
     /absolute offset/. *)
  let rec go_offset = function
    | Read (Calculation (v, c), s) ->
        go_offset v >?= fun abs_offset ->
        syscall.seek fd abs_offset SET >?= fun () ->
        Size.read scheduler syscall fd s >?= fun abs_offset ->
        go_calculation abs_offset c
    | Read (v, s) ->
        go_offset v >?= fun abs_offset ->
        syscall.seek fd abs_offset SET >?= fun () ->
        Size.read scheduler syscall fd s
    | Relative v ->
        go_offset v >?= fun rel_offset ->
        (return <.> ok) (Int64.add abs_offset rel_offset)
    | Absolute v -> go_offset v
    | Value v -> (return <.> ok) v
    | Calculation (v, c) ->
        go_offset v >?= fun abs_offset -> go_calculation abs_offset c
  and go_calculation abs_offset = function
    | Arithmetic.Invert c ->
        go_calculation abs_offset c >?= fun abs_offset ->
        (return <.> ok) (Int64.lognot abs_offset)
    | Arithmetic.Add v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.add abs_offset v)
    | Arithmetic.Sub v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.sub abs_offset v)
    | Arithmetic.Mul v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.mul abs_offset v)
    | Arithmetic.Div v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.div abs_offset v)
    | Arithmetic.Mod v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.rem abs_offset v)
    | Arithmetic.Bitwise_and v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.logand abs_offset v)
    | Arithmetic.Bitwise_xor v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.logxor abs_offset v)
    | Arithmetic.Bitwise_or v ->
        go_offset v >?= fun v -> (return <.> ok) (Int64.logor abs_offset v)
  in
  go_offset offset
