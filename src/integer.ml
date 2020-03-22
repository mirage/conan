type 'a t =
  | Byte : char t
  | Short : int t
  | Int32 : int32 t
  | Int64 : int64 t

let byte = Byte
let short = Short
let int32 = Int32
let int64 = Int64

let pf = Format.fprintf

let pp : type a. a t -> Format.formatter -> a -> unit = function
  | Byte -> fun ppf v -> pf ppf "%02x" (Char.code v)
  | Short -> fun ppf v -> pf ppf "0x%x" v
  | Int32 -> fun ppf v -> pf ppf "0x%lx" v
  | Int64 -> fun ppf v -> pf ppf "0x%Lx" v

let add : type w. w t -> w -> w -> w = function
  | Byte -> fun a b -> Char.(chr (((code a) + (code b)) land 0xff))
  | Short -> fun a b -> (a + b) land 0xffff
  | Int32 -> Int32.add
  | Int64 -> Int64.add

let sub : type w. w t -> w -> w -> w = function
  | Byte -> fun a b -> Char.(chr (((code a) + (code b)) land 0xff))
  | Short -> fun a b -> (a - b) land 0xffff
  | Int32 -> Int32.sub
  | Int64 -> Int64.sub

let mul : type w. w t -> w -> w -> w = function
  | Byte -> fun a b -> Char.(chr (((code a) * (code b)) land 0xff))
  | Short -> fun a b -> (a * b) land 0xffff
  | Int32 -> Int32.mul
  | Int64 -> Int64.mul

let div : type w. ?unsigned:bool -> w t -> w -> w -> w = fun ?(unsigned= false) -> function
  | Byte -> fun a b -> Char.(chr (((code a) / (code b)) land 0xff))
  | Short -> fun a b -> (a / b) land 0xffff
  | Int32 -> if unsigned then Int32.unsigned_div else Int32.div
  | Int64 -> if unsigned then Int64.unsigned_div else Int64.div

let rem : type w. ?unsigned:bool -> w t -> w -> w -> w = fun ?(unsigned= false) -> function
  | Byte -> fun a b -> Char.(chr (((code a) mod (code b)) land 0xff))
  | Short -> fun a b -> (a mod b) land 0xffff
  | Int32 -> if unsigned then Int32.unsigned_rem else Int32.rem
  | Int64 -> if unsigned then Int64.unsigned_rem else Int64.rem

let bitwise_and : type w. w t -> w -> w -> w = function
  | Byte -> fun a b -> Char.(chr (((code a) land (code b)) land 0xff))
  | Short -> fun a b -> (a land b) land 0xffff
  | Int32 -> Int32.logand
  | Int64 -> Int64.logand

let bitwise_xor : type w. w t -> w -> w -> w = function
  | Byte -> fun a b -> Char.(chr (((code a) lxor (code b)) land 0xff))
  | Short -> fun a b -> (a lxor b) land 0xffff
  | Int32 -> Int32.logxor
  | Int64 -> Int64.logxor

let invert : type w. w t -> w -> w = function
  | Byte -> fun v -> Char.(chr ((lnot (code v)) land 0xff))
  | Short -> fun v -> (lnot v) land 0xffff
  | Int32 -> Int32.lognot
  | Int64 -> Int64.lognot

let greater : type w. w t -> w -> w -> bool = function
  | Byte -> (>)
  | Short -> (>)
  | Int32 -> (>)
  | Int64 -> (>)

let lower : type w. w t -> w -> w -> bool = function
  | Byte -> (<)
  | Short -> (<)
  | Int32 -> (<)
  | Int64 -> (<)

let equal : type w. w t -> w -> w -> bool = function
  | Byte -> (=)
  | Short -> (=)
  | Int32 -> (=)
  | Int64 -> (=)

let different : type w. w t -> w -> w -> bool = function
  | Byte -> (<>)
  | Short -> (<>)
  | Int32 -> (<>)
  | Int64 -> (<>)

let zero : type w. w t -> w = function
  | Byte -> '\000'
  | Short -> 0
  | Int32 -> Int32.zero
  | Int64 -> Int64.zero

open Astring.String.Sub

let parse s =
  let v = to_string s in
  let lexbuf = Lexing.from_string v in
  if is_empty s
  then Error `Empty
  else match Lexer.int lexbuf with
    | Ok v ->
      let first = Lexing.lexeme_end lexbuf in
      Ok (Int64.of_string v, with_range ~first s)
    | Error `Malformed ->
      Error (`Invalid_integer v)
    | exception _ ->
      Error (`Invalid_integer v)
