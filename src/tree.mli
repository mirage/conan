type 'a fmt

type t = private Node of (elt * t) list | Done

and elt

val pp : Format.formatter -> t -> unit

type operation = private
  | Rule : Offset.t * ('test, 'v) Ty.t * 'test Test.t * 'v fmt -> operation
  | Name : Offset.t * string -> operation
  | Use : { offset : Offset.t; invert : bool; name : string } -> operation
  | MIME : string -> operation

val operation : elt -> operation

val fmt : 'a fmt -> unit -> ('a -> 'r, 'r) Fmt.fmt

val empty : t

val append : t -> ?filename:string -> ?line:int -> Parse.line -> t
