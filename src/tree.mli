type 'a fmt

type t = private Node of (elt * t) list | Done

and elt

val serialize : Format.formatter -> t -> unit
val serialize_elt : Format.formatter -> elt -> unit

val pp : Format.formatter -> t -> unit

val depth : t -> int
val weight : t -> int

type operation = private
  | Rule : Offset.t * ('test, 'v) Ty.t * 'test Test.t * 'v fmt -> operation
  | Name : Offset.t * string -> operation
  | Use : { offset : Offset.t; invert : bool; name : string } -> operation
  | MIME : string -> operation

val pp_operation : Format.formatter -> operation -> unit

val operation : elt -> operation

val fmt : 'a fmt -> unit -> ('a -> 'r, 'r) Fmt.fmt

val empty : t

val append : t -> ?filename:string -> ?line:int -> Parse.line -> t

val merge : t -> t -> t

(** / *)

module Unsafe : sig
  val rule :
    offset:Offset.t ->
    ('test, 'v) Ty.t ->
    'test Test.t ->
    Parse.message ->
    operation

  val name : offset:Offset.t -> string -> operation

  val use : offset:Offset.t -> invert:bool -> string -> operation

  val mime : string -> operation

  val elt : ?filename:string -> ?line:int -> operation -> elt

  val node : (elt * t) list -> t

  val leaf : t
end
