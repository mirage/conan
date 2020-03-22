type 'a t =
  | Invert of 'a t
  | Add of 'a
  | Sub of 'a
  | Mul of 'a
  | Div of 'a
  | Mod of 'a
  | Bitwise_and of 'a
  | Bitwise_xor of 'a

val pp
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter -> 'a t -> unit

val map : f:('a -> 'b) -> 'a t -> 'b t
val value : 'a t -> 'a

val of_string : with_val:'a -> string -> 'a t
val is : char -> bool

val add : 'a -> 'a t
val sub : 'a -> 'a t
val div : 'a -> 'a t
val rem : 'a -> 'a t
val mul : 'a -> 'a t
val logand : 'a -> 'a t
val logxor : 'a -> 'a t
val invert : 'a t -> 'a t

val process : ?unsigned:bool -> 'a Integer.t -> 'a -> 'a t -> 'a
val process_float : float -> float t -> float
