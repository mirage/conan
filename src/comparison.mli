type 'a t

val serialize :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val of_string : with_val:'a -> string -> 'a t

val is : char -> bool

val map : f:('a -> 'b) -> 'a t -> 'b t

val value : 'a t -> 'a

val equal_to : 'a -> 'a t

val different_to : 'a -> 'a t

val greater_than : 'a -> 'a t

val lower_than : 'a -> 'a t

val bitwise_and : 'a -> 'a t

val bitwise_xor : 'a -> 'a t

val process : 'a Integer.t -> 'a -> 'a t -> bool

val process_float : float -> float t -> bool

val process_string : string -> string t -> bool

val process_ptime : Ptime.Span.t -> Ptime.Span.t t -> bool
