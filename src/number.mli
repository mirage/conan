type t

val int64 : int64 -> t
val float : float -> t

val pp : Format.formatter -> t -> unit

val to_float : t -> float
val to_int : t -> int
val to_int64 : t -> int64
val to_int32 : t -> int32
val to_short : t -> int
val to_byte : t -> char

val parse : Sub.t -> (t * Sub.t, [> `Empty
                                 |  `Invalid_number of string ]) result
