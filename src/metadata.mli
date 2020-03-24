type t

val pp : Format.formatter -> t -> unit

val with_mime : string -> t -> t
val with_output : string -> t -> t

val output : t -> string
val mime : t -> string option
val empty : t
