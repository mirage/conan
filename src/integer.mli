type 'a t = private
  | Byte : char t
  | Short : int t
  | Int32 : int32 t
  | Int64 : int64 t

val serialize : Format.formatter -> 'a t -> unit

val serializer_of : 'a t -> Format.formatter -> 'a -> unit

val byte : char t

val short : int t

val int32 : int32 t

val int64 : int64 t

val pp : 'a t -> Format.formatter -> 'a -> unit

val add : 'a t -> 'a -> 'a -> 'a

val sub : 'a t -> 'a -> 'a -> 'a

val mul : 'a t -> 'a -> 'a -> 'a

val div : ?unsigned:bool -> 'a t -> 'a -> 'a -> 'a

val rem : ?unsigned:bool -> 'a t -> 'a -> 'a -> 'a

val bitwise_and : 'a t -> 'a -> 'a -> 'a

val bitwise_xor : 'a t -> 'a -> 'a -> 'a

val bitwise_or : 'a t -> 'a -> 'a -> 'a

val invert : 'a t -> 'a -> 'a

val greater : 'a t -> 'a -> 'a -> bool

val lower : 'a t -> 'a -> 'a -> bool

val equal : 'a t -> 'a -> 'a -> bool

val different : 'a t -> 'a -> 'a -> bool

val zero : 'a t -> 'a

val parse :
  Sub.t -> (int64 * Sub.t, [> `Empty | `Invalid_integer of string ]) result
