type t = private
  | Byte
  | Leshort
  | Beshort
  | Lelong
  | Belong
  | Melong
  | Leid3
  | Beid3
  | Lequad
  | Bequad

val serialize : Format.formatter -> t -> unit
val byte : t
val leshort : t
val beshort : t
val short : t
val lelong : t
val belong : t
val melong : t
val long : t
val leid3 : t
val beid3 : t
val id3 : t
val lequad : t
val bequad : t
val quad : t
val of_string : string -> t
val is_size : char -> bool
val pp : Format.formatter -> t -> unit

open Sigs

val invert :
  's scheduler -> ('fd, 'error, 's) syscall -> ('fd, 'error, 's) syscall

val read :
  's scheduler ->
  ('fd, 'error, 's) syscall ->
  'fd ->
  t ->
  ((int64, 'error) result, 's) io
