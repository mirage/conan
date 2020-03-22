type t =
  | Relative of t
  | Absolute of t
  | Value of int64
  | Read of t * Size.t
  | Calculation of t * t Arithmetic.t

val pp : Format.formatter -> t -> unit

open Sigs

val process
  :  's scheduler
  -> ('fd, 'error, 's) syscall
  -> 'fd -> t -> int64
  -> ((int64, 'error) result, 's) io
