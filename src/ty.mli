type default = Default
type clear = Clear
type unsigned = { unsigned : bool }
type endian = [ `BE | `LE | `ME | `NE ]

type ('test, 'v) t = private
  | Default : (default, default) t
  | Offset : (int64, int64) t
  | Regex : {
      case_insensitive : bool;
      start : bool;
      limit : int64;
      kind : [ `Byte | `Line ];
    }
      -> (Re.t, Ropes.t) t
  | Clear : (clear, clear) t
  | Search : {
      compact_whitespaces : bool;
      optional_blank : bool;
      lower_case_insensitive : bool;
      upper_case_insensitive : bool;
      text : bool;
      binary : bool;
      trim : bool;
      range : int64;
      pattern : string;
      find : Kmp.finder;
    }
      -> (string, string) t
  | Pascal_string : (string, string) t
  | Unicode_string : [ `BE | `LE ] -> (string, string) t
  | Byte : unsigned * char Arithmetic.t -> (char, char) t
  | Short : unsigned * int Arithmetic.t * [ `BE | `LE | `NE ] -> (int, int) t
  | Long : unsigned * int32 Arithmetic.t * endian -> (int32, int32) t
  | Quad : unsigned * int64 Arithmetic.t * endian -> (int64, int64) t
  | Float : unsigned * float Arithmetic.t * endian -> (float, float) t
  | Double : unsigned * float Arithmetic.t * endian -> (float, float) t
  | Indirect : [ `Rel | `Abs ] -> ('test, 'v) t
  | Date :
      [ `Local | `UTC | `Window ]
      * [ `s32 | `s64 ]
      * Ptime.span Arithmetic.t
      * endian
      -> (Ptime.t, string) t

val serialize : Format.formatter -> ('test, 'v) t -> unit
val pp : Format.formatter -> ('test, 'v) t -> unit
val pp_of_result : ('test, 'v) t -> Format.formatter -> 'v -> unit
val default : (default, default) t
val offset : (int64, int64) t
val clear : (clear, clear) t

val regex :
  ?case_insensitive:bool ->
  ?start:bool ->
  ?limit:int64 ->
  [ `Line | `Byte ] ->
  (Re.t, Ropes.t) t

val pascal_string : (string, string) t

val search :
  ?compact_whitespaces:bool ->
  ?optional_blank:bool ->
  ?lower_case_insensitive:bool ->
  ?upper_case_insensitive:bool ->
  [ `Text | `Binary ] ->
  ?trim:bool ->
  int64 ->
  pattern:string ->
  (string, string) t

val with_range : int64 -> (string, string) t -> (string, string) t
val with_pattern : string -> (string, string) t -> (string, string) t
val str_unicode : [ `BE | `LE ] -> (string, string) t

val numeric :
  ?unsigned:bool ->
  ?endian:endian ->
  'w Integer.t ->
  'w Arithmetic.t ->
  ('w, 'w) t

val date :
  [ `Date | `Ldate | `Qdate | `Qldate | `Qwdate ] ->
  [ `BE | `LE | `ME ] option ->
  Ptime.Span.t Arithmetic.t ->
  (Ptime.t, string) t

val float :
  ?unsigned:bool -> ?endian:endian -> float Arithmetic.t -> (float, float) t

val double :
  ?unsigned:bool -> ?endian:endian -> float Arithmetic.t -> (float, float) t

val indirect : [ `Rel | `Abs ] -> (_, _) t

open Sigs

val process :
  's scheduler ->
  ('fd, 'error, 's) syscall ->
  'fd ->
  int64 ->
  ('test, 'v) t ->
  (('v, [> `Syscall of 'error | `Invalid_date | `Not_found ]) result, 's) io
