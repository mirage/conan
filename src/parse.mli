type s = Sub.t

type numeric =
  [ `Byte
  | `Short
  | `Long
  | `Quad
  | `Date
  | `Ldate
  | `Qdate
  | `Qldate
  | `Qwdate
  | `Double
  | `Float ]

type rule = offset * kind * test * message

and offset =
  int
  * [ `Abs of int64
    | `Rel of int64
    | `Ind of
      [ `Abs | `Rel ]
      * ([ `Abs of int64 | `Rel of int64 ]
        * Size.t option
        * [ `Dir of int64 | `Ind of int64 ] Arithmetic.t option) ]

and kind =
  bool
  * [ `Numeric of
      [ `BE | `LE | `ME ] option * numeric * int64 Arithmetic.t option
    | `Default
    | `Clear
    | `Offset
    | `Indirect of bool
    | `Regex of (bool * bool * bool * int64) option
    | `String16 of [ `BE | `LE ]
    | `String8 of (bool * bool * bool * bool) option
    | `Search of (search_flag list * int64 option) option ]

and search_flag = [ `t | `T | `b | `B | `c | `C | `w | `W ]

and test =
  [ `True
  | `Numeric of (Number.t * string) Comparison.t
  | `String of string Comparison.t ]

and message = [ `No_space of string | `Space of string ]

type line =
  [ `Comment
  | `Apple of string
  | `Ext of string
  | `Mime of string
  | `Strength of int64 Arithmetic.t
  | `Rule of rule
  | `Name of offset * string
  | `Guid of offset * string
  | `Use of offset * bool * string ]

type error =
  [ `Empty
  | `Missing_test of s
  | `Unmatched_parenthesis of s
  | `Unexpected_trailer of s
  | `Invalid_number of s
  | `Invalid_integer of string
  | `Invalid_strength
  | `Invalid_type of s
  | `No_prefix of s * s
  | `Unsupported_type
  | `Invalid_use_command ]

val pp_error : Format.formatter -> error -> unit

val parse_line : string -> (line, error) result

val parse_in_channel : in_channel -> (line list, [> error ]) result
