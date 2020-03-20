type s = Astring.String.Sub.t

type number =
  | Int of int64
  | Float of float

type op = Add | Sub | Mul | Div | Mod | And | Or | Xor
type operation = Add | Sub | Mul | Div
type compare = Equal | Not | Greater | Lower | And | Xor

type size =
  | Byte | Leshort | Beshort | Lelong | Belong | Melong
  | Leid3 | Beid3 | Lequad | Bequad

type numeric =
  [ `Byte | `Short | `Long | `Quad
  | `Date | `Ldate | `Qdate | `Qldate | `Qwdate
  | `Double | `Float ]

type rule = offset * kind * test * message
and offset =
  int * [ `Abs of int64
        | `Rel of int64
        | `Ind of [ `Abs | `Rel ] * ([ `Abs of int64 | `Rel of int64 ]
                                     * size option
                                     * (bool * op * [ `Dir of int64 | `Ind of int64 ]) option) ]
and kind =
  bool * [ `Numeric of [ `BE | `LE | `ME ] option * numeric * (op * int64) option
         | `Default | `Clear | `Indirect of bool
         | `Regex of (bool * bool * int option) option
         | `String16 of [ `BE | `LE ]
         | `String8 of (bool * bool * bool * bool) option
         | `Search of (bool * bool * bool * bool * int64 option) option ]
and test =
  [ `True
  | `Numeric of compare * number
  | `String of compare * string ]
and message = [ `No_space of string | `Space of string ]

type line =
  [ `Comment
  | `Apple of string
  | `Ext of string
  | `Mime of string
  | `Strength of (operation * int64)
  | `Rule of rule
  | `Name of offset * string
  | `Guid of offset * string
  | `Use of offset * string ]

type error =
  [ `Unprocessed of s
  | `Invalid_number of s
  | `Invalid_integer of s
  | `Invalid_strength
  | `Invalid_type of s
  | `Malformed of s
  | `No_prefix of (s * s)
  | `Unexpected of string
  | `Unsupported_type ]

val pp_error : Format.formatter -> error -> unit
val parse_in_channel : in_channel -> (line list, error) result
