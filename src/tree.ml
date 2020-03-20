let ( <.> ) f g = fun x -> f (g x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type default = Default
type clear = Clear
type endian = [ `BE | `LE | `NE | `ME ]

type size = Parse.size =
  | Byte
  | Leshort | Beshort
  | Lelong  | Belong | Melong
  | Leid3   | Beid3
  | Lequad  | Bequad

let pp = Format.fprintf

let pp_size ppf = function
  | Byte -> pp ppf "byte"
  | Leshort -> pp ppf "leshort"
  | Beshort -> pp ppf "beshort"
  | Lelong -> pp ppf "lelong"
  | Belong -> pp ppf "belong"
  | Melong -> pp ppf "melong"
  | Leid3 -> pp ppf "leid3"
  | Beid3 -> pp ppf "beid3"
  | Lequad -> pp ppf "lequad"
  | Bequad -> pp ppf "bequad"

type 'a integer =
  | Byte : char integer
  | Short : int integer
  | Int32 : int32 integer
  | Int64 : int64 integer

let pp_integer : type a. Format.formatter -> a integer -> unit = fun ppf -> function
  | Byte -> pp ppf "byte"
  | Short -> pp ppf "short"
  | Int32 -> pp ppf "int32"
  | Int64 -> pp ppf "int64"

type 'a arithmetic =
  | Invert of 'a arithmetic
  | Add of 'a
  | Sub of 'a
  | Mul of 'a
  | Div of 'a
  | Mod of 'a
  | Bitwise_and of 'a
  | Bitwise_xor of 'a
  | Bitwise_or of 'a

let rec pp_arithmetic ppf = function
  | Invert v -> pp ppf "~ %a" pp_arithmetic v
  | Add _ -> pp ppf "+"
  | Sub _ -> pp ppf "-"
  | Mul _ -> pp ppf "*"
  | Div _ -> pp ppf "/"
  | Mod _ -> pp ppf "%%"
  | Bitwise_and _ -> pp ppf "&"
  | Bitwise_xor _ -> pp ppf "^"
  | Bitwise_or _ -> pp ppf "|"

let rec pp_arithmetic_with_val pp_val ppf = function
  | Invert v -> pp ppf "~ %a" (pp_arithmetic_with_val pp_val) v
  | Add v -> pp ppf "+%a" pp_val v
  | Sub v -> pp ppf "-%a" pp_val v
  | Mul v -> pp ppf "*%a" pp_val v
  | Div v -> pp ppf "/%a" pp_val v
  | Mod v -> pp ppf "%%%a" pp_val v
  | Bitwise_and v -> pp ppf "&%a" pp_val v
  | Bitwise_xor v -> pp ppf "^%a" pp_val v
  | Bitwise_or v -> pp ppf "|%a" pp_val v

type offset =
  | Relative of offset
  | Absolute of offset
  | Value of int64
  | Read of offset * size
  | Calculation of offset * offset arithmetic

let rec pp_offset ppf = function
  | Relative v -> pp ppf "&%a" pp_offset v
  | Absolute v -> pp ppf "%a" pp_offset v
  | Value v -> pp ppf "%Ld" v
  | Read (v, size) -> pp ppf "(%a.%a)" pp_offset v pp_size size
  | Calculation (v, c) -> pp ppf "[%a%a]" pp_offset v pp_arithmetic c

type comparison = Parse.compare =
  | Equal
  | Not
  | Greater
  | Lower
  | And
  | Xor

let pp_comparison ppf = function
  | Equal -> pp ppf "="
  | Not -> pp ppf "!"
  | Greater -> pp ppf "<"
  | Lower -> pp ppf ">"
  | And -> pp ppf "&"
  | Xor -> pp ppf "^"

type unsigned = { unsigned : bool }

let pp_unsigned ppf { unsigned } =
  if unsigned then pp ppf "u"

type ('test, 'v) kind =
  | Default : (default, default) kind
  | Regex : { case_insensitive : bool
            ; start : bool
            ; line_counter : int option } -> (Re.t, string) kind
  | Clear : (clear, clear) kind
  | Search : { compact_whitespaces : bool
             ; optional_blank : bool
             ; lower_case_insensitive : bool
             ; upper_case_insensitive : bool
             ; text : bool
             ; binary : bool
             ; trim : bool
             ; range : int64
             ; pattern : string } -> (string, string) kind
  | Pascal_string (* uh?! *) : (string, string) kind
  | Unicode : [ `BE | `LE ] -> (Uchar.t, string) kind
  | Byte   : unsigned * char arithmetic -> (char, char) kind
  | Short  : unsigned * int arithmetic
             * [ `BE | `LE | `NE ]  -> (int, int) kind
  | Long   : unsigned * int32 arithmetic
             * endian -> (int32, int32) kind
  | Quad   : unsigned * int64 arithmetic
             * endian -> (int64, int64) kind
  | Float  : unsigned * float arithmetic
             * endian -> (float, float) kind
  | Double : unsigned * float arithmetic
             * endian -> (float, float) kind
  | Indirect : [ `Rel | `Abs ] -> ('test, 'v) kind

let pp_flag letter ppf = function true -> pp ppf "%c" letter | false -> ()
let pp_option pp_val ppf = function Some x -> pp_val ppf x | None -> ()
let pp_int ppf = pp ppf "0x%x"
let pp_int32 ppf = pp ppf "0x%lx"
let pp_int64 ppf = pp ppf "0x%Lx"
let pp_float ppf = pp ppf "%f"

let pp_endian ppf = function
  | `BE -> pp ppf "be"
  | `LE -> pp ppf "le"
  | `ME -> pp ppf "me"
  | `NE -> pp ppf "ne"

let pp_kind : type test v. Format.formatter -> (test, v) kind -> unit = fun ppf -> function
  | Default -> pp ppf "default"
  | Clear -> pp ppf "clear"
  | Regex { case_insensitive= false; start= false; line_counter= None; } ->
    pp ppf "regex"
  | Regex { case_insensitive; start; line_counter; } ->
    pp ppf "regex/%a%a%a"
      (pp_flag 'c') case_insensitive
      (pp_flag 's') start
      (pp_option pp_int) line_counter
  | Search { compact_whitespaces= false
           ; optional_blank= false
           ; lower_case_insensitive= false
           ; upper_case_insensitive= false
           ; text= false
           ; binary= false
           ; trim= false
           ; range= 0L
           ; pattern= "" } ->
    pp ppf "search"
  | Search { compact_whitespaces
           ; optional_blank
           ; lower_case_insensitive
           ; upper_case_insensitive
           ; text
           ; binary
           ; trim
           ; range
           ; pattern= _ } ->
    pp ppf "search/%a%a%a%a%a%a%a/%Ld"
      (pp_flag 'W') compact_whitespaces
      (pp_flag 'w') optional_blank
      (pp_flag 'c') lower_case_insensitive
      (pp_flag 'C') upper_case_insensitive
      (pp_flag 'b') binary
      (pp_flag 't') text
      (pp_flag 'T') trim
      range
  | Pascal_string -> pp ppf "pstring"
  | Unicode `BE -> pp ppf "bestring16"
  | Unicode `LE -> pp ppf "lestring16"
  | Byte (unsigned, arithmetic)->
    let pp_byte ppf v = pp ppf "%02x" (Char.code v) in
    pp ppf "%abyte%a" pp_unsigned unsigned (pp_arithmetic_with_val pp_byte) arithmetic
  | Short (unsigned, arithmetic, endian) ->
    pp ppf "%a%ashort%a" pp_unsigned unsigned pp_endian endian (pp_arithmetic_with_val pp_int) arithmetic
  | Long (unsigned, arithmetic, endian) ->
    pp ppf "%a%along%a" pp_unsigned unsigned pp_endian endian (pp_arithmetic_with_val pp_int32) arithmetic
  | Quad (unsigned, arithmetic, endian) ->
    pp ppf "%a%aquad%a" pp_unsigned unsigned pp_endian endian (pp_arithmetic_with_val pp_int64) arithmetic
  | Float (unsigned, arithmetic, endian) ->
    pp ppf "%a%afloat%a" pp_unsigned unsigned pp_endian endian (pp_arithmetic_with_val pp_float) arithmetic
  | Double (unsigned, arithmetic, endian) ->
    pp ppf "%a%adouble%a" pp_unsigned unsigned pp_endian endian (pp_arithmetic_with_val pp_float) arithmetic
  | Indirect `Rel ->
    pp ppf "indirect/r"
  | Indirect `Abs ->
    pp ppf "indirect"

type 'a test =
  | True : 'a test
  | Numeric : 'a integer * comparison * 'a -> 'a test
  | Float : comparison * float -> float test
  | Unicode : Uchar.t * comparison -> Uchar.t test
  | String : comparison * string -> string test

let pp_of_integer : type a. a integer -> Format.formatter -> a -> unit = function
  | Byte -> fun ppf v -> pp ppf "%02x" (Char.code v)
  | Short -> fun ppf v -> pp ppf "0x%x" v
  | Int32 -> fun ppf v -> pp ppf "0x%lx" v
  | Int64 -> fun ppf v -> pp ppf "0x%Lx" v

let pp_test : type a. Format.formatter -> a test -> unit = fun ppf -> function
  | True -> pp ppf "x"
  | Numeric (integer, comparison, v) ->
    let pp_val = pp_of_integer integer in
    pp ppf "%a%a" pp_comparison comparison pp_val v
  | Float (comparison, v) ->
    pp ppf "%a%f" pp_comparison comparison v
  | Unicode (uchr, comparison) ->
    pp ppf "%aU+%04x" pp_comparison comparison (Uchar.to_int uchr)
  | String (comparison, v) ->
    pp ppf "%a%S" pp_comparison comparison v

type 'a fmt =
  { fmt : 'r. unit -> ('a -> 'r, Format.formatter, unit, 'r) format4 }

let pp_fmt ppf v =
  let CamlinternalFormatBasics.Format (_, v) = v in
  pp ppf "%s" v

type operation =
  | Rule : offset * ('test, 'v) kind * 'test test * 'v fmt -> operation
  | Name : offset * string -> operation

let pp_operation ppf = function
  | Rule (offset, kind, test, fmt) ->
    pp ppf "%a\t%a\t%a\t%a" pp_offset offset pp_kind kind pp_test test pp_fmt (fmt.fmt ())
  | Name (offset, name) ->
    pp ppf "%a\t%s" pp_offset offset name

type tree =
  | Node of (operation * tree) list
  | Done

let pp_level ppf n =
  let rec go = function
    | 0 -> ()
    | n -> pp ppf ">" ; go (pred n) in
  go (max n 0)

let pp_tree ppf tree =
  let rec go level = function
    | Done -> ()
    | Node lst ->
      let lst = List.rev lst in
      let iter (rule, tree) =
        pp ppf "%a%a\n%!" pp_level level pp_operation rule ;
        go (succ level) tree in
      List.iter iter lst in
  go 0 tree

let system_long =
  if Sys.big_endian
  then Belong else Lelong

let system_endian = if Sys.big_endian then `BE else `LE

let indirect_1 ?(size= system_long) (invert, op, kind) =
  let calculation v = match (op : Parse.op) with
    | Parse.Add -> Add v
    | Parse.Sub -> Sub v
    | Parse.Mul -> Mul v
    | Parse.Div -> Div v
    | Parse.Mod -> Mod v
    | Parse.And -> Bitwise_and v
    | Parse.Xor -> Bitwise_xor v
    | Parse.Or  -> Bitwise_or v in
  let calculation v =
    if invert
    then Invert (calculation v)
    else calculation v in
  match kind with
  | `Dir v -> calculation (Relative (Value v))
  | `Ind v -> calculation (Relative (Read (Value v, size)))

let indirect_0 (return, (offset, size, disp)) =
  let size = Option.value ~default:system_long size in
  let offset = match disp, offset with
    | None, `Rel offset -> Relative (Value offset)
    | None, `Abs offset -> Absolute (Value offset)
    | Some disp, `Rel offset ->
      let calculation = indirect_1 ~size disp in
      Relative (Calculation (Value offset, calculation))
    | Some disp, `Abs offset ->
      let calculation = indirect_1 ~size disp in
      Absolute (Calculation (Value offset, calculation)) in
  match return with
  | `Rel -> Relative (Read (offset, size))
  | `Abs -> Absolute (Read (offset, size))

let offset = function
  | `Abs offset -> Absolute (Value offset)
  | `Rel offset -> Relative (Value offset)
  | `Ind ind -> indirect_0 ind

type k = Kind : ('test, 'v) kind -> k
type t = Test : 'test test -> t
type f = Format : 'v fmt -> f

let identity x = x

let calculation
  :  cast:(int64 -> 'v)
  -> (Parse.op * int64) option
  -> 'v arithmetic = fun ~cast -> function
  | None -> Add (cast 0L)
  | Some (Parse.Add, v) -> Add (cast v)
  | Some (Parse.Sub, v) -> Sub (cast v)
  | Some (Parse.Mul, v) -> Mul (cast v)
  | Some (Parse.Div, v) -> Div (cast v)
  | Some (Parse.Mod, v) -> Mod (cast v)
  | Some (Parse.And, v) -> Bitwise_and (cast v)
  | Some (Parse.Xor, v) -> Bitwise_xor (cast v)
  | Some (Parse.Or, v) -> Bitwise_or (cast v)

let force_to_use_string_formatter message =
  match Astring.String.cut ~sep:"%" message with
  | None -> None
  | Some (x, r) ->
    if r = "" then None
    else match r.[0] with
      | _ ->
        let r = Bytes.of_string r in
        Bytes.set r 0 's' ; Some (x ^ (Bytes.to_string r))

let format_of_kind
  : type test v. (test, v) kind -> _ -> (v -> 'r, _, _, 'r) format4
  = fun kind message ->
    let (prefix : _ format4), message = match message with
      | `No_space m -> "%,", m | `Space m -> " ", m in
    try match kind with
      | Default ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt
          : (default -> 'r, _, _, 'r) format4
          = Format (Custom (Custom_succ Custom_zero,
                            (fun () Default -> ""), fmt), str) in
        prefix ^^ fmt
      | Clear ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt
          : (clear -> 'r, _, _, 'r) format4
          = Format (Custom (Custom_succ Custom_zero,
                            (fun () Clear -> ""), fmt), str) in
        prefix ^^ fmt
      | Byte _ ->
        prefix ^^ (Scanf.format_from_string message "%c")
      | Search _ ->
        prefix ^^ (Scanf.format_from_string message "%s")
      | Unicode _ ->
        prefix ^^ (Scanf.format_from_string message "%s")
      | Short _ ->
        prefix ^^ (Scanf.format_from_string message "%d")
      | Long _ ->
        prefix ^^ (Scanf.format_from_string message "%ld")
      | Quad _ ->
        prefix ^^ (Scanf.format_from_string message "%Ld")
      | Float _ ->
        prefix ^^ (Scanf.format_from_string message "%f")
      | Double _ ->
        prefix ^^ (Scanf.format_from_string message "%f")
      | Regex _ ->
        prefix ^^ (Scanf.format_from_string message "%s")
      | Pascal_string ->
        prefix ^^ (Scanf.format_from_string message "%s")
      | Indirect _ -> assert false (* TODO *)
    with _ -> match force_to_use_string_formatter message with
      | Some message ->
        let Format (_fmt, _str) = Scanf.format_from_string message "%s" in
        assert false (* TODO(dinosaure): replace [String] by [Custom]. *)
      | None ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt = Format (Custom (Custom_succ Custom_zero,
                        (fun () (_ : v) -> ""), fmt), str) in
        prefix ^^ fmt

let rule
  : Parse.rule -> operation
  = fun ((_level, o), kind, test, message) ->
  let offset = offset o in
  let Kind kind = match kind with
    | _, `Default -> Kind Default
    | _, `Clear -> Kind Clear
    | _, `Regex (Some (case_insensitive, start, line_counter)) ->
      Kind (Regex { case_insensitive; start; line_counter; })
    | _, `Regex None ->
      Kind (Regex { case_insensitive= false
                  ; start= false
                  ; line_counter= None })
    | _, `String16 endian ->
      Kind (Unicode endian)
    | _, `String8 (Some (b, _B, c, _C)) ->
      Kind (Search { pattern= ""
                   ; lower_case_insensitive= c
                   ; upper_case_insensitive= _C
                   ; binary= b || _B
                   ; range= 0L
                   ; text= not (b || _B)        (* TODO: [t] *)
                   ; compact_whitespaces= false (* TODO: [W] *)
                   ; optional_blank= false      (* TODO: [w] *)
                   ; trim= false                (* TODO: [T] *) })
    | _, `Search None | _, `String8 None ->
      Kind (Search { pattern= ""
                   ; lower_case_insensitive= false
                   ; upper_case_insensitive= false
                   ; binary= false
                   ; range= 0L
                   ; text= false
                   ; compact_whitespaces= false
                   ; optional_blank= false
                   ; trim= false })
    | _, `Search (Some (b, _B, c, _C, range)) ->
      let range = Option.value ~default:0L range in
      Kind (Search { pattern= ""
                   ; lower_case_insensitive= c
                   ; upper_case_insensitive= _C
                   ; binary= b || _B
                   ; range
                   ; text= not (b || _B)        (* TODO: [t] *)
                   ; compact_whitespaces= false (* TODO: [W] *)
                   ; optional_blank= false      (* TODO: [w] *)
                   ; trim= false                (* TODO: [T] *) })
    | _, `Indirect rel ->
      Kind (Indirect (if rel then `Rel else `Abs))
    | unsigned, `Numeric (_endian, `Byte, c) ->
      Kind (Byte ({ unsigned },
                  calculation ~cast:(Char.chr <.> Int64.to_int) c))
    | unsigned, `Numeric (Some (`BE | `LE as endian), `Short, c) ->
      Kind (Short ({ unsigned },
                   calculation ~cast:Int64.to_int c, endian))
    | unsigned, `Numeric (_, `Short, c) ->
      Kind (Short ({ unsigned },
                   calculation ~cast:Int64.to_int c, system_endian))
    | unsigned, `Numeric (endian, `Long, c) ->
      let endian = Option.value ~default:system_endian endian in
      Kind (Long ({ unsigned },
                  calculation ~cast:Int64.to_int32 c,
                  (endian :> endian)))
    | unsigned, `Numeric (endian, `Quad, c) ->
      let endian = Option.value ~default:system_endian endian in
      Kind (Quad ({ unsigned },
                  calculation ~cast:identity c,
                  (endian :> endian)))
    | _, _ -> assert false in
  let Test test = match test, kind with
    | `True, _ -> Test True
    | `Numeric (compare, Int v), Byte _ ->
      let chr = (Char.chr <.> Int64.to_int) v in
      Test (Numeric (Byte, compare, chr))
    | `Numeric (compare, Int v), Short _ ->
      Test (Numeric (Short, compare, (Int64.to_int v) land 0xffff))
    | `Numeric (compare, Int v), Long _ ->
      Test (Numeric (Int32, compare, (Int64.to_int32 v)))
    | `Numeric (compare, Int v), Quad _ ->
      Test (Numeric (Int64, compare, v))
    | `Numeric (compare, Int v), Unicode _ ->
      let uchar = (Uchar.of_int <.> Int64.to_int) v in
      Test (Unicode (uchar, compare))
    (* TODO(dinosaure): [`String] and [Unicode]. *)
    | `Numeric (compare, Float v), Double _ ->
      Test (Float (compare, v))
    | `Numeric (compare, Float v), Float _ ->
      Test (Float (compare, v))
    | `String (compare, v), Search _
    | `String (compare, v), Pascal_string ->
      Test (String (compare, v))
    | `Numeric (_, Int v), kind ->
      invalid_arg "Impossible to test an integer (%Lx) with the given type: %a" v pp_kind kind
    | `Numeric (_, Float v), kind ->
      invalid_arg "Impossible to test a float (%f) with the given type: %a" v pp_kind kind
    | `String (_, v), kind ->
      invalid_arg "Impossible to test a string (%S) with the given type: %a" v pp_kind kind in
  let make
    : type test0 test1 v. test0 test -> (test1, v) kind -> operation
    = fun test kind -> match test, kind with
      | True, Default
      | String _, Default
      | Numeric _, Default
      | Float _, Default
      | Unicode _, Default ->
        Rule (offset, kind, True, { fmt= fun () -> format_of_kind kind message })
      | True, Clear
      | String _, Clear
      | Numeric _, Clear ->
        Rule (offset, kind, True, { fmt= fun () -> format_of_kind kind message })
      | String (Equal, pattern), Search search ->
        Rule (offset, Search { search with pattern }, test, { fmt= fun () -> format_of_kind kind message })
      | Numeric (Byte, _, _), Byte _ ->
        Rule (offset, kind, test, { fmt= fun () -> format_of_kind kind message })
      | Numeric (Short, _, _), Short _ ->
        Rule (offset, kind, test, { fmt= fun () -> format_of_kind kind message })
      | Numeric (Int32, _, _), Long _ ->
        Rule (offset, kind, test, { fmt= fun () -> format_of_kind kind message })
      | Numeric (Int64, _, _), Quad _ ->
        Rule (offset, kind, test, { fmt= fun () -> format_of_kind kind message })
      | True, _ ->
        Rule (offset, kind, True, { fmt= fun () -> format_of_kind kind message })
      | test, kind ->
        invalid_arg "Impossible to operate a test (%a) on the given value (%a)"
          pp_test test pp_kind kind in
  make test kind

let name
  : _ -> operation
  = fun ((_level, o), name) ->
  let offset = offset o in
  Name (offset, name)

let operation = function
  | `Rule (((level, _), _, _, _) as v) ->
    let rule = rule v in
    level, rule
  | `Name (((level, _), _) as v) ->
    let name = name v in
    level, name
  | _ -> assert false (* TODO *)

let rec left = function
  | Done | Node [] -> 0
  | Node ((_, hd) :: _) ->
    1 + left hd

let append tree (line : Parse.line) = match line with
  | `Rule _ | `Name _ ->
    let level, operation = operation line in
    if level <= left tree
    then
      let rec go cur tree =
        if cur = level
        then match tree with
          | Done ->
            Node [ operation, Done ]
          | Node l ->
            Node ((operation, Done) :: l)
        else match tree with
          | Done | Node [] -> Node [ operation, Done ]
          | Node ((x, hd) :: tl) ->
            let hd = go (succ cur) hd in
            Node ((x, hd) :: tl) in
      go 0 tree
    else tree
  | _ -> tree
