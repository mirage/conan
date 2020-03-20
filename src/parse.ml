(* XXX(dinosaure): anyone can say that this parser is a sh*t and he is true. *)

type s = Astring.String.Sub.t

let ( >>= ) x f = match x with
  | Ok x -> f x
  | Error err -> Error err

let ( >|= ) x f = match x with
  | Ok x -> Ok (f x)
  | Error err -> Error err

let ( <.> ) f g = fun x -> f (g x)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

open Astring.String.Sub

let is_op = function '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' -> true | _ -> false

let is_wsp = function ' ' | '\t' .. '\r' -> true | _ -> false

let is_size = function 'Q' | 'q' | 'B' | 'b' | 'C' | 'c' | 's' | 'h' | 'S' | 'H' | 'l' | 'L' | 'm' | 'i' | 'I' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_compare = function '=' | '!' | '<' | '>' | '&' | '^' -> true | _ -> false
let is_greater = function '>' -> true | _ -> false

let is_ampersand = function '&' -> true | _ -> false

let is_c = function 'c' -> true | _ -> false
let is_C = function 'C' -> true | _ -> false
let is_b = function 'b' -> true | _ -> false
let is_B = function 'B' -> true | _ -> false
let is_s = function 's' -> true | _ -> false
let is_r = function 'r' -> true | _ -> false

type size =
  | Byte | Leshort | Beshort | Lelong | Belong | Melong
  | Leid3 | Beid3 | Lequad | Bequad

let size_of_string = function
  | "B" | "b" | "C" | "c" -> Byte
  | "s" | "h" -> Leshort
  | "S" | "H" -> Beshort
  | "l" -> Lelong
  | "L" -> Belong
  | "m" -> Melong
  | "i" -> Leid3
  | "I" -> Beid3
  | "q" -> Lequad
  | "Q" -> Bequad
  | v -> invalid_arg "Invalid size: %S" v

type op = Add | Sub | Mul | Div | Mod | And | Or | Xor

let op_of_string = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | "&" -> And
  | "|" -> Or
  | "^" -> Xor
  | v -> invalid_arg "Invalid operator: %S" v

type number =
  | Int of int64
  | Float of float

let parse_number s =
  let lexbuf = Lexing.from_string (to_string s) in
  match Integer.parser lexbuf with
  | Ok (`Int literal) ->
    let first = Lexing.lexeme_end lexbuf in
    Ok (Int (Int64.of_string literal), with_range ~first s)
  | Ok (`Float literal) ->
    let first = Lexing.lexeme_end lexbuf in
    Ok (Float (float_of_string literal), with_range ~first s)
  | Error `Malformed -> Error (`Invalid_number s)

let parse_int s =
  let lexbuf = Lexing.from_string (to_string s) in
  match Integer.parser lexbuf with
  | Ok (`Int literal) ->
    let first = Lexing.lexeme_end lexbuf in
    Ok (Int64.of_string literal, with_range ~first s)
  | _ -> Error (`Invalid_integer s)

let lparent = v "(" and rparent = v ")"
let ampersand = v "&"
let tilde = v "~"
let dot = v "."
let u = v "u"
let slash = v "/"

let parse_disp s =
  if is_prefix ~affix:lparent s && is_suffix ~affix:rparent s
  then parse_int ((tail <.> tail ~rev:true) s) >|= fun (v, s) -> (`Ind v, s)
  else parse_int s >|= fun (v, s) -> (`Dir v, s)

let parse_abs_or_rel s =
  let rel, s =
    if is_prefix ~affix:ampersand s
    then true, tail s
    else false, s in
  parse_int s >|= fun (v, s) ->
  if rel
  then (`Rel v, s)
  else (`Abs v, s)

let parse_indirect_offset s =
  if is_prefix ~affix:lparent s && is_suffix ~affix:rparent s
  then
    let s = (tail <.> tail ~rev:true) s in
    parse_abs_or_rel s >>= fun (offset, s) ->
    let size, s =
      if is_prefix ~affix:dot s
      then
        let size, s = (span ~max:1 ~sat:is_size <.> tail) s in
        Some ((size_of_string <.> to_string) size), s
      else None, s in
    let op, s =
      let invert, s =
        if is_prefix ~affix:tilde s
        then true, tail s
        else false, s in
      let op, s = span ~min:1 ~max:1 ~sat:is_op s in
      if length op = 1
      then Some (invert, (op_of_string <.> to_string) op), s
      else None, s in
    match op with
    | Some (invert, op) -> parse_disp s >|= fun (disp, _) -> offset, size, Some (invert, op, disp)
    | None -> Ok (offset, size, None)
  else Error (`Malformed s)

let parse_offset s =
  let level, s = span ~sat:is_greater s in
  let rel, ss = span ~sat:is_ampersand s in
  if is_prefix ~affix:lparent ss
  then parse_indirect_offset ss >|= fun v ->
    length level, `Ind ((if not (is_empty rel) then `Rel else `Abs), v)
  else parse_abs_or_rel s >>= fun (offset, s) ->
    if is_empty s
    then Ok (length level, offset)
    else Error (`Unprocessed s)

let prefix ~affix s =
  match cut ~sep:affix s with
  | Some (empty, s) ->
    if is_empty empty then Ok s else Error (`No_prefix (affix, s))
  | None -> Error (`No_prefix (affix, s))

let le = v "le" and be = v "be" and me = v "me"

let byte     = v "byte"
and long     = v "long"
and quad     = v "quad"
and date     = v "date"
and clear    = v "clear"
and short    = v "short"
and ldate    = v "ldate"
and regex    = v "regex"
and qdate    = v "qdate"
and float    = v "float"
and double   = v "double"
and qldate   = v "qldate"
and qwdate   = v "qwdate"
and search   = v "search"
and string   = v "string"
and pstring  = v "pstring"
and default  = v "default"
and indirect = v "indirect"

let _16 = v "16"

type numeric =
  [ `Byte | `Short | `Long | `Quad
  | `Date | `Ldate | `Qdate | `Qldate | `Qwdate
  | `Double | `Float ]

type default = [ `Default ]
type regex = [ `Regex ]

let parse_type s =
  let unsigned, s =
    if is_prefix ~affix:u s
    then true, tail s
    else false, s in
  let is_le = is_prefix ~affix:le s in
  let is_be = is_prefix ~affix:be s in
  let is_me = is_prefix ~affix:me s in
  let s = if is_le || is_be || is_me then (tail <.> tail) s else s in
  let res = match String.sub (to_string s) 0 4 with
    | "clea" -> prefix ~affix:clear  s >|= fun s -> `Clear, s
    | "byte" -> prefix ~affix:byte   s >|= fun s -> `Byte, s
    | "shor" -> prefix ~affix:short  s >|= fun s -> `Short, s
    | "long" -> prefix ~affix:long   s >|= fun s -> `Long, s
    | "quad" -> prefix ~affix:quad   s >|= fun s -> `Quad, s
    | "doub" -> prefix ~affix:double s >|= fun s -> `Double, s
    | "floa" -> prefix ~affix:float  s >|= fun s -> `Float, s
    | "date" -> prefix ~affix:date   s >|= fun s -> `Date, s
    | "ldat" -> prefix ~affix:ldate  s >|= fun s -> `Ldate, s
    | "qdat" -> prefix ~affix:qdate  s >|= fun s -> `Qdate, s
    | "qlda" -> prefix ~affix:qldate s >|= fun s -> `Qldate, s
    | "qwda" -> prefix ~affix:qwdate s >|= fun s -> `Qwdate, s
    | "rege" -> prefix ~affix:regex  s >|= fun s -> `Regex, s
    | "sear" -> prefix ~affix:search s >|= fun s -> `Search, s
    | "stri" -> prefix ~affix:string s >>= fun s ->
      let string16 = is_prefix ~affix:_16 s in
      if string16
      then prefix ~affix:_16 s >|= fun s -> `String16, s
      else Ok (`String, s)
    | "pstr" -> prefix ~affix:pstring s >|= fun s -> `Pstring, s
    | "defa" -> prefix ~affix:default s >|= fun s -> `Default, s
    | "indi" -> prefix ~affix:indirect s >|= fun s -> `Indirect, s
    | _ -> Error (`Invalid_type s) in
  let endian = match is_le, is_be, is_me with
    | true, false, false -> Some `LE
    | false, true, false -> Some `BE
    | false, false, true -> Some `ME
    | false, false, false -> None
    | _ -> assert false in
  res >>= fun (kind, s) -> match kind with
  | `Clear -> Ok (unsigned, `Clear)
  | `Indirect ->
    ( match cut ~sep:slash s with
      | None -> Ok (unsigned, `Indirect false)
      | Some (empty, s) ->
        if is_empty empty
        then
          let has_r = exists is_r s in
          Ok (unsigned, `Indirect has_r)
        else Error (`Unprocessed empty) )
  | #default -> Ok (unsigned, `Default)
  | #regex ->
    ( match cut ~sep:slash s with
      | None -> Ok (unsigned, `Regex None)
      | Some (empty, s) ->
        if is_empty empty
        then
          let has_c = exists is_c s in
          let has_s = exists is_s s in
          let s = trim ~drop:(( not ) <.> is_digit) s in
          Ok (unsigned, `Regex (Some (has_c, has_s, to_int s)))
        else Error (`Unprocessed empty) )
  | `String | `Search ->
    ( match cut ~sep:slash s with
      | None -> Ok (unsigned, `Search None)
      | Some (empty, s) ->
        if is_empty empty
        then
          let has_b = exists is_b s in
          let has_B = exists is_B s in
          let has_c = exists is_c s in
          let has_C = exists is_C s in
          let s = trim ~drop:(( not ) <.> is_digit) s in
          if is_empty s
          then Ok (unsigned, `Search (Some (has_b,
                                            has_B,
                                            has_c,
                                            has_C,
                                            None)))
          else parse_int s >>= fun (v, _) ->
            Ok (unsigned, `Search (Some (has_b,
                                         has_B,
                                         has_c,
                                         has_C,
                                         Some v)))
        else Error (`Unprocessed empty) )
  | `Pstring ->
    ( match cut ~sep:slash s with
      | None -> Ok (unsigned, `String8 None)
      | Some (empty, s) ->
        if is_empty empty
        then
          let has_b = exists is_b s in
          let has_B = exists is_B s in
          let has_c = exists is_c s in
          let has_C = exists is_C s in
          Ok (unsigned, `String8 (Some (has_b, has_B, has_c, has_C)))
        else Error (`Unprocessed empty) )
  | `String16 ->
    ( match endian with
      | Some (`LE | `BE as endian) -> Ok (unsigned, `String16 endian)
      | _ -> Error `Unsupported_type )
  | #numeric as numeric ->
    let op, s =
      let op, s = span ~min:1 ~max:1 ~sat:is_op s in
      if length op = 1
      then Some ((op_of_string <.> to_string) op), s
      else None, s in
    match op with
    | Some op -> parse_int s >|= fun (v, _) -> unsigned, `Numeric (endian, numeric, Some (op, v))
    | None -> Ok (unsigned, `Numeric (endian, numeric, None))

let x = v "x"

type compare =
  | Equal | Not | Greater | Lower | And | Xor

let compare_of_string = function
  | "=" -> Equal
  | "!" -> Not
  | "<" -> Greater
  | ">" -> Lower
  | "&" -> And
  | "^" -> Xor
  | v -> invalid_arg "Invalid compare: %S" v

let parse_test s =
  if equal_bytes s x then Ok `True
  else
    let compare, s = span ~min:1 ~max:1 ~sat:is_compare s in
    let compare =
      if is_empty compare
      then compare_of_string "=" else (compare_of_string <.> to_string) compare in
    match parse_number s with
    | Ok (v, empty) ->
      if is_empty empty
      then Ok (`Numeric (compare, v))
      else Ok (`String (compare, to_string s))
    | Error _ ->
      let lexbuf = Lexing.from_string (to_string s) in
      let buf = Buffer.create (length s) in
      let contents = Integer.string buf lexbuf in (* escape *)
      Ok (`String (compare, contents))

let parse_message s = match head s with
  | Some '\x08' ->
    Ok (`No_space ((to_string <.> tail) s))
  | Some '\\' ->
    ( match (head <.> tail) s with
      | Some 'b' -> Ok (`No_space ((to_string <.> tail <.> tail) s))
      | _ -> Ok (`Space (to_string s)) )
  | _ -> Ok (`Space (to_string s))

type operation = Add | Sub | Mul | Div
let operation_of_string = function
  | "+" -> Add
  | "*" -> Mul
  | "/" -> Div
  | "-" -> Sub
  | v -> invalid_arg "Invalid operation: %S" v

let parse_strength s =
  let s = trim ~drop:is_wsp s in
  let op, s = span ~min:1 ~max:1 ~sat:(function '+' | '*' | '/' | '-' -> true | _ -> false) s in
  let s = trim ~drop:is_wsp s in
  if length op = 1
  then parse_int s >|= fun (v, _) -> ((operation_of_string <.> to_string) op), v
  else Error `Invalid_strength

let hws = v "\t"
let wsp = v " "

type rule = offset * kind * test * message
and offset =
  int * [ `Abs of int64
        | `Rel of int64
        | `Ind of [ `Abs | `Rel ] * ([ `Abs of int64 | `Rel of int64 ]
                                     * size option
                                     * (bool * op * [ `Ind of int64 | `Dir of int64 ]) option) ]
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

let best_effort s = match cuts ~empty:false ~sep:hws s with
  | [] -> Error (`Malformed s)
  | [ _ ] ->
    ( match cuts ~empty:false ~sep:wsp s with
      | [] -> Error (`Malformed s)
      | [ test ] -> Ok (test, [])
      | test :: message -> Ok (test, message) )
  | test :: message -> Ok (test, message)

let parse_line line =
  match Astring.String.head line with
  | None | Some '#' -> Ok `Comment
  | _ ->
    let s = v line in
    let verb, s = span ~sat:(( not ) <.> is_wsp) s in
    let s = drop ~sat:is_wsp s in

    match to_string verb with
    | "!:apple" -> Ok (`Apple (to_string s))
    | "!:ext" -> Ok (`Ext (to_string s))
    | "!:mime" -> Ok (`Mime (to_string s))
    | "!:strength" ->
      parse_strength s >>= fun strength ->
      Ok (`Strength strength)
    | _ ->
      parse_offset verb >>= fun offset ->
      let ty, s = span ~sat:(( not ) <.> is_wsp) s in
      let s = drop ~sat:is_wsp s in
      match to_string ty with
      | "name" -> Ok (`Name (offset, to_string s))
      | "guid" -> Ok (`Guid (offset, to_string s))
      | "use" -> Ok (`Use (offset, to_string s))
      | _ ->
        parse_type ty >>= fun ty ->
        best_effort s >>= fun (test, message) ->
        parse_test test >>= fun test ->
        parse_message (concat ~sep:wsp message) >>= fun message ->
        Ok (`Rule (offset, ty, test, message))

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

let pp_error ppf err =
  let pp = Format.fprintf in
  let to_string = Astring.String.Sub.to_string in

  match err with
  | `Unprocessed s -> pp ppf "Unprocessed %S" (to_string s)
  | `Invalid_number s -> pp ppf "Invalid number %S" (to_string s)
  | `Invalid_integer s -> pp ppf "Invalid integer %S" (to_string s)
  | `Invalid_strength -> pp ppf "Invalid strength"
  | `Invalid_type s -> pp ppf "Invalid type %S" (to_string s)
  | `Malformed s -> pp ppf "Malformed %S" (to_string s)
  | `No_prefix (prefix, s) -> pp ppf "Expected prefix %S on %S" (to_string prefix) (to_string s)
  | `Unexpected s -> pp ppf "Unexpected %S" s
  | `Unsupported_type -> pp ppf "Unsupported type"

let parse_in_channel ic =
  let rec go acc = match input_line ic with
    | line -> parse_line line >>= fun v -> go (v :: acc)
    | exception End_of_file -> Ok (List.rev acc) in
  go []
