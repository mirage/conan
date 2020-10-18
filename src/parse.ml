(* XXX(dinosaure): anyone can say that this parser is a sh*t and he is true. *)

type s = Sub.t

let string_head s = if String.length s > 0 then Some s.[0] else None

let string_tail s =
  if String.length s > 0 then String.sub s 1 (String.length s - 1) else ""

let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err

let ( >|= ) x f = match x with Ok x -> Ok (f x) | Error err -> Error err

let ( <.> ) f g x = f (g x)

open Sub

let is_wsp = function ' ' | '\t' .. '\r' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_greater = function '>' -> true | _ -> false

let is_ampersand = function '&' -> true | _ -> false

let is_c = function 'c' -> true | _ -> false

let is_C = function 'C' -> true | _ -> false

let is_b = function 'b' -> true | _ -> false

let is_B = function 'B' -> true | _ -> false

let is_s = function 's' -> true | _ -> false

let is_r = function 'r' -> true | _ -> false

let is_l = function 'l' -> true | _ -> false

let is_t = function 't' -> true | _ -> false

let is_T = function 'T' -> true | _ -> false

let is_w = function 'w' -> true | _ -> false

let is_W = function 'W' -> true | _ -> false

let lparent = v "("

and rparent = v ")"

let ampersand = v "&"

let tilde = v "~"

let dot = v "."

let u = v "u"

let slash = v "/"

let parse_disp s =
  if is_prefix ~affix:lparent s && is_suffix ~affix:rparent s
  then Integer.parse ((tail <.> tail ~rev:true) s) >|= fun (v, s) -> (`Ind v, s)
  else Integer.parse s >|= fun (v, s) -> (`Dir v, s)

let parse_abs_or_rel s =
  let rel, s =
    if is_prefix ~affix:ampersand s then (true, tail s) else (false, s) in
  Integer.parse s >|= fun (v, s) -> if rel then (`Rel v, s) else (`Abs v, s)

let parse_indirect_offset s =
  if is_prefix ~affix:lparent s && is_suffix ~affix:rparent s
  then
    let s = (tail <.> tail ~rev:true) s in
    parse_abs_or_rel s >>= fun (offset, s) ->
    let size, s =
      if is_prefix ~affix:dot s
      then
        let size, s = (span ~max:1 ~sat:Size.is_size <.> tail) s in
        (Some ((Size.of_string <.> to_string) size), s)
      else (None, s) in
    let arithmetic, s =
      let invert, s =
        if is_prefix ~affix:tilde s then (true, tail s) else (false, s) in
      let arithmetic, s = span ~min:1 ~max:1 ~sat:Arithmetic.is s in
      if length arithmetic = 1
      then (Some (invert, to_string arithmetic), s)
      else (None, s) in
    match arithmetic with
    | Some (invert, arithmetic) ->
        parse_disp s >>= fun (disp, empty) ->
        if is_empty empty
        then
          let arithmetic = Arithmetic.of_string ~with_val:disp arithmetic in
          let arithmetic =
            if invert then Arithmetic.invert arithmetic else arithmetic in
          Ok (offset, size, Some arithmetic)
        else Error (`Unexpected_trailer empty)
    | None -> Ok (offset, size, None)
  else Error (`Unmatched_parenthesis s)

let parse_offset s =
  let level, s = span ~sat:is_greater s in
  let rel, ss = span ~sat:is_ampersand s in
  if is_prefix ~affix:lparent ss
  then
    parse_indirect_offset ss >|= fun v ->
    (length level, `Ind ((if not (is_empty rel) then `Rel else `Abs), v))
  else
    parse_abs_or_rel s >>= fun (offset, empty) ->
    if is_empty empty
    then Ok (length level, offset)
    else Error (`Unexpected_trailer s)

let prefix ~affix s =
  match cut ~sep:affix s with
  | Some (empty, s) ->
      if is_empty empty then Ok s else Error (`No_prefix (affix, s))
  | None -> Error (`No_prefix (affix, s))

let le = v "le"

and be = v "be"

and me = v "me"

let byte = v "byte"

and long = v "long"

and quad = v "quad"

and date = v "date"

and clear = v "clear"

and short = v "short"

and ldate = v "ldate"

and regex = v "regex"

and qdate = v "qdate"

and float = v "float"

and double = v "double"

and qldate = v "qldate"

and qwdate = v "qwdate"

and search = v "search"

and string = v "string"

and pstring = v "pstring"

and default = v "default"

and indirect = v "indirect"

let _16 = v "16"

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

type default = [ `Default ]

type regex = [ `Regex ]

let parse_type s =
  let unsigned, s =
    if is_prefix ~affix:u s then (true, tail s) else (false, s) in
  let is_le = is_prefix ~affix:le s in
  let is_be = is_prefix ~affix:be s in
  let is_me = is_prefix ~affix:me s in
  let s = if is_le || is_be || is_me then (tail <.> tail) s else s in
  let res =
    match String.sub (to_string s) 0 4 with
    | "clea" -> prefix ~affix:clear s >|= fun s -> (`Clear, s)
    | "byte" -> prefix ~affix:byte s >|= fun s -> (`Byte, s)
    | "shor" -> prefix ~affix:short s >|= fun s -> (`Short, s)
    | "long" -> prefix ~affix:long s >|= fun s -> (`Long, s)
    | "quad" -> prefix ~affix:quad s >|= fun s -> (`Quad, s)
    | "doub" -> prefix ~affix:double s >|= fun s -> (`Double, s)
    | "floa" -> prefix ~affix:float s >|= fun s -> (`Float, s)
    | "date" -> prefix ~affix:date s >|= fun s -> (`Date, s)
    | "ldat" -> prefix ~affix:ldate s >|= fun s -> (`Ldate, s)
    | "qdat" -> prefix ~affix:qdate s >|= fun s -> (`Qdate, s)
    | "qlda" -> prefix ~affix:qldate s >|= fun s -> (`Qldate, s)
    | "qwda" -> prefix ~affix:qwdate s >|= fun s -> (`Qwdate, s)
    | "rege" -> prefix ~affix:regex s >|= fun s -> (`Regex, s)
    | "sear" -> prefix ~affix:search s >|= fun s -> (`Search, s)
    | "stri" ->
        prefix ~affix:string s >>= fun s ->
        let string16 = is_prefix ~affix:_16 s in
        if string16
        then prefix ~affix:_16 s >|= fun s -> (`String16, s)
        else Ok (`String, s)
    | "pstr" -> prefix ~affix:pstring s >|= fun s -> (`Pstring, s)
    | "defa" -> prefix ~affix:default s >|= fun s -> (`Default, s)
    | "indi" -> prefix ~affix:indirect s >|= fun s -> (`Indirect, s)
    | _ -> Error (`Invalid_type s)
    | exception _ -> Error (`Invalid_type s) in
  let endian =
    match (is_le, is_be, is_me) with
    | true, false, false -> Some `LE
    | false, true, false -> Some `BE
    | false, false, true -> Some `ME
    | false, false, false -> None
    | _ -> assert false (* XXX(dinosaure): should never occur! *) in
  res >>= fun (kind, s) ->
  match kind with
  | `Clear -> Ok (unsigned, `Clear)
  | `Indirect -> (
      match cut ~sep:slash s with
      | None -> Ok (unsigned, `Indirect false)
      | Some (empty, s) ->
          if is_empty empty
          then
            let has_r = exists is_r s in
            Ok (unsigned, `Indirect has_r)
          else Error (`Unexpected_trailer empty))
  | #default -> Ok (unsigned, `Default)
  | #regex -> (
      match cut ~sep:slash s with
      | None -> Ok (unsigned, `Regex None)
      | Some (empty, s) ->
          if is_empty empty
          then
            match cut ~sep:slash s with
            | Some (a, b) ->
                let limit, flags =
                  if for_all is_digit a then (a, b) else (b, a) in
                let has_c = exists is_c flags in
                let has_s = exists is_s flags in
                let has_l = exists is_l flags in
                Integer.parse limit >>= fun (limit, _empty) ->
                Ok (unsigned, `Regex (Some (has_c, has_s, has_l, limit)))
            | None ->
                if for_all is_digit s
                then
                  Integer.parse s >>= fun (limit, _empty) ->
                  Ok (unsigned, `Regex (Some (false, false, false, limit)))
                else
                  let has_c = exists is_c s in
                  let has_s = exists is_s s in
                  let has_l = exists is_l s in
                  Ok (unsigned, `Regex (Some (has_c, has_s, has_l, 8192L)))
          else Error (`Unexpected_trailer empty))
  | `String | `Search -> (
      match cut ~sep:slash s with
      | None -> Ok (unsigned, `Search None)
      | Some (empty, s) ->
          if is_empty empty
          then
            match cut ~sep:slash s with
            | Some (a, b) ->
                let limit, flags =
                  match Integer.parse a with Ok _ -> (a, b) | Error _ -> (b, a)
                in
                let v = [] in
                let v = if exists is_b flags then `b :: v else v in
                let v = if exists is_B flags then `B :: v else v in
                let v = if exists is_c flags then `c :: v else v in
                let v = if exists is_C flags then `C :: v else v in
                let v = if exists is_t flags then `t :: v else v in
                let v = if exists is_W flags then `W :: v else v in
                let v = if exists is_w flags then `w :: v else v in
                let v = if exists is_T flags then `T :: v else v in
                Integer.parse limit >>= fun (limit, empty) ->
                if is_empty empty
                then Ok (unsigned, `Search (Some (v, Some limit)))
                else Error (`Unexpected_trailer empty)
            | None ->
            match Integer.parse s with
            | Ok (limit, empty) ->
                if is_empty empty
                then Ok (unsigned, `Search (Some ([], Some limit)))
                else Error (`Unexpected_trailer empty)
            | Error (`Empty | `Invalid_integer _) ->
                let v = [] in
                let v = if exists is_b s then `b :: v else v in
                let v = if exists is_B s then `B :: v else v in
                let v = if exists is_c s then `c :: v else v in
                let v = if exists is_C s then `C :: v else v in
                let v = if exists is_t s then `t :: v else v in
                let v = if exists is_W s then `W :: v else v in
                let v = if exists is_w s then `w :: v else v in
                let v = if exists is_T s then `T :: v else v in
                Ok (unsigned, `Search (Some (v, None)))
          else Error (`Unexpected_trailer empty))
  | `Pstring -> (
      match cut ~sep:slash s with
      | None -> Ok (unsigned, `String8 None)
      | Some (empty, s) ->
          if is_empty empty
          then
            let has_b = exists is_b s in
            let has_B = exists is_B s in
            let has_c = exists is_c s in
            let has_C = exists is_C s in
            Ok (unsigned, `String8 (Some (has_b, has_B, has_c, has_C)))
          else Error (`Unexpected_trailer empty))
  | `String16 -> (
      match endian with
      | Some ((`LE | `BE) as endian) -> Ok (unsigned, `String16 endian)
      | _ -> Error `Unsupported_type)
  | #numeric as numeric -> (
      let arithmetic, s =
        let arithmetic, s = span ~min:1 ~max:1 ~sat:Arithmetic.is s in
        if length arithmetic = 1
        then (Some (to_string arithmetic), s)
        else (None, s) in
      match arithmetic with
      | Some arithmetic ->
          Integer.parse s >>= fun (with_val, empty) ->
          if is_empty empty
          then
            Ok
              ( unsigned,
                `Numeric
                  ( endian,
                    numeric,
                    Some (Arithmetic.of_string ~with_val arithmetic) ) )
          else Error (`Unexpected_trailer empty)
      | None -> Ok (unsigned, `Numeric (endian, numeric, None)))

let x = v "x"

let is_x s =
  let s = trim ~drop:is_wsp s in
  equal_bytes s x

let is_modifier s = match head s with
  | Some ('G' .. 'Z') | Some ('g' .. 'z') -> length s = 1
  | _ -> false

let parse_test s =
  if is_x s
  then Ok `True
  else
    let comparison, s = span ~min:1 ~max:1 ~sat:Comparison.is s in
    let comparison = if is_empty comparison then "=" else to_string comparison in
    let s = trim s in (* XXX(dinosaure): we can have [< 10], so [s = " 10"], we
                         must [trim] it. *)
    let parse_string s =
      let lexbuf = Lexing.from_string (to_string s) in
      let buf = Buffer.create (length s) in
      let contents = Lexer.string buf lexbuf in
      (* escape *)
      Ok (`String (Comparison.of_string ~with_val:contents comparison)) in
    match Number.parse s with
    | Ok (v, empty) ->
        if is_empty empty || is_modifier empty
        then Ok (`Numeric (Comparison.of_string ~with_val:(v, to_string s) comparison))
        else parse_string s
    | Error (`Invalid_number _ | `Empty) ->
        if is_empty s
        then
          Ok
            (`Numeric
              (Comparison.of_string ~with_val:(Number.int64 0L, "") comparison))
        else parse_string s

let parse_message s =
  match head s with
  | Some '\x08' -> Ok (`No_space ((to_string <.> tail) s))
  | Some '\\' -> (
      match (head <.> tail) s with
      | Some 'b' -> Ok (`No_space ((to_string <.> tail <.> tail) s))
      | _ -> Ok (`Space (to_string s)))
  | _ -> Ok (`Space (to_string s))

let parse_strength s =
  let s = trim ~drop:is_wsp s in
  let arithmetic, s = span ~min:1 ~max:1 ~sat:Arithmetic.is s in
  let s = trim ~drop:is_wsp s in
  if length arithmetic = 1
  then
    Integer.parse s >>= fun (with_val, _empty) ->
    Ok (Arithmetic.of_string ~with_val (to_string arithmetic))
  else Error `Invalid_strength

let parse_use offset s =
  let buf = Buffer.create 16 in
  let lexbuf = Lexing.from_string (to_string s) in
  let name = Lexer.string buf lexbuf in
  let empty = with_range ~first:(Lexing.lexeme_end lexbuf) s in
  if name <> "" && is_empty empty
  then
    match string_head name with
    | Some '^' -> Ok (`Use (offset, true, string_tail name))
    | _ -> Ok (`Use (offset, false, name))
  else Error `Invalid_use_command

let hws = v "\t"

let wsp = v " "

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
    | `Indirect of bool
    | `Regex of (bool * bool * bool * int64) option
    | `String16 of [ `BE | `LE ]
    | `String8 of (bool * bool * bool * bool) option
    | `Search of (search_flag list * int64 option) option ]

and search_flag = [ `t | `T | `b | `B | `c | `C | `w | `W ]

and test =
  [ `True | `Numeric of (Number.t * string) Comparison.t | `String of string Comparison.t ]

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

let escape = v "\\"

(* TODO(dinosaure): we should clear semantic of this function:
   "x\t" returns ("x", [])
*)
let best_effort s =
  match cuts ~empty:false ~sep:hws s with
  | [] -> Error (`Missing_test s)
  | [ test0 ] -> (
      match cuts ~empty:false ~sep:wsp s with
      | [] -> Ok (test0, [])
      | [ test1 ] -> if is_empty test0 then Ok (test1, []) else Ok (test0, [])
      | hd :: tl ->
        let test, message =
          List.fold_left
            (fun (test, message) elt -> match test, message with
               | [], [] -> [ elt ], []
               | hd :: _ as test, [] ->
                 if is_suffix ~affix:escape hd
                 then (elt :: test, [])
                 else (test, [ elt ])
               | _, message -> (test, elt :: message))
            ([ hd ], []) tl in
        let test = concat ~sep:wsp (List.rev test) in
        Ok (test, message))
  | test :: message ->
    Ok (test, message)

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

let parse_line line : (line, [> error ]) result =
  match string_head line with
  | None | Some '#' -> Ok `Comment
  | _ -> (
      let s = v line in
      let verb, s = span ~sat:(not <.> is_wsp) s in
      let s = drop ~sat:is_wsp s in

      match to_string verb with
      | "!:apple" -> Ok (`Apple (to_string s))
      | "!:ext" -> Ok (`Ext (to_string s))
      | "!:mime" -> Ok (`Mime (to_string s))
      | "!:strength" ->
          parse_strength s >>= fun strength -> Ok (`Strength strength)
      | _ -> (
          parse_offset verb >>= fun offset ->
          let ty, s = span ~sat:(not <.> is_wsp) s in
          let s = drop ~sat:is_wsp s in
          match to_string ty with
          | "name" -> Ok (`Name (offset, to_string s))
          | "guid" -> Ok (`Guid (offset, to_string s))
          | "use" -> parse_use offset (trim ~drop:is_wsp s)
          | _ ->
              parse_type ty >>= fun ty ->
              best_effort s >>= fun (test, message) ->
              parse_test test >>= fun test ->
              parse_message (concat ~sep:wsp message) >>= fun message ->
              Ok (`Rule (offset, ty, test, message))))

let pp_error ppf err =
  let pp = Format.fprintf in
  let to_string = Sub.to_string in

  match err with
  | `Invalid_number s -> pp ppf "Invalid number %S" (to_string s)
  | `Invalid_integer s -> pp ppf "Invalid integer %S" s
  | `Invalid_strength -> pp ppf "Invalid strength"
  | `Invalid_type s -> pp ppf "Invalid type %S" (to_string s)
  | `No_prefix (prefix, s) ->
      pp ppf "Expected prefix %S on %S" (to_string prefix) (to_string s)
  | `Unsupported_type -> pp ppf "Unsupported type"
  | `Unmatched_parenthesis s -> pp ppf "Unmatched parenthesis %S" (to_string s)
  | `Unexpected_trailer s -> pp ppf "Unexpected_trailer %S" (to_string s)
  | `Missing_test s -> pp ppf "Missing test %S" (to_string s)
  | `Empty -> pp ppf "Empty string"
  | `Invalid_use_command -> pp ppf "Invalid #use command"

let parse_in_channel ic =
  let rec go acc =
    match input_line ic with
    | line -> parse_line line >>= fun v -> go (v :: acc)
    | exception End_of_file -> Ok (List.rev acc) in
  go []
