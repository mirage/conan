type t = Int of int64 | Float of float

let int64 v = Int v

let float v = Float v

let pp ppf = function
  | Int v -> Format.fprintf ppf "%Ld" v
  | Float v -> Format.fprintf ppf "%f" v

let to_float = function Int v -> Int64.to_float v | Float v -> v

(* TODO(dinosaure): check with 32bits/64bits platform. *)
let to_int = function Int v -> Int64.to_int v | Float v -> Float.to_int v

let to_int64 = function Int v -> v | Float v -> Int64.of_float v

let to_int32 = function
  | Int v -> Int64.to_int32 v
  | Float v -> Int32.of_float v

let to_short = function
  | Int v -> Int64.to_int v land 0xffff
  | Float v -> Float.to_int v land 0xffff

let to_byte = function
  | Int v -> Char.chr (Int64.to_int v land 0xff)
  | Float v -> Char.chr (Float.to_int v land 0xff)

let parse s =
  let open Sub in
  let v = to_string s in
  let lexbuf = Lexing.from_string v in
  if is_empty s
  then Error `Empty
  else
    match Lexer.int_or_float lexbuf with
    | Ok (`Int literal) ->
        let first = Lexing.lexeme_end lexbuf in
        Ok (Int (Int64.of_string literal), with_range ~first s)
    | Ok (`Float literal) ->
        let first = Lexing.lexeme_end lexbuf in
        Ok (Float (Float.of_string literal), with_range ~first s)
    | Error `Malformed -> Error (`Invalid_number v)
    | exception _ -> Error (`Invalid_number v)
