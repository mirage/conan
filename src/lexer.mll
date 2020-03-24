{
let digit c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false (* XXX(dinosaure): should never occur! *)

let to_int lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit (Lexing.lexeme_char lexbuf i) in
    assert (v < base);
    c := (base * !c) + v
  done; !c

let store_lexeme buf lexbuf =
  Buffer.add_string buf (Lexing.lexeme lexbuf)

let store_string_char _lexbuf buf chr =
  Buffer.add_char buf chr

let store_string_utf_8_uchar _lexbuf buf uchr =
  Buffer.add_utf_8_uchar buf uchr

let add_escaped_char lexbuf buf chr = store_string_char lexbuf buf chr
let add_escaped_uchar lexbuf buf uchr = store_string_utf_8_uchar lexbuf buf uchr

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let char_for_decimal_code lexbuf i =
  let code = to_int lexbuf ~base:10 ~first:i ~last:(i + 2) in
  if code < 0 || code > 255
  then invalid_arg "Invalid character"
  else Char.chr code

let char_for_octal_code lexbuf i =
  let code = to_int lexbuf ~base:8 ~first:i ~last:(i + 2) in
  if code < 0 || code > 255
  then invalid_arg "Invalid character"
  else Char.chr code

let char_for_hexadecimal_code lexbuf i =
  Char.chr (to_int lexbuf ~base:16 ~first:i ~last:(i + 1))

let uchar_for_uchar_escape lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 and last = len - 2 in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true -> invalid_arg "Invalid unicode character"
  | false ->
    let code = to_int lexbuf ~base:16 ~first ~last in
    if Uchar.is_valid code
    then Uchar.unsafe_of_int code
    else invalid_arg "Invalid unicode character"
}

let dec_literal = [ '0'-'9' ] [ '0'-'9' '_' ]*
let hex_literal = '0' [ 'x' 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' '_' ]*
let oct_literal = '0' [ 'o' 'O' ] [ '0'-'7' ] [ '0'-'7' '_' ]*
let bin_literal = '0' [ 'b' 'B' ] [ '0'-'1' ] [ '0'-'1' '_' ]*
let int_literal = dec_literal | hex_literal | oct_literal | bin_literal

let hex_digit = [ '0'-'9' 'A'-'F' 'a'-'f' ]

let float_literal =
  [ '0'-'9' ] [ '0'-'9' '_' ]*
  ('.' [ '0'-'9' '_']*)?
  ([ 'e' 'E' ] [ '+' '-' ]? [ '0'-'9' ] [ '0'-'9' '_' ]*)?

rule int_or_float = parse
  | (('-' | '+')* int_literal) as lit { Ok (`Int lit) }
  | (('-' | '+')* float_literal) as lit { Ok (`Float lit) }
  | _ { Error `Malformed }

and int = parse
  | (('-' | '+')* int_literal) as lit { Ok lit }
  | _ { Error `Malformed }

and string buf = parse
  | '\\' ([ '\\' '\'' '\"' 'n' 't' 'b' 'r' ' ' '^' ] as chr)
      { add_escaped_char lexbuf buf (char_for_backslash chr)
      ; string buf lexbuf }
  | '\\' [ '0'-'7' ] [ '0'-'7' ] [ '0'-'7' ]
      { add_escaped_char lexbuf buf (char_for_octal_code lexbuf 1)
      ; string buf lexbuf }
  | '\\' 'o' [ '0'-'7' ] [ '0'-'7' ] [ '0'-'7' ]
      { add_escaped_char lexbuf buf (char_for_octal_code lexbuf 2)
      ; string buf lexbuf }
  | '\\' 'x' [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]
      { add_escaped_char lexbuf buf (char_for_hexadecimal_code lexbuf 2)
      ; string buf lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { add_escaped_uchar lexbuf buf (uchar_for_uchar_escape lexbuf)
      ; string buf lexbuf }
  | '\\' _ { store_lexeme buf lexbuf ; string buf lexbuf }
  | eof { Buffer.contents buf }
  | _ as chr { store_string_char lexbuf buf chr
             ; string buf lexbuf }

{

}
