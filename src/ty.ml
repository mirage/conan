type default = Default
type clear = Clear

type unsigned = { unsigned : bool }
type endian = [ `BE | `LE | `ME | `NE ]

type ('test, 'v) t =
  | Default : (default, default) t
  | Regex : { case_insensitive : bool
            ; start : bool
            ; limit : int64 (* default: 8KiB *)
            ; kind : [ `Byte | `Line ] } -> (Re.t, string) t
  | Clear : (clear, clear) t
  | Search : { compact_whitespaces : bool
             ; optional_blank : bool
             ; lower_case_insensitive : bool
             ; upper_case_insensitive : bool
             ; text : bool
             ; binary : bool
             ; trim : bool
             ; range : int64
             ; pattern : string } -> (string, string) t
  | Pascal_string (* uh?! *) : (string, string) t
  | Unicode : [ `BE | `LE ] -> (Uchar.t, string) t
  | Byte   : unsigned * char Arithmetic.t -> (char, char) t
  | Short  : unsigned * int Arithmetic.t
             * [ `BE | `LE | `NE ]  -> (int, int) t
  | Long   : unsigned * int32 Arithmetic.t
             * endian -> (int32, int32) t
  | Quad   : unsigned * int64 Arithmetic.t
             * endian -> (int64, int64) t
  | Float  : unsigned * float Arithmetic.t
             * endian -> (float, float) t
  | Double : unsigned * float Arithmetic.t
             * endian -> (float, float) t
  | Indirect : [ `Rel | `Abs ] -> ('test, 'v) t

let pf = Format.fprintf

let pp_unsigned ppf { unsigned } =
  if unsigned then pf ppf "u"

let pp_flag letter ppf = function
  | true -> pf ppf "%c" letter | false -> ()
let pp_int ppf = pf ppf "0x%x"
let pp_int32 ppf = pf ppf "0x%lx"
let pp_int64 ppf = pf ppf "0x%Lx"
let pp_float ppf = pf ppf "%f"

let pp_endian ppf = function
  | `BE -> pf ppf "be"
  | `LE -> pf ppf "le"
  | `ME -> pf ppf "me"
  | `NE -> pf ppf "ne"

let pp
  : type test v. Format.formatter -> (test, v) t -> unit
  = fun ppf -> function
    | Default -> pf ppf "default"
    | Clear -> pf ppf "clear"
    | Regex { case_insensitive= false
            ; start= false
            ; limit= 8192L
            ; kind= `Byte } ->
      pf ppf "regex"
    | Regex { case_insensitive; start; limit; kind; } ->
      let line = match kind with `Byte -> false | `Line -> true in
      pf ppf "regex/%a%a%a/%Ld"
        (pp_flag 'c') case_insensitive
        (pp_flag 's') start
        (pp_flag 'l') line
        limit
    | Search { compact_whitespaces= false
             ; optional_blank= false
             ; lower_case_insensitive= false
             ; upper_case_insensitive= false
             ; text= false
             ; binary= false
             ; trim= false
             ; range= 0L
             ; pattern= "" } ->
      pf ppf "search"
    | Search { compact_whitespaces
             ; optional_blank
             ; lower_case_insensitive
             ; upper_case_insensitive
             ; text
             ; binary
             ; trim
             ; range
             ; pattern= _ } ->
      pf ppf "search/%a%a%a%a%a%a%a/%Ld"
        (pp_flag 'W') compact_whitespaces
        (pp_flag 'w') optional_blank
        (pp_flag 'c') lower_case_insensitive
        (pp_flag 'C') upper_case_insensitive
        (pp_flag 'b') binary
        (pp_flag 't') text
        (pp_flag 'T') trim
        range
    | Pascal_string -> pf ppf "pstring"
    | Unicode `BE -> pf ppf "bestring16"
    | Unicode `LE -> pf ppf "lestring16"
    | Byte (unsigned, arithmetic)->
      let pp_byte ppf v = pf ppf "%02x" (Char.code v) in
      pf ppf "%abyte%a" pp_unsigned unsigned
        (Arithmetic.pp pp_byte) arithmetic
    | Short (unsigned, arithmetic, endian) ->
      pf ppf "%a%ashort%a" pp_unsigned unsigned pp_endian endian
        (Arithmetic.pp pp_int) arithmetic
    | Long (unsigned, arithmetic, endian) ->
      pf ppf "%a%along%a" pp_unsigned unsigned pp_endian endian
        (Arithmetic.pp pp_int32) arithmetic
    | Quad (unsigned, arithmetic, endian) ->
      pf ppf "%a%aquad%a" pp_unsigned unsigned pp_endian endian
        (Arithmetic.pp pp_int64) arithmetic
    | Float (unsigned, arithmetic, endian) ->
      pf ppf "%a%afloat%a" pp_unsigned unsigned pp_endian endian
        (Arithmetic.pp pp_float) arithmetic
    | Double (unsigned, arithmetic, endian) ->
      pf ppf "%a%adouble%a" pp_unsigned unsigned pp_endian endian
        (Arithmetic.pp pp_float) arithmetic
    | Indirect `Rel ->
      pf ppf "indirect/r"
    | Indirect `Abs ->
      pf ppf "indirect"

let default : (default, default) t = Default
let clear : (clear, clear) t = Clear

let regex
    ?(case_insensitive= false)
    ?(start= false)
    ?(limit= 8192L) kind =
  Regex { case_insensitive
        ; start
        ; limit
        ; kind }

let pascal_string = Pascal_string

let search
    ?(compact_whitespaces= false)
    ?(optional_blank= false)
    ?(lower_case_insensitive= false)
    ?(upper_case_insensitive= false)
    kind
    ?(trim= false)
    range ~pattern =
  if Int64.of_int (String.length pattern) > range
  then invalid_arg "Pattern can not be larger than the range" ;
  let text, binary = match kind with
    | `Text -> true, false
    | `Binary -> false, true in
  Search { compact_whitespaces
         ; optional_blank
         ; lower_case_insensitive
         ; upper_case_insensitive
         ; text
         ; binary
         ; trim
         ; range
         ; pattern }

let with_range range = function
  | Search v -> Search { v with range }
  | t -> t

let with_pattern pattern = function
  | Search v -> Search { v with pattern }
  | t -> t

let unicode endian = Unicode endian

let system_endian =
  if Sys.big_endian
  then `BE else `LE

let numeric
  : type w. ?unsigned:bool -> ?endian:endian -> w Integer.t -> w Arithmetic.t -> (w, w) t
  = fun ?(unsigned= false) ?(endian= system_endian) w a -> match w with
  | Integer.Byte -> Byte ({ unsigned }, a)
  | Integer.Short ->
    let endian = match endian with
      | `BE -> `BE
      | `LE -> `LE
      | `NE -> `NE
      | _ -> invalid_arg "Invalid endian for short" in
    Short ({ unsigned }, a, endian)
  | Integer.Int32 ->
    Long ({ unsigned }, a, endian)
  | Integer.Int64 ->
    Quad ({ unsigned }, a, endian)

let float ?(unsigned= false) ?(endian= system_endian) c =
  Float ({ unsigned }, c, endian)

let double ?(unsigned= false) ?(endian= system_endian) c =
  Double ({ unsigned }, c, endian)

let indirect v = Indirect v

open Sigs

let ( <.> ) f g = fun x -> f (g x)

let newline = "\n" (* TODO: windows *)

let ok x = Ok x

let reword_error f = function
  | Ok x -> Ok x
  | Error err -> Error (f err)

let id x = x

let process_numeric
  : type test v.
    (test, v) t -> Size.t * (int64 -> v) * v Integer.t
  = function
    | Byte _ -> Size.byte, (Char.chr <.> Int64.to_int), Integer.byte
    | Short (_, _, `LE) -> Size.leshort, Int64.to_int, Integer.short
    | Short (_, _, `BE) -> Size.beshort, Int64.to_int, Integer.short
    | Short (_, _, `NE) -> Size.short, Int64.to_int, Integer.short
    | Long (_, _, `LE) -> Size.lelong, Int64.to_int32, Integer.int32
    | Long (_, _, `BE) -> Size.belong, Int64.to_int32, Integer.int32
    | Long (_, _, `NE) -> Size.long, Int64.to_int32, Integer.int32
    | Quad (_, _, `LE) -> Size.lequad, id, Integer.int64
    | Quad (_, _, `BE) -> Size.bequad, id, Integer.int64
    | Quad (_, _, `NE) -> Size.quad, id, Integer.int64
    | _ ->
      assert false

let read_float _scheduler _syscall _fd _endian = assert false
let read_double _scheduler _syscall _fd _endian = assert false

let process
  : type s fd error test v.
     s scheduler
  -> (fd, error, s) syscall
  -> fd
  -> int64
  -> (test, v) t
  -> ((v, [> `Syscall of error
          |  `Not_found ]) result, s) io
  = fun ({ bind; return; } as scheduler) syscall fd abs_offset ty ->
    let ( >>= ) = bind in
    let ( >?= ) x f = x >>= function
      | Ok x -> f x
      | Error err -> return (Error err) in
    let ( >|= ) x f = x >>= fun x -> return (f x) in
    match ty with
    | Default -> (return <.> ok) (Default : default)
    | Clear   -> (return <.> ok) (Clear : clear)
    | Search { pattern= ""; _ } -> (return <.> ok) ""
    | Search { pattern
             ; compact_whitespaces= _
             ; optional_blank= _
             ; lower_case_insensitive= _
             ; upper_case_insensitive= _
             ; text= _
             ; binary= _
             ; trim= _
             ; range } ->
      let get fd ~pos =
        syscall.seek fd (Int64.add abs_offset pos) SET >?= fun () ->
        syscall.read_int8 fd >?= fun code ->
        (return <.> ok <.> Char.chr) code in
      Bm.find_all scheduler ~get ~ln:range fd ~pattern >|=
      reword_error (fun err -> `Syscall err) >?= fun results ->
      let results = List.sort Int64.compare results in
      ( match results with
        | [] -> return (Error `Not_found)
        | rel_offset :: _ ->
          syscall.seek fd (Int64.add abs_offset rel_offset) SET >|=
          reword_error (fun err -> `Syscall err) >?= fun () ->
          syscall.read fd (String.length pattern) >|=
          reword_error (fun err -> `Syscall err))
    | Regex { kind; limit; _ } ->
      ( match kind with
        | `Byte ->
          syscall.read fd (Int64.to_int limit) >|=
          reword_error (fun err -> `Syscall err)
        | `Line ->
          let rec go acc = function
            | 0 -> (return
                    <.> ok
                    <.> String.concat newline
                    <.> List.rev) acc
            | n -> syscall.line fd >|=
              reword_error (fun err -> `Syscall err) >?= fun line ->
                go (line :: acc) (pred n) in
          go [] (max 0 (Int64.to_int limit)) )
    | Byte ({ unsigned }, c) ->
      let size, converter, w = process_numeric ty in
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      Size.read scheduler syscall fd size >?=
      (return <.> ok <.> converter) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process ~unsigned w v c)
    | Short ({ unsigned }, c, _) ->
      let size, converter, w = process_numeric ty in
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      Size.read scheduler syscall fd size >?=
      (return <.> ok <.> converter) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process ~unsigned w v c)
    | Long ({ unsigned }, c, _) ->
      let size, converter, w = process_numeric ty in
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      Size.read scheduler syscall fd size >?=
      (return <.> ok <.> converter) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process ~unsigned w v c)
    | Quad ({ unsigned }, c, _) ->
      let size, converter, w = process_numeric ty in
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      Size.read scheduler syscall fd size >?=
      (return <.> ok <.> converter) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process ~unsigned w v c)
    | Float ({ unsigned= _ }, c, endian) ->
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read_float scheduler syscall fd endian >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process_float v c)
    | Double ({ unsigned= _ }, c, endian) ->
      syscall.seek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read_double scheduler syscall fd endian >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (Arithmetic.process_float v c)
    | _ -> assert false
