type default = Default
type clear = Clear
type unsigned = { unsigned : bool }
type endian = [ `BE | `LE | `ME | `NE ]

type ('test, 'v) t =
  | Default : (default, default) t
  | Offset : (int64, int64) t
  | Regex : {
      case_insensitive : bool;
      start : bool;
      limit : int64;
      (* default: 8KiB *)
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
  | Pascal_string (* uh?! *) : (string, string) t
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

let serialize_endian ppf = function
  | `BE -> Format.pp_print_string ppf "`BE"
  | `LE -> Format.pp_print_string ppf "`LE"
  | `ME -> Format.pp_print_string ppf "`ME"
  | `NE -> Format.pp_print_string ppf "`NE"

let serialize : type test v. Format.formatter -> (test, v) t -> unit =
 fun ppf -> function
  | Default -> Format.pp_print_string ppf "Conan.Ty.default"
  | Offset -> Format.pp_print_string ppf "Conan.Ty.offset"
  | Regex { case_insensitive; start; limit; kind } ->
      let serialize_kind ppf = function
        | `Byte -> Format.fprintf ppf "`Byte"
        | `Line -> Format.fprintf ppf "`Line"
      in
      Format.fprintf ppf
        "@[<2>Conan.Ty.regex@ ~case_insensitive:%b ~start:%b@ ~limit:%LdL@ %a@]"
        case_insensitive start limit serialize_kind kind
  | Clear -> Format.pp_print_string ppf "Conan.Ty.clear"
  | Search
      {
        compact_whitespaces;
        optional_blank;
        lower_case_insensitive;
        upper_case_insensitive;
        text;
        binary;
        trim;
        range;
        pattern;
        find = _;
      } ->
      let serialize_type ppf () =
        match (text, binary) with
        | true, false -> Format.pp_print_string ppf "`Text"
        | false, true -> Format.pp_print_string ppf "`Binary"
        | _ -> assert false
        (* XXX(dinosaure): should never occur! *)
      in
      Format.fprintf ppf
        "@[<2>Conan.Ty.search@ ~compact_whitespaces:%b@ ~optional_blank:%b@ \
         ~lower_case_insensitive:%b@ ~upper_case_insensitive:%b@ @[%a@]@ \
         ~trim:%b@ %LdL@ ~pattern:%S@]"
        compact_whitespaces optional_blank lower_case_insensitive
        upper_case_insensitive serialize_type () trim range pattern
  | Pascal_string -> Format.pp_print_string ppf "Conan.Ty.pascal_string"
  | Unicode_string `BE -> Format.fprintf ppf "@[<2>Conan.Ty.str_unicode@ `BE@]"
  | Unicode_string `LE -> Format.fprintf ppf "@[<2>Conan.Ty.str_unicode@ `LE@]"
  | Byte ({ unsigned }, arithmetic) ->
      Format.fprintf ppf
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ Conan.Integer.byte@ @[%a@]@]"
        unsigned
        Serialize.(parens (Arithmetic.serialize char))
        arithmetic
  | Short ({ unsigned }, arithmetic, endian) ->
      let serialize_endian ppf = function
        | `BE -> Format.pp_print_string ppf "`BE"
        | `LE -> Format.pp_print_string ppf "`LE"
        | `NE -> Format.pp_print_string ppf "`NE"
      in
      Format.fprintf ppf
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.short \
         @[%a@]@]"
        unsigned serialize_endian endian
        Serialize.(parens (Arithmetic.serialize int))
        arithmetic
  | Long ({ unsigned }, arithmetic, endian) ->
      Format.fprintf ppf
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.int32 \
         @[%a@]@]"
        unsigned serialize_endian endian
        Serialize.(parens (Arithmetic.serialize int32))
        arithmetic
  | Quad ({ unsigned }, arithmetic, endian) ->
      Format.fprintf ppf
        "@[<2>Conan.Ty.numeric@ ~unsigned:%b@ ~endian:%a@ Conan.Integer.int64 \
         @[%a@]@]"
        unsigned serialize_endian endian
        Serialize.(parens (Arithmetic.serialize int64))
        arithmetic
  | Float ({ unsigned }, arithmetic, endian) ->
      Format.fprintf ppf
        "@[<2>Conan.Ty.float@ ~unsigned:%b@ ~endian:%a@ @[%a@]@]" unsigned
        serialize_endian endian
        Serialize.(parens (Arithmetic.serialize float))
        arithmetic
  | Double ({ unsigned }, arithmetic, endian) ->
      Format.fprintf ppf
        "@[<2>Conan.Ty.double@ ~unsigned:%b@ ~endian:%a@ @[%a@]@]" unsigned
        serialize_endian endian
        Serialize.(parens (Arithmetic.serialize float))
        arithmetic
  | Indirect `Rel -> Format.fprintf ppf "@[<2>Conan.Ty.indirect@ `Rel@]"
  | Indirect `Abs -> Format.fprintf ppf "@[<2>Conan.Ty.indirect@ `Abs@]"
  | Date (zone, size, arithmetic, endian) ->
      let serialize_type ppf () =
        match (zone, size) with
        | `UTC, `s32 -> Format.pp_print_string ppf "`Date"
        | `Local, `s32 -> Format.pp_print_string ppf "`Ldate"
        | `UTC, `s64 -> Format.pp_print_string ppf "`Qdate"
        | `Local, `s64 -> Format.pp_print_string ppf "`Qldate"
        | `Window, `s64 -> Format.pp_print_string ppf "`Qwdate"
        | `Window, `s32 -> assert false
        (* XXX(dinosaure): should never occur! *)
      in
      Format.fprintf ppf
        "@[<2>Conan.Ty.date@ @[%a@]@ @[<1>(Some@ %a)@]@ @[%a@]@]" serialize_type
        () serialize_endian endian
        Serialize.(parens (Arithmetic.serialize (parens ptime_span)))
        arithmetic

let pf = Format.fprintf
let pp_unsigned ppf { unsigned } = if unsigned then pf ppf "u"
let pp_flag letter ppf = function true -> pf ppf "%c" letter | false -> ()
let pp_int ppf = pf ppf "0x%x"
let pp_int32 ppf = pf ppf "0x%lx"
let pp_int64 ppf = pf ppf "0x%Lx"

let pp_ptime ppf v =
  match Ptime.Span.to_int_s v with
  | Some v -> pf ppf "%d" v
  | None -> pf ppf "%.0f" (Ptime.Span.to_float_s v)

let pp_float ppf = pf ppf "%f"

let pp_endian ppf = function
  | `BE -> pf ppf "be"
  | `LE -> pf ppf "le"
  | `ME -> pf ppf "me"
  | `NE -> pf ppf "ne"

let pp_date_size ppf = function `s32 -> () | `s64 -> pf ppf "q"

let pp_of_result : type test v. (test, v) t -> Format.formatter -> v -> unit =
  function
  | Default -> fun ppf Default -> Format.fprintf ppf "x"
  | Offset -> fun ppf v -> Format.fprintf ppf "%Ld" v
  | Regex _ ->
      fun ppf ropes ->
        let str, off, len = Ropes.to_string ropes in
        Format.fprintf ppf "%S" (String.sub str off len)
  | Clear -> fun ppf Clear -> Format.fprintf ppf "clear"
  | Search _ -> fun ppf v -> Format.fprintf ppf "%S" v
  | Pascal_string -> fun ppf v -> Format.fprintf ppf "%S" v
  | Unicode_string _ -> fun ppf v -> Format.fprintf ppf "%S" v
  | Byte _ -> fun ppf chr -> Format.fprintf ppf "%S" (String.make 1 chr)
  | Short _ -> fun ppf v -> Format.fprintf ppf "%d" v
  | Long _ -> fun ppf v -> Format.fprintf ppf "%ld" v
  | Quad _ -> fun ppf v -> Format.fprintf ppf "%Ld" v
  | Float _ -> fun ppf v -> Format.fprintf ppf "%f" v
  | Double _ -> fun ppf v -> Format.fprintf ppf "%f" v
  | Indirect _ -> fun ppf _ -> Format.fprintf ppf "#indirection"
  | Date _ -> fun ppf v -> Format.fprintf ppf "%S" v

let pp : type test v. Format.formatter -> (test, v) t -> unit =
 fun ppf -> function
  | Default -> pf ppf "default"
  | Clear -> pf ppf "clear"
  | Offset -> pf ppf "offset"
  | Regex
      { case_insensitive = false; start = false; limit = 8192L; kind = `Byte }
    ->
      pf ppf "regex"
  | Regex { case_insensitive; start; limit; kind } ->
      let line = match kind with `Byte -> false | `Line -> true in
      pf ppf "regex/%a%a%a/%Ld" (pp_flag 'c') case_insensitive (pp_flag 's')
        start (pp_flag 'l') line limit
  | Search
      {
        compact_whitespaces = false;
        optional_blank = false;
        lower_case_insensitive = false;
        upper_case_insensitive = false;
        text = false;
        binary = false;
        trim = false;
        range = 0L;
        pattern = "";
        find = _;
      } ->
      pf ppf "search"
  | Search
      {
        compact_whitespaces;
        optional_blank;
        lower_case_insensitive;
        upper_case_insensitive;
        text;
        binary;
        trim;
        range;
        pattern = _;
        find = _;
      } ->
      pf ppf "search/%a%a%a%a%a%a%a/%Ld" (pp_flag 'W') compact_whitespaces
        (pp_flag 'w') optional_blank (pp_flag 'c') lower_case_insensitive
        (pp_flag 'C') upper_case_insensitive (pp_flag 'b') binary (pp_flag 't')
        text (pp_flag 'T') trim range
  | Pascal_string -> pf ppf "pstring"
  | Unicode_string `BE -> pf ppf "bestring16"
  | Unicode_string `LE -> pf ppf "lestring16"
  | Byte (unsigned, arithmetic) ->
      let pp_byte ppf v = pf ppf "%02x" (Char.code v) in
      pf ppf "%abyte%a" pp_unsigned unsigned (Arithmetic.pp pp_byte) arithmetic
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
  | Indirect `Rel -> pf ppf "indirect/r"
  | Indirect `Abs -> pf ppf "indirect"
  | Date (`Local, size, arithmetic, endian) ->
      pf ppf "%a%aldate%a" pp_endian endian pp_date_size size
        (Arithmetic.pp pp_ptime) arithmetic
  | Date (`UTC, size, arithmetic, endian) ->
      pf ppf "%a%adate%a" pp_endian endian pp_date_size size
        (Arithmetic.pp pp_ptime) arithmetic
  | Date (`Window, size, arithmetic, endian) ->
      pf ppf "%a%awdate%a" pp_endian endian pp_date_size size
        (Arithmetic.pp pp_ptime) arithmetic

let default : (default, default) t = Default
let offset : (int64, int64) t = Offset
let clear : (clear, clear) t = Clear

let regex ?(case_insensitive = false) ?(start = false) ?(limit = 8192L) kind =
  Regex { case_insensitive; start; limit; kind }

let pascal_string = Pascal_string

let search ?(compact_whitespaces = false) ?(optional_blank = false)
    ?(lower_case_insensitive = false) ?(upper_case_insensitive = false) kind
    ?(trim = false) range ~pattern =
  if Int64.of_int (String.length pattern) > range then
    invalid_arg "Pattern can not be larger than the range";
  let text, binary =
    match kind with `Text -> (true, false) | `Binary -> (false, true)
  in
  Search
    {
      compact_whitespaces;
      optional_blank;
      lower_case_insensitive;
      upper_case_insensitive;
      text;
      binary;
      trim;
      range;
      pattern;
      find = Kmp.find_one ~pattern;
    }

let with_range range = function Search v -> Search { v with range } | t -> t

let with_pattern pattern = function
  | Search v -> Search { v with pattern; find = Kmp.find_one ~pattern }
  | t -> t

let str_unicode endian = Unicode_string endian
let system_endian = if Sys.big_endian then `BE else `LE

let numeric : type w.
    ?unsigned:bool ->
    ?endian:endian ->
    w Integer.t ->
    w Arithmetic.t ->
    (w, w) t =
 fun ?(unsigned = false) ?(endian = system_endian) w a ->
  match w with
  | Integer.Byte -> Byte ({ unsigned }, a)
  | Integer.Short ->
      let endian =
        match endian with
        | `BE -> `BE
        | `LE -> `LE
        | `NE -> `NE
        | _ -> invalid_arg "Invalid endian for short"
      in
      Short ({ unsigned }, a, endian)
  | Integer.Int32 -> Long ({ unsigned }, a, endian)
  | Integer.Int64 -> Quad ({ unsigned }, a, endian)

let date kind endian a =
  let endian =
    match endian with
    | Some `BE -> `BE
    | Some `LE -> `LE
    | Some `ME -> `ME
    | None -> if Sys.big_endian then `BE else `LE
  in
  match kind with
  | `Date -> Date (`UTC, `s32, a, endian)
  | `Ldate -> Date (`Local, `s32, a, endian)
  | `Qdate -> Date (`UTC, `s64, a, endian)
  | `Qldate -> Date (`Local, `s64, a, endian)
  | `Qwdate -> Date (`Window, `s64, a, endian)

let float ?(unsigned = false) ?(endian = system_endian) c =
  Float ({ unsigned }, c, endian)

let double ?(unsigned = false) ?(endian = system_endian) c =
  Double ({ unsigned }, c, endian)

let indirect v = Indirect v

open Sigs

let ( <.> ) f g x = f (g x)
let newline = "\n" (* TODO: windows *)
let ok x = Ok x
let of_option ~error = function Some x -> Ok x | None -> Error (error ())
let reword_error f = function Ok x -> Ok x | Error err -> Error (f err)
let id x = x

let read_float ({ bind; return } as scheduler) syscall fd endian =
  let ( >>= ) = bind in
  let size =
    match endian with
    | `LE -> Size.lelong
    | `BE -> Size.belong
    | `ME -> assert false (* TODO *)
    | `NE -> Size.long
  in
  Size.read scheduler syscall fd size >>= function
  | Ok v ->
      let v = Int64.to_int32 v in
      (* safe *)
      return (Ok (Int32.float_of_bits v))
  | Error _ as err -> return err

let read_double ({ bind; return } as scheduler) syscall fd endian =
  let ( >>= ) = bind in
  let size =
    match endian with
    | `LE -> Size.lequad
    | `BE -> Size.bequad
    | `ME -> assert false (* TODO *)
    | `NE -> Size.quad
  in
  Size.read scheduler syscall fd size >>= function
  | Ok v -> return (Ok (Int64.float_of_bits v))
  | Error _ as err -> return err

let process : type s fd error test v.
    s scheduler ->
    (fd, error, s) syscall ->
    fd ->
    int64 ->
    (test, v) t ->
    ((v, [> `Syscall of error | `Invalid_date | `Not_found ]) result, s) io =
 fun ({ bind; return } as scheduler) syscall fd abs_offset ty ->
  let ( >>= ) = bind in
  let ( >?= ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)
  in
  let ( >|= ) x f = x >>= fun x -> return (f x) in
  match ty with
  | Default -> (return <.> ok) (Default : default)
  | Offset -> (return <.> ok) abs_offset
  | Clear -> (return <.> ok) (Clear : clear)
  | Search { pattern = ""; _ } -> (return <.> ok) ""
  | Search
      {
        pattern;
        compact_whitespaces = _;
        optional_blank = _;
        lower_case_insensitive = _;
        upper_case_insensitive = _;
        text = _;
        binary = _;
        trim = _;
        range;
        find;
      } -> (
      let get fd ~pos =
        syscall.seek fd (Int64.add abs_offset pos) SET >?= fun () ->
        syscall.read_int8 fd >?= fun code -> (return <.> ok <.> Char.chr) code
      in
      find.Kmp.f scheduler ~get ~ln:range fd
      >|= reword_error (fun err -> `Syscall err)
      >?= fun results ->
      let results = List.sort Int64.compare results in
      match results with
      | [] -> return (Error `Not_found)
      | rel_offset :: _ ->
          let len_pattern = Int64.of_int (String.length pattern) in
          syscall.seek fd
            Int64.(add (add abs_offset rel_offset) len_pattern)
            SET
          >|= reword_error (fun err -> `Syscall err)
          (* TODO(dinosaure): we should return the pattern as-is into the document
             and not the pattern from the database. *)
          >?= fun () -> return (Ok pattern))
  | Regex { kind; limit; _ } -> (
      match kind with
      | `Byte ->
          syscall.read fd (Int64.to_int limit)
          >|= reword_error (fun err -> `Syscall err)
          >?= fun raw -> return (Ok (Ropes.of_string raw))
      | `Line ->
          let rec go acc = function
            | 0 ->
                let ropes =
                  Ropes.append
                    (Ropes.concat ~sep:newline acc)
                    (Ropes.of_string newline)
                in
                return (Ok ropes)
            | n ->
                syscall.line fd >|= reword_error (fun err -> `Syscall err)
                >?= fun (off, len, line) ->
                go (Ropes.of_string ~off ~len line :: acc) (pred n)
          in
          go [] (max 0 (Int64.to_int limit)))
  | Byte ({ unsigned }, c) ->
      let size, converter, w =
        (Size.byte, Char.chr <.> Int64.to_int, Integer.byte)
      in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >?= (return <.> ok <.> converter)
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process ~unsigned w v c)
  | Short ({ unsigned }, c, endian) ->
      let size, converter, w =
        match endian with
        | `LE -> (Size.leshort, Int64.to_int, Integer.short)
        | `BE -> (Size.beshort, Int64.to_int, Integer.short)
        | `NE -> (Size.short, Int64.to_int, Integer.short)
      in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >?= (return <.> ok <.> converter)
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process ~unsigned w v c)
  | Long ({ unsigned }, c, endian) ->
      let size, converter, w =
        match endian with
        | `LE -> (Size.lelong, Int64.to_int32, Integer.int32)
        | `BE -> (Size.belong, Int64.to_int32, Integer.int32)
        | `NE -> (Size.long, Int64.to_int32, Integer.int32)
        | `ME -> failwith "Middle-endian not supported"
      in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >?= (return <.> ok <.> converter)
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process ~unsigned w v c)
  | Quad ({ unsigned }, c, endian) ->
      let size, converter, w =
        match endian with
        | `LE -> (Size.lequad, id, Integer.int64)
        | `BE -> (Size.bequad, id, Integer.int64)
        | `NE -> (Size.quad, id, Integer.int64)
        | `ME -> failwith "Middle-endian not supported"
      in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >?= (return <.> ok <.> converter)
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process ~unsigned w v c)
  | Float ({ unsigned = _ }, c, endian) ->
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      read_float scheduler syscall fd endian
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process_float v c)
  | Double ({ unsigned = _ }, c, endian) ->
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      read_double scheduler syscall fd endian
      >|= reword_error (fun err -> `Syscall err)
      >?= fun v -> (return <.> ok) (Arithmetic.process_float v c)
  | Date (_, `s32, c, endian) -> (
      let size =
        match endian with
        | `BE -> Size.belong
        | `LE -> Size.lelong
        | `NE -> Size.long
        | `ME -> assert false
      in
      let error () = `Invalid_date in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >|= reword_error (fun err -> `Syscall err)
      >?= (return <.> of_option ~error <.> Ptime.Span.of_float_s
         <.> Int64.to_float)
      >?= fun v ->
      match Ptime.of_span (Arithmetic.process_ptime v c) with
      | Some v -> return (ok (Ptime.to_rfc3339 v))
      | None -> return (Error `Invalid_date))
  | Date (_, `s64, c, endian) -> (
      let size =
        match endian with
        | `BE -> Size.bequad
        | `LE -> Size.lequad
        | `NE -> Size.quad
        | `ME -> assert false
      in
      let error () = `Invalid_date in
      syscall.seek fd abs_offset SET >|= reword_error (fun err -> `Syscall err)
      >?= fun () ->
      Size.read scheduler syscall fd size
      >|= reword_error (fun err -> `Syscall err)
      >?= (return <.> of_option ~error <.> Ptime.Span.of_float_s
         <.> Int64.to_float)
      >?= fun v ->
      match Ptime.of_span (Arithmetic.process_ptime v c) with
      | Some v -> return (ok (Ptime.to_rfc3339 v))
      | None -> return (Error `Invalid_date))
  | Unicode_string _ -> return (Ok "")
  | Pascal_string -> assert false
  | Indirect _ -> assert false
