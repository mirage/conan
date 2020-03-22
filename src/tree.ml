let ( <.> ) f g = fun x -> f (g x)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pf = Format.fprintf

type 'a fmt =
  { fmt : 'r. unit -> ('a -> 'r, Format.formatter, unit, 'r) format4 }

let pp_fmt ppf v =
  let CamlinternalFormatBasics.Format (_, v) = v in
  pf ppf "%s" v

type operation =
  | Rule : Offset.t * ('test, 'v) Ty.t * 'test Test.t * 'v fmt -> operation
  | Name : Offset.t * string -> operation
  | Use : Offset.t * string -> operation

let pp_operation ppf = function
  | Rule (offset, kind, test, fmt) ->
    pf ppf "%a\t%a\t%a\t%a"
      Offset.pp offset Ty.pp kind Test.pp test pp_fmt (fmt.fmt ())
  | Name (offset, name) ->
    pf ppf "%a\t%s" Offset.pp offset name
  | Use (offset, name) ->
    pf ppf "%a\t%s" Offset.pp offset name

type tree =
  | Node of (operation * tree) list
  | Done

let pp_level ppf n =
  let rec go = function
    | 0 -> ()
    | n -> pf ppf ">" ; go (pred n) in
  go (max n 0)

let pp_tree ppf tree =
  let rec go level = function
    | Done -> ()
    | Node lst ->
      let lst = List.rev lst in
      let iter (rule, tree) =
        pf ppf "%a%a\n%!" pp_level level pp_operation rule ;
        go (succ level) tree in
      List.iter iter lst in
  go 0 tree

let system_long = Size.long
let system_endian = if Sys.big_endian then `BE else `LE

let indirect_1 ?(size= system_long) v =
  let f = function
    | `Dir v -> Offset.Value v
    | `Ind v -> Offset.(Relative (Read (Value v, size))) in
  Arithmetic.map ~f v

let indirect_0 (return, (offset, size, disp)) =
  let open Offset in
  let size = Option.value ~default:system_long size in
  let offset = match disp, offset with
    | None, `Rel offset -> Relative (Value offset)
    | None, `Abs offset -> Absolute (Value offset)
    | Some disp, `Rel offset ->
      let calculation = indirect_1 ~size disp in
      Calculation (Relative (Value offset), calculation)
    | Some disp, `Abs offset ->
      let calculation = indirect_1 ~size disp in
      Calculation (Absolute (Value offset), calculation) in
  match return with
  | `Rel -> Relative (Read (offset, size))
  | `Abs -> Absolute (Read (offset, size))

let offset = function
  | `Abs offset -> Offset.(Absolute (Value offset))
  | `Rel offset -> Offset.(Relative (Value offset))
  | `Ind ind -> indirect_0 ind

type k = Kind : ('test, 'v) Ty.t -> k
type t = Test : 'test Test.t -> t
type f = Format : 'v fmt -> f

let identity x = x

let calculation
  :  cast:(int64 -> 'v)
  -> int64 Arithmetic.t option
  -> 'v Arithmetic.t = fun ~cast:f -> function
  | None -> Arithmetic.add (f 0L)
  | Some c -> Arithmetic.map ~f c

let force_to_use_string_formatter message =
  match Astring.String.cut ~sep:"%" message with
  | None -> None
  | Some (x, r) ->
    if r = "" then None
    else match r.[0] with
      | _ ->
        let r = Bytes.of_string r in
        Bytes.set r 0 's' ; Some (x ^ "%" ^ (Bytes.to_string r))

let prepend_percent message =
  match Astring.String.cut ~sep:"%" message with
  | None -> message
  | Some (a, b) ->
    a ^ "%%" ^ b

let format_of_kind
  : type test v. (test, v) Ty.t -> _ -> (v -> 'r, _, _, 'r) format4
  = fun ty message ->
    let (prefix : _ format4), message = match message with
      | `No_space m -> "%,", m | `Space m -> if m <> "" then " ", m else "", m in
    try match ty with
      | Default ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt
          : (Ty.default -> 'r, _, _, 'r) format4
          = Format (Custom (Custom_succ Custom_zero,
                            (fun () Default -> ""), fmt), str) in
        prefix ^^ fmt
      | Clear ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt
          : (Ty.clear -> 'r, _, _, 'r) format4
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
        (* TODO *)
        let open CamlinternalFormatBasics in
        let message = prepend_percent message in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt = Format (Custom (Custom_succ Custom_zero,
                        (fun () (_ : v) -> ""), fmt), str) in
        prefix ^^ fmt
      | None ->
        let open CamlinternalFormatBasics in
        let Format (fmt, str) = Scanf.format_from_string message "%," in
        let fmt = Format (Custom (Custom_succ Custom_zero,
                        (fun () (_ : v) -> ""), fmt), str) in
        prefix ^^ fmt

let rule
  : Parse.rule -> operation
  = fun ((_level, o), ty, test, message) ->
  let offset = offset o in
  let Kind ty = match ty with
    | _, `Default -> Kind Ty.default
    | _, `Clear -> Kind Ty.clear
    | _, `Regex (Some (case_insensitive, start, line, limit)) ->
      let kind = if line then `Line else `Byte in
      Kind (Ty.regex ~case_insensitive ~start ~limit kind)
    | _, `Regex None ->
      Kind (Ty.regex `Byte)
    | _, `String16 endian ->
      Kind (Ty.unicode endian)
    | _, `String8 (Some (b, _B, c, _C)) ->
      Kind (Ty.search
              ~lower_case_insensitive:c
              ~upper_case_insensitive:_C
              (if b || _B then `Binary else `Text)
              0L ~pattern:"")
    | _, `Search None | _, `String8 None ->
      Kind (Ty.search `Text ~pattern:"" 0L)
    | _, `Search (Some (flags, range)) ->
      let range = Option.value ~default:0L range in
      let lower_case_insensitive = List.exists ((=) `c) flags in
      let upper_case_insensitive = List.exists ((=) `C) flags in
      let compact_whitespaces = List.exists ((=) `W) flags in
      let optional_blank = List.exists ((=) `w) flags in
      let trim = List.exists ((=) `T) flags in
      let kind = match List.exists ((=) `b) flags,
                       List.exists ((=) `B) flags,
                       List.exists ((=) `t) flags with
      | true, true, false
      | true, false, false
      | false, true, false -> `Binary
      | _, _, _ -> `Text in
      Kind (Ty.search ~compact_whitespaces
              ~optional_blank
              ~lower_case_insensitive
              ~upper_case_insensitive
              ~trim
              kind ~pattern:"" range)
    | _, `Indirect rel ->
      Kind (Ty.indirect (if rel then `Rel else `Abs))
    | unsigned, `Numeric (_endian, `Byte, c) ->
      let cast = Char.chr <.> Int64.to_int in
      Kind (Ty.numeric ~unsigned Integer.byte (calculation ~cast c))
    | unsigned, `Numeric (Some (`BE | `LE as endian), `Short, c) ->
      let cast = Int64.to_int in
      Kind (Ty.numeric ~unsigned ~endian Integer.short (calculation ~cast c))
    | unsigned, `Numeric (_, `Short, c) ->
      let cast = Int64.to_int in
      Kind (Ty.numeric ~unsigned Integer.short (calculation ~cast c))
    | unsigned, `Numeric (endian, `Long, c) ->
      let cast = Int64.to_int32 in
      Kind (Ty.numeric ~unsigned
              ?endian:(endian :> Ty.endian option)
              Integer.int32 (calculation ~cast c))
    | unsigned, `Numeric (endian, `Quad, c) ->
      Kind (Ty.numeric ~unsigned
              ?endian:(endian :> Ty.endian option)
           Integer.int64 (calculation ~cast:(fun x -> x) c))
    | _, _ -> assert false in
  let Test test = match test, ty with
    | `True, _ -> Test Test.always_true
    | `Numeric c, Byte _ ->
      Test (Test.numeric Integer.byte (Comparison.map ~f:Number.to_byte c))
    | `Numeric c, Short _ ->
      Test (Test.numeric Integer.short (Comparison.map ~f:Number.to_short c))
    | `Numeric c, Long _ ->
      Test (Test.numeric Integer.int32 (Comparison.map ~f:Number.to_int32 c))
    | `Numeric c, Quad _ ->
      Test (Test.numeric Integer.int64 (Comparison.map ~f:Number.to_int64 c))
    | `Numeric c, Unicode _ ->
      let f = Uchar.of_int <.> Number.to_int in
      let c = Comparison.map ~f c in
      Test (Test.unicode c)
    (* TODO(dinosaure): [`String] and [Unicode]. *)
    | `Numeric c, Double _ ->
      let c = Comparison.map ~f:Number.to_float c in
      Test (Test.float c)
    | `Numeric c, Float _ ->
      let c = Comparison.map ~f:Number.to_float c in
      Test (Test.float c)
    | `String c, Search _
    | `String c, Pascal_string ->
      Test (Test.string c)
    | `String c, Regex _ ->
      let f v =
        try Re.Posix.re v
        with _ -> invalid_arg "Invalid POSIX regular expression: %S" v in
      Test (Test.regex (Comparison.map ~f c))
    | `Numeric c, Search _ ->
      let c = Comparison.map ~f:Number.to_int c in
      Test (Test.length c)
    | `Numeric c, ty ->
      let v = Comparison.value c in
      invalid_arg "Impossible to test a number (%a) with the given type: %a"
        Number.pp v Ty.pp ty
    | `String c, ty ->
      let v = Comparison.value c in
      invalid_arg "Impossible to test a string (%S) with the given type: %a"
        v Ty.pp ty in
  let make
    : type test0 test1 v. test0 Test.t -> (test1, v) Ty.t -> operation
    = fun test ty -> match test, ty with
      | True, Default
      | String _, Default
      | Numeric _, Default
      | Float _, Default
      | Unicode _, Default ->
        Rule (offset, ty, Test.always_true,
              { fmt= fun () -> format_of_kind ty message })
      | True, Clear
      | String _, Clear
      | Numeric _, Clear ->
        Rule (offset, ty, Test.always_true,
              { fmt= fun () -> format_of_kind ty message })
      | Regex c, Regex _ ->
        Rule (offset, ty, Test.regex c,
              { fmt= fun () -> format_of_kind ty message })
      | String c, Search { range; _ } ->
        let pattern = Comparison.value c in
        let range = max range ((Int64.of_int <.> String.length) pattern) in
        Rule (offset, Ty.with_range (Ty.with_pattern ty pattern) range,
              test, { fmt= fun () -> format_of_kind ty message })
      | Length _, Search _ ->
        Rule (offset, ty, test, { fmt= fun () -> format_of_kind ty message })
      | Numeric (Byte, _), Byte _ ->
        Rule (offset, ty, test, { fmt= fun () -> format_of_kind ty message })
      | Numeric (Short, _), Short _ ->
        Rule (offset, ty, test, { fmt= fun () -> format_of_kind ty message })
      | Numeric (Int32, _), Long _ ->
        Rule (offset, ty, test, { fmt= fun () -> format_of_kind ty message })
      | Numeric (Int64, _), Quad _ ->
        Rule (offset, ty, test, { fmt= fun () -> format_of_kind ty message })
      | True, _ ->
        Rule (offset, ty, Test.always_true,
              { fmt= fun () -> format_of_kind ty message })
      | test, ty ->
        invalid_arg "Impossible to operate a test (%a) on the given value (%a)"
          Test.pp test Ty.pp ty in
  make test ty

let name
  : _ -> operation
  = fun ((_level, o), name) ->
  let offset = offset o in
  Name (offset, name)

let use
  : _ -> operation
  = fun ((_level, o), name) ->
  let offset = offset o in
  Use (offset, name)

let operation = function
  | `Rule (((level, _), _, _, _) as v) ->
    let rule = rule v in
    level, rule
  | `Name (((level, _), _) as v) ->
    let name = name v in
    level, name
  | `Use (((level, _), _) as v) ->
    let use = use v in
    level, use
  | _ -> assert false (* TODO *)

let rec left = function
  | Done | Node [] -> 0
  | Node ((_, hd) :: _) ->
    1 + left hd

let append tree (line : Parse.line) = match line with
  | `Rule _ | `Name _ | `Use _ ->
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
