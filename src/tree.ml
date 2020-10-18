let () = Printexc.record_backtrace true

let ( <.> ) f g x = f (g x)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let pf = Format.fprintf

type 'a fmt = { fmt : 'r. unit -> ('a -> 'r, 'r) Fmt.fmt }

type operation =
  | Rule : Offset.t * ('test, 'v) Ty.t * 'test Test.t * 'v fmt -> operation
  | Name : Offset.t * string -> operation
  | Use : { offset : Offset.t; invert : bool; name : string } -> operation
  | MIME : string -> operation

let pp_operation ppf = function
  | Rule (offset, ty, test, _) ->
      pf ppf "%a\t%a\t%a\t#fmt" Offset.pp offset Ty.pp ty Test.pp test
  | Name (offset, name) -> pf ppf "%a\t%s" Offset.pp offset name
  | Use { offset; invert = false; name } ->
      pf ppf "%a\t%s" Offset.pp offset name
  | Use { offset; invert = true; name } ->
      pf ppf "\\^%a\t%s" Offset.pp offset name
  | MIME v -> pf ppf "!:mime %s" v

type tree = Node of (operation * tree) list | Done

let pp_level ppf n =
  let rec go = function
    | 0 -> ()
    | n ->
        pf ppf ">" ;
        go (pred n) in
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

let indirect_1 ?(size = system_long) v =
  let f = function
    | `Dir v -> Offset.Value v
    | `Ind v -> Offset.(Relative (Read (Value v, size))) in
  Arithmetic.map ~f v

let indirect_0 (return, (offset, size, disp)) =
  let open Offset in
  let size = Option.value ~default:system_long size in
  let offset =
    match (disp, offset) with
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

type k = Ty : ('test, 'v) Ty.t -> k

type t = Test : 'test Test.t -> t

type f = Format : 'v fmt -> f

let identity x = x

let calculation :
    cast:(int64 -> 'v) -> int64 Arithmetic.t option -> 'v Arithmetic.t =
 fun ~cast:f -> function
  | None -> Arithmetic.add (f 0L)
  | Some c -> Arithmetic.map ~f c

let percent = Sub.v "%"

let rec force_to_use_any_formatter s =
  let open Sub in
  match cut ~sep:percent s with
  | None -> None
  | Some (x, r) ->
  match head r with
  | None -> None
  | Some '%' -> (
      match force_to_use_any_formatter (tail r) with
      | None -> None
      | Some r -> Some (to_string x ^ "%%" ^ r))
  | _ ->
      let flags, r = span ~sat:Fmt.is_flag r in
      let padding, r = span ~sat:Fmt.is_digit r in
      let r = tail r in
      Some
        (to_string x
        ^ "%"
        ^ to_string flags
        ^ to_string padding
        ^ "!"
        ^ to_string r)

let key_of_ty : type test v. string -> (test, v) Ty.t -> v Fmt.Hmap.Key.key =
 fun message ty0 ->
  let any = Fmt.Hmap.Key.create () in
  let (Fmt.Ty ty1) = Fmt.ty_of_string ~any message in
  match (ty0, ty1) with
  | Byte _, Int End -> Pps.char_to_int
  | Long _, Int End -> Pps.int32_to_int
  | _ ->
      invalid_arg "Impossible to convert %a to %a on %S" Ty.pp ty0 Fmt.pp_ty ty1
        message

let format_of_ty : type test v. (test, v) Ty.t -> _ -> (v -> 'r, 'r) Fmt.fmt =
 fun ty message ->
  let with_space, message =
    match message with
    | `No_space message -> (false, message)
    | `Space "" -> (false, "")
    | `Space message -> (true, message) in
  let with_space fmt =
    if not with_space then fmt else Fmt.((pp_string $ " ") :: fmt) in
  let any = Fmt.Hmap.Key.create () in
  try
    match ty with
    | Default ->
        let fmt = Fmt.of_string ~any message Fmt.End in
        with_space Fmt.([ ignore ] ^^ fmt)
    | Clear ->
        let fmt = Fmt.of_string ~any message Fmt.End in
        with_space Fmt.([ ignore ] ^^ fmt)
    | Byte _ -> with_space (Fmt.of_string ~any message Fmt.(Char End))
    | Search _ -> with_space (Fmt.of_string ~any message Fmt.(String End))
    | Unicode_string _ ->
        with_space (Fmt.of_string ~any message Fmt.(String End))
    | Short _ -> with_space (Fmt.of_string ~any message Fmt.(Int End))
    | Long _ -> with_space (Fmt.of_string ~any message Fmt.(Int32 End))
    | Quad _ -> with_space (Fmt.of_string ~any message Fmt.(Int64 End))
    | Float _ -> with_space (Fmt.of_string ~any message Fmt.(Float End))
    | Double _ -> with_space (Fmt.of_string ~any message Fmt.(Float End))
    | Regex _ -> with_space (Fmt.of_string ~any message Fmt.(String End))
    | Pascal_string -> with_space (Fmt.of_string ~any message Fmt.(String End))
    | Date _ -> with_space (Fmt.of_string ~any message Fmt.(String End))
    | Indirect _ -> assert false
  with _ -> (
    match force_to_use_any_formatter (Sub.v message) with
    | Some message1 ->
        let key = key_of_ty message ty in
        with_space (Fmt.of_string message1 ~any:key Fmt.(Any (key, End)))
    | None -> with_space Fmt.([ ignore ] ^^ of_string ~any message End))

(* TODO: "\\0" -> "\000", it's an hack! *)
let normalize_regex str =
  let zero = Sub.v "\\0" in
  let rec go str =
    match Sub.cut ~sep:zero str with
    | Some (a, b) ->
        let b = go b in
        Sub.concat [ a; Sub.v "\000"; b ]
    | None -> str in
  Sub.to_string (go (Sub.v str))

type date = [ `Date | `Ldate | `Qdate | `Qldate | `Qwdate ]

let rule : Parse.rule -> operation =
 fun ((_level, o), ty, test, message) ->
  let offset = offset o in
  let (Ty ty) =
    match ty with
    | _, `Default -> Ty Ty.default
    | _, `Clear -> Ty Ty.clear
    | _, `Regex (Some (case_insensitive, start, line, limit)) ->
        let kind = if line then `Line else `Byte in
        Ty (Ty.regex ~case_insensitive ~start ~limit kind)
    | _, `Regex None -> Ty (Ty.regex `Byte)
    | _, `String16 endian -> Ty (Ty.str_unicode endian)
    | _, `String8 (Some (b, _B, c, _C)) ->
        Ty
          (Ty.search ~lower_case_insensitive:c ~upper_case_insensitive:_C
             (if b || _B then `Binary else `Text)
             0L ~pattern:"")
    | _, `Search None | _, `String8 None -> Ty (Ty.search `Text ~pattern:"" 0L)
    | _, `Search (Some (flags, range)) ->
        let range = Option.value ~default:0L range in
        let lower_case_insensitive = List.exists (( = ) `c) flags in
        let upper_case_insensitive = List.exists (( = ) `C) flags in
        let compact_whitespaces = List.exists (( = ) `W) flags in
        let optional_blank = List.exists (( = ) `w) flags in
        let trim = List.exists (( = ) `T) flags in
        let kind =
          match
            ( List.exists (( = ) `b) flags,
              List.exists (( = ) `B) flags,
              List.exists (( = ) `t) flags )
          with
          | true, true, false | true, false, false | false, true, false ->
              `Binary
          | _, _, _ -> `Text in
        Ty
          (Ty.search ~compact_whitespaces ~optional_blank
             ~lower_case_insensitive ~upper_case_insensitive ~trim kind
             ~pattern:"" range)
    | _, `Indirect rel -> Ty (Ty.indirect (if rel then `Rel else `Abs))
    | _, `Numeric (endian, (#date as date), c) ->
        let cast = Ptime.Span.of_int_s <.> Int64.to_int in
        Ty (Ty.date date endian (calculation ~cast c))
    | unsigned, `Numeric (_endian, `Byte, c) ->
        let cast = Char.chr <.> ( land ) 0xff <.> Int64.to_int in
        Ty (Ty.numeric ~unsigned Integer.byte (calculation ~cast c))
    | unsigned, `Numeric (Some ((`BE | `LE) as endian), `Short, c) ->
        let cast = Int64.to_int in
        Ty (Ty.numeric ~unsigned ~endian Integer.short (calculation ~cast c))
    | unsigned, `Numeric (_, `Short, c) ->
        let cast = Int64.to_int in
        Ty (Ty.numeric ~unsigned Integer.short (calculation ~cast c))
    | unsigned, `Numeric (endian, `Long, c) ->
        let cast = Int64.to_int32 in
        Ty
          (Ty.numeric ~unsigned
             ?endian:(endian :> Ty.endian option)
             Integer.int32 (calculation ~cast c))
    | unsigned, `Numeric (endian, `Quad, c) ->
        Ty
          (Ty.numeric ~unsigned
             ?endian:(endian :> Ty.endian option)
             Integer.int64
             (calculation ~cast:(fun x -> x) c))
    | unsigned, `Numeric (endian, `Double, c) ->
        let cast = Int64.float_of_bits in
        Ty
          (Ty.double ~unsigned
             ?endian:(endian :> Ty.endian option)
             (calculation ~cast c))
    | unsigned, `Numeric (endian, `Float, c) ->
        let cast = Int64.float_of_bits in
        Ty
          (Ty.float ~unsigned
             ?endian:(endian :> Ty.endian option)
             (calculation ~cast c)) in
  let (Test test) =
    match (test, ty) with
    | `True, _ -> Test Test.always_true
    | `Numeric c, Byte _ ->
        let c = Comparison.map ~f:fst c in
        Test (Test.numeric Integer.byte (Comparison.map ~f:Number.to_byte c))
    | `Numeric c, Short _ ->
        let c = Comparison.map ~f:fst c in
        Test (Test.numeric Integer.short (Comparison.map ~f:Number.to_short c))
    | `Numeric c, Long _ ->
        let c = Comparison.map ~f:fst c in
        Test (Test.numeric Integer.int32 (Comparison.map ~f:Number.to_int32 c))
    | `Numeric c, Quad _ ->
        let c = Comparison.map ~f:fst c in
        Test (Test.numeric Integer.int64 (Comparison.map ~f:Number.to_int64 c))
    | `Numeric c, Double _ ->
        let c = Comparison.map ~f:fst c in
        let c = Comparison.map ~f:Number.to_float c in
        Test (Test.float c)
    | `Numeric c, Float _ ->
        let c = Comparison.map ~f:fst c in
        let c = Comparison.map ~f:Number.to_float c in
        Test (Test.float c)
    | `String c, Search _ | `String c, Pascal_string -> Test (Test.string c)
    | `String c, Unicode_string _ -> Test (Test.string c)
    | `String c, Regex _ -> (
        let f v = Re.Posix.re (normalize_regex v) in
        try Test (Test.regex (Comparison.map ~f c))
        with _ -> Test Test.always_false)
    | `Numeric c, Search _ ->
        (* XXX(dinosaure): choose [string] repr instead [numeric] repr. *)
        let c = Comparison.map ~f:snd c in
        Test (Test.string c)
    | `Numeric c, Date _ ->
        let c = Comparison.map ~f:fst c in
        Test (Test.date (Comparison.map ~f:Number.to_ptime c))
    | `Numeric c, ty ->
        let c = Comparison.map ~f:fst c in
        let v = Comparison.value c in
        invalid_arg "Impossible to test a number (%a) with the given type: %a"
          Number.pp v Ty.pp ty
    | `String c, ty ->
        let v = Comparison.value c in
        invalid_arg "Impossible to test a string (%S) with the given type: %a" v
          Ty.pp ty in
  let make : type test0 test1 v. test0 Test.t -> (test1, v) Ty.t -> operation =
   fun test ty ->
    match (test, ty) with
    | True, Default
    | String _, Default
    | Numeric _, Default
    | Float _, Default
    | Unicode_string _, Default ->
        Rule
          ( offset,
            ty,
            Test.always_true,
            { fmt = (fun () -> format_of_ty ty message) } )
    | True, Clear | String _, Clear | Numeric _, Clear ->
        Rule
          ( offset,
            ty,
            Test.always_true,
            { fmt = (fun () -> format_of_ty ty message) } )
    | Regex c, Regex _ ->
        Rule
          ( offset,
            ty,
            Test.regex c,
            { fmt = (fun () -> format_of_ty ty message) } )
    | String c, Search { range; _ } ->
        let pattern = Comparison.value c in
        let range = max range ((Int64.of_int <.> String.length) pattern) in
        Rule
          ( offset,
            (Ty.with_range range <.> Ty.with_pattern pattern) ty,
            test,
            { fmt = (fun () -> format_of_ty ty message) } )
    | Length _, Search _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Numeric (Byte, _), Byte _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Numeric (Short, _), Short _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Numeric (Int32, _), Long _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Numeric (Int64, _), Quad _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Date _, Date _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Float _, Double _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | Float _, Float _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | String _, Unicode_string _ ->
        Rule (offset, ty, test, { fmt = (fun () -> format_of_ty ty message) })
    | True, _ ->
        Rule
          ( offset,
            ty,
            Test.always_true,
            { fmt = (fun () -> format_of_ty ty message) } )
    | False, _ ->
        Rule
          ( offset,
            ty,
            Test.always_false,
            { fmt = (fun () -> format_of_ty ty message) } )
    | test, ty ->
        invalid_arg "Impossible to operate a test (%a) on the given value (%a)"
          Test.pp test Ty.pp ty in
  make test ty

let name : _ -> operation =
 fun ((_level, o), name) ->
  let offset = offset o in
  Name (offset, name)

let use : _ -> operation =
 fun ((_level, o), invert, name) ->
  let offset = offset o in
  Use { offset; invert; name }

let mime : _ -> operation = fun v -> MIME v

exception Not_implemented

let operation ~max = function
  | `Rule (((level, _), _, _, _) as v) ->
      let rule = rule v in
      (level, rule)
  | `Name (((level, _), _) as v) ->
      let name = name v in
      (level, name)
  | `Use (((level, _), _, _) as v) ->
      let use = use v in
      (level, use)
  | `Mime v -> (max, mime v)
  | _ -> raise Not_implemented

let rec left = function
  | Done | Node [] -> 0
  | Node ((_, hd) :: _) -> 1 + left hd

let rec depth_left = function
  | Done | Node [] -> 0
  | Node ((_, hd) :: _) -> 1 + depth_left hd

let append tree (line : Parse.line) =
  match line with
  | `Rule _ | `Name _ | `Use _ | `Mime _ ->
      let max = depth_left tree in
      let level, operation = operation ~max line in
      if level <= left tree
      then
        let rec go cur tree =
          if cur = level
          then
            match tree with
            | Done -> Node [ (operation, Done) ]
            | Node l -> Node ((operation, Done) :: l)
          else
            match tree with
            | Done | Node [] -> Node [ (operation, Done) ]
            | Node ((x, hd) :: tl) ->
                let hd = go (succ cur) hd in
                Node ((x, hd) :: tl) in
        go 0 tree
      else tree
  | _ -> tree
