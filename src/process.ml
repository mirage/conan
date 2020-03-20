[@@@warning "-27"]

let ( <.> ) f g = fun x -> f (g x)

let ok x = Ok x

let reword_error f = function
  | Ok _ as v -> v
  | Error err -> Error (f err)

type ('a, 's) io

type 's scheduler =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }

type where = SET | CUR | END

type ('fd, 'error, 's) syscall =
  { lseek : 'fd -> int64 -> where -> ((unit, 'error) result, 's) io
  ; read : 'fd -> int -> ((string, 'error) result, 's) io
  ; read_int8 : 'fd -> ((int, 'error) result, 's) io
  ; read_int16_ne : 'fd -> ((int, 'error) result, 's) io
  ; read_int32_ne : 'fd -> ((int32, 'error) result, 's) io
  ; read_int64_ne : 'fd -> ((int64, 'error) result, 's) io }

type metadata =
  { name : string option
  ; mime : string option
  ; output : string }

let pp_option pp_val ppf = function
  | Some v -> pp_val ppf v
  | None -> Format.fprintf ppf "<none>"

let pp_string ppf = Format.fprintf ppf "%s"

let pp_metadata ppf metadata =
  Format.fprintf ppf "{ @[<hov>name= %a;@ \
                               mime= %a;@ \
                               output= %S@] }"
    (pp_option pp_string) metadata.name
    (pp_option pp_string) metadata.mime
    metadata.output

type error = [ `Invalid_test ]

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let read { bind; return; } syscall fd size =
  let ( >>= ) = bind in
  let ( >?= ) x f = x >>= function
    | Ok x -> f x
    | Error err -> return (Error err) in
  let ( >|= ) x f = x >?= fun x -> return (Ok (f x)) in
  match size with
  | Parse.Byte -> syscall.read_int8 fd >|= Int64.of_int
  | Parse.Leshort ->
    if Sys.big_endian
    then syscall.read_int16_ne fd >|= swap16 >|= Int64.of_int
    else syscall.read_int16_ne fd >|= Int64.of_int
  | Parse.Beshort ->
    if Sys.big_endian
    then syscall.read_int16_ne fd >|= Int64.of_int
    else syscall.read_int16_ne fd >|= swap16 >|= Int64.of_int
  | Parse.Lelong ->
    if Sys.big_endian
    then syscall.read_int32_ne fd >|= swap32 >|= Int64.of_int32
    else syscall.read_int32_ne fd >|= Int64.of_int32
  | Parse.Belong ->
    if Sys.big_endian
    then syscall.read_int32_ne fd >|= Int64.of_int32
    else syscall.read_int32_ne fd >|= swap32 >|= Int64.of_int32
  | Parse.Lequad ->
    if Sys.big_endian
    then syscall.read_int64_ne fd >|= swap64
    else syscall.read_int64_ne fd
  | Parse.Bequad ->
    if Sys.big_endian
    then syscall.read_int64_ne fd >|= swap64
    else syscall.read_int64_ne fd
  | _ -> assert false (* TODO *)

(* TODO *)
let read_float _scheduler _syscall _fd = assert false
let read_double _scheduler _syscall _fd = assert false

let process_offset ({ bind; return; } as scheduler) syscall fd abs_offset offset =
  let ( >>= ) = bind in
  let ( >?= ) x f = x >>= function
    | Ok x -> f x
    | Error err -> return (Error err) in
  let rec go_offset = function
    | Tree.Read (v, size) ->
      go_offset v >>= fun _ ->
      syscall.lseek fd abs_offset SET >?= fun () ->
      read scheduler syscall fd size
    | Tree.Absolute v -> go_offset v
    | Tree.Relative v ->
      go_offset v >?= fun rel_offset ->
      (return <.> ok) (Int64.add abs_offset rel_offset)
    | Tree.Value v -> return (Ok v)
    | Tree.Calculation (v, c) ->
      go_offset v >?= fun a ->
      go_calculation a c
  and go_calculation a = function
    | Invert b ->
      go_calculation a b >?= fun v ->
      (return <.> ok) (Int64.lognot v)
    | Add b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.add a b)
    | Sub b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.sub a b)
    | Mod b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.rem a b)
    | Div b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.div a b)
    | Mul b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.mul a b)
    | Bitwise_and b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.logand a b)
    | Bitwise_xor b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.logxor a b)
    | Bitwise_or b ->
      go_offset b >?= fun b ->
      (return <.> ok) (Int64.logor a b) in
  go_offset offset

let add_char a b = let open Char in unsafe_chr ((code a) + (code b))
let sub_char a b = let open Char in unsafe_chr ((code a) - (code b))
let mul_char a b = let open Char in unsafe_chr ((code a) * (code b))
let div_char a b = let open Char in unsafe_chr ((code a) / (code b))
let bitwise_and_char a b = let open Char in unsafe_chr ((code a) land (code b))
let bitwise_xor_char a b = let open Char in unsafe_chr ((code a) lxor (code b))
let bitwise_or_char a b = let open Char in unsafe_chr ((code a) lor (code b))

let add_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> add_char
  | Short -> fun a b -> (a + b) land 0xffff
  | Int32 -> Int32.add
  | Int64 -> Int64.add

let sub_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> sub_char
  | Short -> fun a b -> (a - b) land 0xffff
  | Int32 -> Int32.sub
  | Int64 -> Int64.sub

let mul_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> mul_char
  | Short -> fun a b -> (a * b) land 0xffff
  | Int32 -> Int32.mul
  | Int64 -> Int64.mul

let div_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> div_char
  | Short -> fun a b -> (a / b) land 0xffff
  | Int32 -> Int32.div
  | Int64 -> Int64.div

let bitwise_and_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> bitwise_and_char
  | Short -> ( land )
  | Int32 -> Int32.logand
  | Int64 -> Int64.logand

let bitwise_xor_integer : type a. a Tree.integer -> (a -> a -> a) = function
  | Byte -> bitwise_xor_char
  | Short -> ( lxor )
  | Int32 -> Int32.logxor
  | Int64 -> Int64.logxor

let process_arithmetic
  : type a. a Tree.integer -> a -> a Tree.arithmetic -> a
  = fun w a c -> match c with
    | Add b -> add_integer w a b
    | Sub b -> sub_integer w a b
    | Mul b -> mul_integer w a b
    | Div b -> div_integer w a b
    | Bitwise_and b -> bitwise_and_integer w a b
    | Bitwise_xor b -> bitwise_xor_integer w a b
    | _ -> assert false (* TODO *)

let rec process_float_arithmetic
  : float -> float Tree.arithmetic -> float
  = fun a -> function
    | Invert c -> Float.neg (process_float_arithmetic a c)
    | Add b -> Float.add a b
    | Sub b -> Float.sub a b
    | Mul b -> Float.mul a b
    | Div b -> Float.div a b
    | Mod b -> Float.rem a b
    | Bitwise_and _ | Bitwise_xor _ | Bitwise_or _ -> invalid_arg "Invalid bitwise operation on float"

module Bm = struct
  let occ needle nlen =
    let occ = Array.make 256 (-1) in
    for a = 0 to nlen - 2 do
      occ.(Char.code needle.[a]) <- a
    done ; occ

  let memcmp v a b l =
    let pos = ref 0 in
    while !pos < l && v.[a + !pos] = v.[b + !pos] do incr pos done ;
    if !pos = l then true else false

  let boyer_moore_needle_match needle nlen portion offset =
    let va = ref (nlen - offset - portion) in
    let ignore = ref 0 in
    if !va < 0 then ( ignore := - !va ; va := 0; ) ;
    if !va > 0 && needle.[!va - 1] = needle.[nlen - portion - 1]
    then false else memcmp needle (nlen - portion + !ignore) !va (portion - !ignore)

  let skip needle nlen =
    let skip = Array.make nlen 0 in
    for a = 0 to nlen - 1 do
      let value = ref 0 in
      while !value < nlen && not (boyer_moore_needle_match needle nlen a !value) do incr value done ;
      skip.(nlen - a - 1) <- !value
    done ; skip

  let search { bind; return; } ~get text hpos occ skip needle nlen =
    let ( >>= ) x f = bind x (function Ok x -> f x | Error err -> return (Error err)) in

    let npos = ref (nlen - 1) in
    let res = ref hpos in

    let rec go () =
      get text ~pos:Int64.(add hpos (of_int !npos)) >>= fun chr ->
      if needle.[!npos] = chr
      then ( if !npos = 0
             then return (Ok ())
             else ( decr npos ; go () ) )
      else ( res := (-1L) ; return (Ok ()) ) in
    go () >>= fun () ->
    (* while needle.[!npos] = get text Int64.(add hpos (of_int !npos))
       do if !npos = 0 then raise Found ; decr npos done ; res := (-1L) *)
    get text ~pos:Int64.(add hpos (of_int !npos)) >>= fun chr ->
    let shift_a = Int64.of_int skip.(!npos) in
    let shift_b =
      let x = Char.code chr in
      Int64.of_int (!npos - occ.(x)) in
    return (Ok (!res, Int64.add hpos (max shift_a shift_b)))

  let find_all ~pattern:needle =
    let nlen = String.length needle in
    let occ = occ needle nlen in
    let skip = skip needle nlen in
    fun ({ bind; return; } as s) ~ln ~get text ->
      let ( >>= ) x f = bind x (function Ok x -> f x | Error err -> return (Error err)) in

      let top = Int64.sub ln (Int64.of_int nlen) in
      let rec go hpos acc =
        if hpos > top
        then return (Ok acc)
        else search s ~get text hpos occ skip needle nlen >>= function
          | (-1L), hpos -> go hpos acc
          | res, hpos -> go hpos (res :: acc) in
      go 0L []
end

let process_read
  : type s fd error test v.
     s scheduler
  -> (fd, error, s) syscall
  -> fd
  -> int64
  -> (test, v) Tree.kind
  -> ((v, [> `Syscall of error ]) result, s) io
  = fun ({ bind; return; } as scheduler) syscall fd abs_offset kind ->
    let ( >>= ) = bind in
    let ( >?= ) x f = x >>= function
      | Ok x -> f x
      | Error err -> return (Error err) in
    let ( >|= ) x f = x >>= fun x -> return (f x) in
    match kind with
    | Tree.Default -> (return <.> ok) (Tree.Default : Tree.default)
    | Tree.Clear -> (return <.> ok) (Tree.Clear : Tree.clear)
    | Tree.Search { pattern
                  ; compact_whitespaces
                  ; optional_blank
                  ; lower_case_insensitive
                  ; upper_case_insensitive
                  ; text
                  ; binary
                  ; trim
                  ; range } ->
      let get fd ~pos =
        syscall.lseek fd (Int64.add abs_offset pos) SET >?= fun () ->
        syscall.read_int8 fd >?= fun code ->
        (return <.> ok <.> Char.chr) code in
      Bm.find_all scheduler ~get ~ln:range fd ~pattern >|=
      reword_error (fun err -> `Syscall err) >?= fun results ->
      let results = List.sort Int64.compare results in
      ( match results with
        | [] ->
          Format.eprintf ">>> no result for %S.\n%!" pattern ;
          return (Ok "")
        | abs_offset :: _ ->
          syscall.lseek fd abs_offset SET >|=
          reword_error (fun err -> `Syscall err) >?= fun () ->
          syscall.read fd (String.length pattern) >|=
          reword_error (fun err -> `Syscall err))
    | Byte ({ unsigned }, c) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Byte >?=
      (return <.> ok <.> Char.chr <.> Int64.to_int) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Byte v c)
    | Short ({ unsigned }, c, `LE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Leshort >?=
      (return <.> ok <.> Int64.to_int) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Short v c)
    | Short ({ unsigned }, c, `BE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Beshort >?=
      (return <.> ok <.> Int64.to_int) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Short v c)
    | Long ({ unsigned }, c, `LE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Lelong >?=
      (return <.> ok <.> Int64.to_int32) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Int32 v c)
    | Long ({ unsigned }, c, `BE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Belong >?=
      (return <.> ok <.> Int64.to_int32) >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Int32 v c)
    | Quad ({ unsigned }, c, `LE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Lequad >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Int64 v c)
    | Quad ({ unsigned }, c, `BE) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read scheduler syscall fd Tree.Bequad >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_arithmetic Tree.Int64 v c)
    | Float ({ unsigned }, c, endian) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read_float scheduler syscall fd >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_float_arithmetic v c)
    | Double ({ unsigned }, c, endian) ->
      syscall.lseek fd abs_offset SET >|=
      reword_error (fun err -> `Syscall err) >?= fun () ->
      read_double scheduler syscall fd >|=
      reword_error (fun err -> `Syscall err) >?= fun v ->
      (return <.> ok) (process_float_arithmetic v c)
    | _ -> assert false

let is_not_zero : type a. a Tree.integer -> a -> bool
  = fun w v -> match w, v with
    | Byte, '\000' -> false
    | Short, 0 -> false
    | Int32, 0l -> false
    | Int64, 0L -> false
    | _, _ -> true

let compare_integer
  : type a. a Tree.integer -> Tree.comparison -> a -> a -> bool
  = fun w comparison a b -> match comparison with
    | Equal -> a = b
    | Not -> a <> b
    | Greater -> a < b
    | Lower -> a > b
    | And -> is_not_zero w (bitwise_and_integer w a b)
    | Xor -> is_not_zero w (bitwise_xor_integer w a b)

let compare_float c a b = match c with
  | Tree.Equal -> a = b
  | Tree.Not -> a <> b
  | Tree.Greater -> a < b
  | Tree.Lower -> a > b
  | Tree.And | Tree.Xor -> invalid_arg "Invalid bitwise operation on float"

let compare_string c a b = match c with
  | Tree.Equal -> a = b
  | Tree.Not -> a <> b
  | Tree.Greater -> a < b
  | Tree.Lower -> a > b
  | Tree.And | Tree.Xor -> invalid_arg "Invalid bitwise operation on string"

let process_test
  : type test v. (test, v) Tree.kind -> test Tree.test -> v -> bool
  = fun kind test a -> match kind, test with
    | _, True -> true
    | Byte _, Numeric (integer, comparison, b) ->
      compare_integer integer comparison a b
    | Short _, Numeric (integer, comparison, b) ->
      compare_integer integer comparison a b
    | Long _, Numeric (integer, comparison, b) ->
      compare_integer integer comparison a b
    | Quad _, Numeric (integer, comparison, b) ->
      compare_integer integer comparison a b
    | Float _, Float (comparison, b) ->
      compare_float comparison a b
    | Double _, Float (comparison, b) ->
      compare_float comparison a b
    | Unicode _, Unicode (b, comparison) -> assert false (* TODO *)
    | Search _, String (comparison, b) ->
      compare_string comparison a b
    | Pascal_string, String (comparison, b) ->
      compare_string comparison a b
    | Regex _, String _ -> assert false (* TODO *)
    | Regex _, Unicode _ -> assert false (* TODO *)
    | Search _, Unicode _ -> assert false
    | Pascal_string, Unicode _ -> assert false
    | Indirect _, _ -> assert false
    | Regex _, Numeric (_, _, _) -> assert false
    | Regex _, Float (_, _) -> assert false
    | Unicode _, Numeric _ -> assert false
    | Unicode _, Float _ -> assert false
    | Unicode _, String _ -> assert false
    | Double _, Unicode _ -> assert false
    | Float _, Unicode _ -> assert false
    | Quad _, Unicode _ -> assert false
    | Long _, Unicode _ -> assert false
    | Short _, Unicode _ -> assert false
    | Byte _, Unicode _ -> assert false
    | Double _,      Numeric _ -> .
    | Float _,       Numeric _ -> .
    | Pascal_string, Numeric _ -> .
    | Search _,      Numeric _ -> .
    | Default,       Numeric _ -> .
    | Clear,         Numeric _ -> .
    | _, _ -> assert false

let process_fmt
  : type v. metadata -> (_, v) Tree.kind -> v Tree.fmt -> v -> metadata
  = fun metadata kind { Tree.fmt } v ->
    Format.kasprintf (fun output -> { metadata with output }) ("%s" ^^ (fmt ())) metadata.output v

let process
  : type s fd error.
     s scheduler
  -> (fd, error, s) syscall
  -> fd
  -> int64
  -> metadata
  -> Tree.operation
  -> ((int64 * metadata, [> `Syscall of error
                         |  `Invalid_test ]) result, s) io
  = fun ({ bind; return; } as scheduler) syscall fd abs_offset metadata -> function
    | Tree.Name (_, name) -> return (Ok (abs_offset, { metadata with name= Some name }))
    | Tree.Rule (offset, kind, test, fmt) ->
      let ( >>= ) = bind in
      let ( >|= ) x f = x >>= fun x -> return (f x) in
      let ( >?= ) x f = x >>= function
        | Ok x -> f x
        | Error err -> return (Error err) in
      process_offset scheduler syscall fd abs_offset offset >|=
      reword_error (fun err -> `Syscall err) >?= fun abs_offset ->
      process_read scheduler syscall fd abs_offset kind >?= fun v ->
      match process_test kind test v with
      | true ->
        let metadata = process_fmt metadata kind fmt v in
        return (Ok (abs_offset, metadata))
      | false ->
        return (Error `Invalid_test)

let rec ascending_walk
  = fun ({ bind; return; } as scheduler) syscall fd results queue ->
    let ( >>= ) = bind in

    match Queue.pop queue with
    | _, candidate, Tree.Done -> ascending_walk scheduler syscall fd (candidate :: results) queue
    | abs_offset, candidate, (Tree.Node lst) ->
      let lst = List.rev lst in
      let rec go = function
        | [] -> return ()
        | (operation, tree) :: rest ->
          Format.eprintf "[-] process %a %!" Tree.pp_operation operation ;
          process scheduler syscall fd abs_offset candidate operation >>= function
          | Ok (abs_offset, candidate) ->
            Format.eprintf "[ok].\n%!" ;
            Queue.push (abs_offset, candidate, tree) queue ; go rest
          | Error err ->
            Format.eprintf "[error].\n%!" ;
            go rest in
      go lst >>= fun () -> ascending_walk scheduler syscall fd results queue
    | exception Queue.Empty -> return (List.rev results)

let ascending_walk scheduler syscall fd tree =
  let queue = Queue.create () in
  Queue.push (0L, { name= None; mime= None; output= "" }, tree) queue ;
  ascending_walk scheduler syscall fd [] queue
