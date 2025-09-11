(* XXX(dinosaure): at the beginning, we used [Format.format] but we can
   replace a "%*" (like "%d") by an user-defined one (like "%a"). This
   module wants to provide our own [Format.format] type with this ability
   with [Hmap]. See [Atom]. *)

type ('a, 'b) refl = Refl : ('a, 'a) refl
type formatter = Format.formatter

module Hmap = struct
  module Tid = struct
    type _ t = ..
  end

  module type Tid = sig
    type t
    type _ Tid.t += Tid : t Tid.t
  end

  type 'a tid = (module Tid with type t = 'a)

  let tid () (type s) =
    let module M = struct
      type t = s
      type _ Tid.t += Tid : t Tid.t
    end in
    (module M : Tid with type t = s)

  let refl : type a b. a tid -> b tid -> (a, b) refl option =
   fun r s ->
    let module A = (val r : Tid with type t = a) in
    let module B = (val s : Tid with type t = b) in
    match A.Tid with B.Tid -> Some Refl | _ -> None

  module Key = struct
    type 'a key = { uid : int; tid : 'a tid }

    let uid =
      let id = ref (-1) in
      fun () ->
        incr id;
        !id

    let create () =
      let uid = uid () in
      let tid = tid () in
      { uid; tid }

    type t = V : 'a key -> t

    let hide_type k = V k
    let equal (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid = 0
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
  end

  type 'a key = 'a Key.key

  module Map = Map.Make (Key)

  type binding =
    | B :
        'a key
        * (?padding:[ `Left of int | `Right of int | `Zero of int ] ->
          ?precision:int ->
          formatter ->
          'a ->
          unit)
        -> binding

  type t = binding Map.t

  let empty = Map.empty
  let add k v t = Map.add (V k) (B (k, v)) t

  let find : type a.
      a key ->
      t ->
      (?padding:[ `Left of int | `Right of int | `Zero of int ] ->
      ?precision:int ->
      Format.formatter ->
      a ->
      unit)
      option =
   fun k t ->
    match Map.find (Key.V k) t with
    | B (k', v) -> (
        match refl k.Key.tid k'.Key.tid with
        | Some Refl -> Some v
        | None -> None)
    | exception Not_found -> None

  let get k t =
    match find k t with Some pp -> pp | None -> invalid_arg "Key not found"
end

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type 'a t = formatter -> 'a -> unit

type ('ty, 'v) padding =
  | Nop : ('v, 'v) padding
  | Lit : [ `Left | `Right | `Zero ] * int -> ('v, 'v) padding
  | Arg : [ `Left | `Right | `Zero ] -> (int -> 'v, 'v) padding

type ('ty, 'v) precision =
  | Nop : ('v, 'v) precision
  | Lit : int -> ('v, 'v) precision
  | Arg : (int -> 'v, 'v) precision

type _ conv =
  | Conv_c : char conv
  | Conv_d : int conv
  | Conv_x : int conv
  | Conv_Cx : int conv
  | Conv_X : int conv
  | Conv_CX : int conv

type ('ty, 'v) order =
  | Byte :
      char conv * ('u, 'v) padding * ('v, char -> 'w) precision
      -> ('u, 'w) order
  | Short :
      bool * int conv * ('u, 'v) padding * ('v, int -> 'w) precision
      -> ('u, 'w) order
  | Long :
      bool * int conv * ('u, 'v) padding * ('v, int32 -> 'w) precision
      -> ('u, 'w) order
  | Quad :
      bool * int conv * ('u, 'v) padding * ('v, int64 -> 'w) precision
      -> ('u, 'w) order
  | Float : ('u, 'v) padding * ('v, float -> 'w) precision -> ('u, 'w) order
  | String : ('u, string -> 'v) padding -> ('u, 'v) order
  | Const : 'a t * 'a -> ('v, 'v) order
  | Noop : ('v, 'v) order
  | Atom :
      ('u, 'v) padding * ('v, 'a -> 'w) precision * 'a Hmap.key
      -> ('u, 'w) order
  | Param : ('a t -> 'a -> 'v, 'v) order
  | Ignore : ('a -> 'v, 'v) order

and ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | ( :: ) : ('ty, 'v) order * ('v, 'r) fmt -> ('ty, 'r) fmt

type ('ty, 'v) ty = ('ty, 'v, 'ty, 'v) tyrel

and ('ty0, 'v0, 'ty1, 'v1) tyrel =
  | Char :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (char -> 'ty0, 'v0, char -> 'ty1, 'v1) tyrel
  | Int :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (int -> 'ty0, 'v0, int -> 'ty1, 'v1) tyrel
  | Int32 :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (int32 -> 'ty0, 'v0, int32 -> 'ty1, 'v1) tyrel
  | Int64 :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (int64 -> 'ty0, 'v0, int64 -> 'ty1, 'v1) tyrel
  | Float :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (float -> 'ty0, 'v0, float -> 'ty1, 'v1) tyrel
  | String :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> (string -> 'ty0, 'v0, string -> 'ty1, 'v1) tyrel
  | Param :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> ('a t -> 'a -> 'ty0, 'v0, 'a t -> 'a -> 'ty1, 'v1) tyrel
  | Any :
      'a Hmap.key * ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> ('a -> 'ty0, 'v0, 'a -> 'ty1, 'v1) tyrel
  | Ignore :
      ('ty0, 'v0, 'ty1, 'v1) tyrel
      -> ('a -> 'ty0, 'v0, 'a -> 'ty1, 'v1) tyrel
  | End : ('v0, 'v0, 'v1, 'v1) tyrel

type ('v, 'r) tw = T : ('u, 'v) fmt * ('v, 'w) ty -> ('u, 'w) tw
type ('v, 'r) pw = P : ('u, 'v) padding * ('v, 'w) ty -> ('u, 'w) pw

type ('v, 'r) ppw =
  | V : ('a, 'b) padding * ('b, 'c) precision * ('c, 'd) ty -> ('a, 'd) ppw

type v = Fmt : ('ty, 'v) fmt -> v
type w = Ty : ('ty, 'v) ty -> w

let pf = Format.fprintf
let strf = Format.asprintf

let padding_and_precision_to_padding : type u v w.
    (u, v) padding -> (v, w) precision -> (u, w) padding =
 fun padding precision ->
  match (padding, precision) with
  | Nop, Nop -> Nop
  | Nop, Lit n -> Lit (`Left, n)
  | Nop, Arg -> Arg `Left
  | Lit (dir, n), Nop -> Lit (dir, n)
  | Arg dir, Nop -> Arg dir
  | Arg _, Arg ->
      assert false (* [int -> int -> *] can not be reduced to [int -> *] *)
  | Arg dir, Lit _ -> Arg dir
  | Lit (dir, _), Arg -> Arg dir
  | Lit (dir, n), Lit _ -> Lit (dir, n)

let pp_ty : type v r. Format.formatter -> (v, r) ty -> unit =
 fun ppf ty ->
  let rec flat : type v r. _ list -> (v, r) ty -> _ list =
   fun acc -> function
     | End -> List.rev acc
     | Char ty -> flat (`Char :: acc) ty
     | Int ty -> flat (`Int :: acc) ty
     | Int32 ty -> flat (`Int32 :: acc) ty
     | Int64 ty -> flat (`Int64 :: acc) ty
     | Float ty -> flat (`Float :: acc) ty
     | String ty -> flat (`String :: acc) ty
     | Param ty -> flat (`Param :: acc) ty
     | Any (_, ty) -> flat (`Any :: acc) ty
     | Ignore ty -> flat (`Ignore :: acc) ty
  in
  let pp_ty ppf = function
    | `Char -> pf ppf "char"
    | `Int -> pf ppf "int"
    | `Int32 -> pf ppf "int32"
    | `Int64 -> pf ppf "int64"
    | `Float -> pf ppf "float"
    | `String -> pf ppf "string"
    | `Param -> pf ppf "'a t -> 'a"
    | `Any -> pf ppf "'any"
    | `Ignore -> pf ppf "'ignore"
  in
  let pp_list pp_val ~sep:pp_sep ppf lst =
    let rec go : _ list -> unit = function
      | [] -> ()
      | [ x ] -> pp_val ppf x
      | x :: r ->
          pf ppf "%a%a" pp_val x pp_sep ();
          go r
    in
    go lst
  in
  let sep ppf () = pf ppf " -> " in
  pf ppf "(%a)" (pp_list ~sep pp_ty) (flat [] ty)

let ty_of_padding : type v r. (v, r) padding -> (v, r) ty = function
  | Nop -> End
  | Lit _ -> End
  | Arg _ -> Int End

let ty_of_precision : type v r. (v, r) precision -> (v, r) ty = function
  | Nop -> End
  | Lit _ -> End
  | Arg -> Int End

let rec concat_ty : type u v w. (u, v) ty -> (v, w) ty -> (u, w) ty =
 fun l1 l2 ->
  match l1 with
  | End -> l2
  | Char l1 -> Char (concat_ty l1 l2)
  | Int l1 -> Int (concat_ty l1 l2)
  | Int32 l1 -> Int32 (concat_ty l1 l2)
  | Int64 l1 -> Int64 (concat_ty l1 l2)
  | Float l1 -> Float (concat_ty l1 l2)
  | String l1 -> String (concat_ty l1 l2)
  | Param l1 -> Param (concat_ty l1 l2)
  | Any (key, l1) -> Any (key, concat_ty l1 l2)
  | Ignore l1 -> Ignore (concat_ty l1 l2)

let ( @ ) = concat_ty

let rec ty_of_fmt : type v r. (v, r) fmt -> (v, r) ty = function
  | [] -> End
  | Const _ :: fmt -> ty_of_fmt fmt
  | Noop :: fmt -> ty_of_fmt fmt
  | Ignore :: fmt -> Ignore (ty_of_fmt fmt)
  | Atom (padding, precision, key) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Any (key, ty_of_fmt fmt)
  | Param :: fmt -> Param (ty_of_fmt fmt)
  | Byte (_, padding, precision) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Char (ty_of_fmt fmt)
  | Short (_unsigned, _, padding, precision) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Int (ty_of_fmt fmt)
  | Long (_unsigned, _, padding, precision) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Int32 (ty_of_fmt fmt)
  | Quad (_usigned, _, padding, precision) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Int64 (ty_of_fmt fmt)
  | Float (padding, precision) :: fmt ->
      let padding = ty_of_padding padding in
      let precision = ty_of_precision precision in
      padding @ precision @ Float (ty_of_fmt fmt)
  | String padding :: fmt ->
      let padding = ty_of_padding padding in
      padding @ String (ty_of_fmt fmt)

exception Invalid_type
exception Invalid_key

let gen_padding : type ty0 v0 ty1 v1.
    (ty0, v0) padding -> (ty1, v1) ty -> (ty1, v1) pw =
 fun pad ty ->
  match (pad, ty) with
  | Nop, _ -> P (Nop, ty)
  | Lit (padding, pad), _ -> P (Lit (padding, pad), ty)
  | Arg padding, Int ty -> P (Arg padding, ty)
  | _ -> raise Invalid_type

let gen_padding_precision : type u v w ty0 v0.
    (u, v) padding -> (v, w) precision -> (ty0, v0) ty -> (ty0, v0) ppw =
 fun padding precision ty ->
  match (precision, gen_padding padding ty) with
  | Nop, P (padding, ty) -> V (padding, Nop, ty)
  | Lit v, P (padding, ty) -> V (padding, Lit v, ty)
  | Arg, P (padding, Int ty) -> V (padding, Arg, ty)
  | _ -> raise Invalid_type

let rec gen : type ty0 v0 ty1 v1. (ty0, v0) fmt -> (ty1, v1) ty -> (ty1, v1) tw
    =
 fun fmt ty ->
  match (fmt, ty) with
  | [], rest -> T ([], rest)
  | String padding :: fmt_rest, _ -> (
      match gen_padding padding ty with
      | P (padding, String ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (String padding :: fmt, ty)
      | _ -> raise Invalid_type)
  | Const (pp, v) :: fmt_rest, ty_rest ->
      let (T (fmt, ty)) = gen fmt_rest ty_rest in
      T (Const (pp, v) :: fmt, ty)
  | Byte (conv, padding, precision) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Char ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (Byte (conv, padding, precision) :: fmt, ty)
      | _ -> raise Invalid_type)
  | Short (unsigned, conv, padding, precision) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Int ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (Short (unsigned, conv, padding, precision) :: fmt, ty)
      | _ -> raise Invalid_type)
  | Long (unsigned, conv, padding, precision) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Int32 ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (Long (unsigned, conv, padding, precision) :: fmt, ty)
      | _ -> raise Invalid_type)
  | Quad (unsigned, conv, padding, precision) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Int64 ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (Quad (unsigned, conv, padding, precision) :: fmt, ty)
      | _ -> raise Invalid_type)
  | Float (padding, precision) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Float ty_rest) ->
          let (T (fmt, ty)) = gen fmt_rest ty_rest in
          T (Float (padding, precision) :: fmt, ty)
      | _ -> raise Invalid_type)
  | Atom (padding, precision, key0) :: fmt_rest, _ -> (
      match gen_padding_precision padding precision ty with
      | V (padding, precision, Any (key1, ty_rest)) -> (
          if not Hmap.Key.(equal (hide_type key0) (hide_type key1)) then
            raise Invalid_key
          else
            match Hmap.refl key0.Hmap.Key.tid key1.Hmap.Key.tid with
            | Some Refl ->
                let (T (fmt, ty)) = gen fmt_rest ty_rest in
                T (Atom (padding, precision, key0) :: fmt, ty)
            | None -> raise Invalid_key)
      | _ -> raise Invalid_type)
  | Noop :: fmt_rest, ty_rest -> gen fmt_rest ty_rest
  | Ignore :: fmt_rest, Ignore ty_rest ->
      let (T (fmt, ty)) = gen fmt_rest ty_rest in
      T (Ignore :: fmt, ty)
  | Param :: fmt_rest, Param ty_rest ->
      let (T (fmt, ty)) = gen fmt_rest ty_rest in
      T (Param :: fmt, ty)
  | Param :: _, _ -> raise Invalid_type
  | Ignore :: _, _ -> raise Invalid_type

let rec concat : type u v w. (u, v) fmt -> (v, w) fmt -> (u, w) fmt =
 fun l1 l2 -> match l1 with [] -> l2 | x :: r -> x :: concat r l2

(* XXX(dinosaure): tail-rec? *)

let ( ^^ ) = concat

let pp_char ~conv:_ ?padding ?precision ppf v =
  match (padding, precision) with
  | Some (`Left padding), _ ->
      pf ppf (Scanf.format_from_string (strf "%%-%dc" padding) "%c") v
  | Some (`Zero _), _ -> pf ppf "%0c" v
  | _ -> pf ppf "%c" v

let pp_int ~unsigned ~conv:_ ?padding ?precision ppf v =
  match (unsigned, padding, precision) with
  | false, None, None -> pf ppf "%d" v
  | false, Some (`Left padding), None -> pf ppf "%-*d" padding v
  | false, Some (`Zero padding), _ -> pf ppf "%0*d" padding v
  | false, Some (`Right padding), None -> pf ppf "%*d" padding v
  | false, None, Some precision -> pf ppf "%.*d" precision v
  | false, Some (`Left padding), Some precision ->
      pf ppf "%-*.*d" padding precision v
  | false, Some (`Right padding), Some precision ->
      pf ppf "%*.*d" padding precision v
  | true, None, None -> pf ppf "%u" v
  | true, Some (`Left padding), None -> pf ppf "%-*u" padding v
  | true, Some (`Zero padding), _ -> pf ppf "%0*u" padding v
  | true, Some (`Right padding), None -> pf ppf "%*u" padding v
  | true, None, Some precision -> pf ppf "%.*u" precision v
  | true, Some (`Left padding), Some precision ->
      pf ppf "%-*.*u" padding precision v
  | true, Some (`Right padding), Some precision ->
      pf ppf "%*.*u" padding precision v

let pp_int32 ~unsigned ~conv:_ ?padding ?precision ppf v =
  match (unsigned, padding, precision) with
  | false, None, None -> pf ppf "%ld" v
  | false, Some (`Left padding), None -> pf ppf "%-*ld" padding v
  | false, Some (`Zero padding), _ -> pf ppf "%0*ld" padding v
  | false, Some (`Right padding), None -> pf ppf "%*ld" padding v
  | false, None, Some precision -> pf ppf "%.*ld" precision v
  | false, Some (`Left padding), Some precision ->
      pf ppf "%-*.*ld" padding precision v
  | false, Some (`Right padding), Some precision ->
      pf ppf "%*.*ld" padding precision v
  | true, None, None -> pf ppf "%lu" v
  | true, Some (`Left padding), None -> pf ppf "%-*lu" padding v
  | true, Some (`Zero padding), _ -> pf ppf "%0*lu" padding v
  | true, Some (`Right padding), None -> pf ppf "%*lu" padding v
  | true, None, Some precision -> pf ppf "%.*lu" precision v
  | true, Some (`Left padding), Some precision ->
      pf ppf "%-*.*lu" padding precision v
  | true, Some (`Right padding), Some precision ->
      pf ppf "%*.*lu" padding precision v

let pp_int64 ~unsigned ~conv:_ ?padding ?precision ppf v =
  match (unsigned, padding, precision) with
  | false, None, None -> pf ppf "%Ld" v
  | false, Some (`Left padding), None -> pf ppf "%-*Ld" padding v
  | false, Some (`Zero padding), _ -> pf ppf "%0*Ld" padding v
  | false, Some (`Right padding), None -> pf ppf "%*Ld" padding v
  | false, None, Some precision -> pf ppf "%.*Ld" precision v
  | false, Some (`Left padding), Some precision ->
      pf ppf "%-*.*Ld" padding precision v
  | false, Some (`Right padding), Some precision ->
      pf ppf "%*.*Ld" padding precision v
  | true, None, None -> pf ppf "%Lu" v
  | true, Some (`Left padding), None -> pf ppf "%-*Lu" padding v
  | true, Some (`Zero padding), _ -> pf ppf "%0*Lu" padding v
  | true, Some (`Right padding), None -> pf ppf "%*Lu" padding v
  | true, None, Some precision -> pf ppf "%.*Lu" precision v
  | true, Some (`Left padding), Some precision ->
      pf ppf "%-*.*Lu" padding precision v
  | true, Some (`Right padding), Some precision ->
      pf ppf "%*.*Lu" padding precision v

let pp_float ?padding ?precision ppf v =
  match (padding, precision) with
  | None, None -> pf ppf "%f" v
  | Some (`Left padding), None -> pf ppf "%-*f" padding v
  | Some (`Zero padding), _ -> pf ppf "%0*f" padding v
  | Some (`Right padding), None -> pf ppf "%*f" padding v
  | None, Some precision -> pf ppf "%.*f" precision v
  | Some (`Left padding), Some precision -> pf ppf "%-*.*f" padding precision v
  | Some (`Right padding), Some precision -> pf ppf "%*.*f" padding precision v

let pp_string ?padding ppf v =
  match padding with
  | Some (`Left padding) -> pf ppf "%-*s" padding v
  | Some (`Right padding) -> pf ppf "%*s" padding v
  | _ -> pf ppf "%s" v

type wpd = Pd : ('v, 'r) padding -> wpd
type wpr = Pr : ('v, 'r) precision -> wpr

let is_flag = function '-' | '0' | '#' -> true | _ -> false
let is_dash = function '-' -> true | _ -> false
let is_zero = function '0' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_hash = function '#' -> true | _ -> false

type s = Sub.t
type 'r ebb = Ebb : ('x, 'r) fmt -> 'r ebb

type ('x, 'r) pdebb =
  | PdEbb : (_, 'x -> 'a) padding * ('a, 'r) fmt -> ('x, 'r) pdebb

type ('x, 'r) prebb =
  | PrEbb : (_, 'x -> 'a) precision * ('a, 'r) fmt -> ('x, 'r) prebb

type ('x, 'r) pdprebb =
  | PdPrEbb :
      ('u, 'v) padding * ('v, 'w -> 'a) precision * ('a, 'r) fmt
      -> ('w, 'r) pdprebb

let make_prebb : type a b. (a, b) precision -> (_, _) fmt -> (_, _) prebb =
 fun precision fmt ->
  match precision with
  | Nop -> PrEbb (Nop, fmt)
  | Lit v -> PrEbb (Lit v, fmt)
  | Arg -> PrEbb (Arg, fmt)

let make_pdebb : type a b. (a, b) padding -> (_, _) fmt -> (_, _) pdebb =
 fun padding fmt ->
  match padding with
  | Nop -> PdEbb (Nop, fmt)
  | Lit (k, v) -> PdEbb (Lit (k, v), fmt)
  | Arg v -> PdEbb (Arg v, fmt)

let make_pdprebb : type a b c d.
    (a, b) padding -> (c, d) precision -> (_, _) fmt -> (_, _) pdprebb =
 fun padding precision fmt ->
  let (PrEbb (precision, fmt)) = make_prebb precision fmt in
  match padding with
  | Nop -> PdPrEbb (Nop, precision, fmt)
  | Lit (k, v) -> PdPrEbb (Lit (k, v), precision, fmt)
  | Arg v -> PdPrEbb (Arg v, precision, fmt)

let rec parse_precision : type x v r.
    any:x Hmap.key -> bool -> (v, _) padding -> s -> r ebb =
 fun ~any alteration_flag padding s ->
  let open Sub in
  let dot = v "." in
  if is_prefix ~affix:dot s then
    let precision, s = span ~sat:is_digit (tail s) in
    match to_string precision with
    | "" -> parse_flag ~any alteration_flag padding Arg s
    | precision ->
        parse_flag ~any alteration_flag padding
          (Lit (int_of_string precision))
          s
  else parse_flag ~any alteration_flag padding Nop s

and parse_padding : type x r. any:x Hmap.key -> s -> r ebb =
 fun ~any s ->
  let open Sub in
  let flags, s = span ~sat:is_flag s in
  let left_padding = exists is_dash flags in
  let zero_padding = exists is_zero flags in
  let alteration_flag = exists is_hash flags in
  let padding, s = span ~sat:is_digit s in
  match (left_padding, zero_padding, to_string padding) with
  | false, false, "" -> parse_precision ~any alteration_flag Nop s
  | true, false, "" ->
      parse_precision ~any alteration_flag (Lit (`Right, 0)) s
      (* XXX(dinosaure): check "%-s" please! I don't know what it should do! *)
  | false, true, "" -> parse_precision ~any alteration_flag (Arg `Zero) s
  | false, false, pad ->
      parse_precision ~any alteration_flag (Lit (`Right, int_of_string pad)) s
  | true, false, pad ->
      parse_precision ~any alteration_flag (Lit (`Left, int_of_string pad)) s
  | false, true, pad ->
      parse_precision ~any alteration_flag (Lit (`Zero, int_of_string pad)) s
  | true, true, _ -> invalid_arg "Invalid padding flags"

and parse_flag : type x a b c d r.
    any:x Hmap.key -> bool -> (a, b) padding -> (c, d) precision -> s -> r ebb =
 fun ~any alteration_flag padding precision s ->
  let open Sub in
  match head s with
  | None -> invalid_arg "Invalid format: %S" (to_string s)
  | Some '%' ->
      let (Ebb fmt) = go ~any (tail s) in
      Ebb (Const (pp_string, "%") :: fmt)
  | Some 'a' ->
      let (Ebb fmt) = go ~any (tail s) in
      Ebb (Param :: fmt)
  | Some 'c' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb (Byte (Conv_c, padding, precision) :: fmt)
  | Some 'd' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb (Short (false, Conv_d, padding, precision) :: fmt)
  | Some 'x' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb
        (Short
           ( true,
             (if alteration_flag then Conv_Cx else Conv_x),
             padding,
             precision )
        :: fmt)
  | Some 'X' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb
        (Short
           ( true,
             (if alteration_flag then Conv_CX else Conv_X),
             padding,
             precision )
        :: fmt)
  | Some '!' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb (Atom (padding, precision, any) :: fmt)
  | Some 's' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      let padding = padding_and_precision_to_padding padding precision in
      Ebb (String padding :: fmt)
  | Some 'u' ->
      let (Ebb fmt) = go ~any (tail s) in
      let (PdPrEbb (padding, precision, fmt)) =
        make_pdprebb padding precision fmt
      in
      Ebb (Short (true, Conv_d, padding, precision) :: fmt)
  | Some chr -> invalid_arg "Invalid formatter %c" chr

and go : type x r. any:x Hmap.key -> s -> r ebb =
 fun ~any s ->
  let open Sub in
  let percent = v "%" in
  match cut ~sep:percent s with
  | None ->
      if is_empty s then Ebb [] else Ebb [ Const (pp_string, to_string s) ]
  | Some (x, s) ->
      if not (is_empty x) then
        let (Ebb fmt) = parse_padding ~any s in
        Ebb (Const (pp_string, to_string x) :: fmt)
      else parse_padding ~any s

and parse ~any fmt =
  let open Sub in
  go ~any (v fmt)

let coerce : type v0 r0 v1 r1. (v0, r0) fmt -> (v1, r1) ty -> (v1, r1) fmt =
 fun fmt ty ->
  match gen fmt ty with T (fmt, End) -> fmt | _ -> invalid_arg "coerce"

let of_string : type x v r. any:x Hmap.key -> string -> (v, r) ty -> (v, r) fmt
    =
 fun ~any s ty ->
  let (Ebb fmt) = parse ~any s in
  coerce fmt ty

let ty_of_string : type x. any:x Hmap.key -> string -> w =
 fun ~any s ->
  let (Ebb fmt) = parse ~any s in
  Ty (ty_of_fmt fmt)

let noop : _ order = Noop
let ignore : _ order = Ignore
let const pp v = Const (pp, v)
let ( $ ) = const

let padding = function
  | `Left -> fun x -> `Left x
  | `Right -> fun x -> `Right x
  | `Zero -> fun x -> `Zero x

let keval_padding : type ty v.
    formatter ->
    (ty, v) padding ->
    (formatter -> [ `Left of int | `Right of int | `Zero of int ] option -> v) ->
    ty =
 fun ppf padding k ->
  match padding with
  | Nop -> k ppf None
  | Lit (`Left, v) -> k ppf (Some (`Left v))
  | Lit (`Right, v) -> k ppf (Some (`Right v))
  | Lit (`Zero, v) -> k ppf (Some (`Zero v))
  | Arg `Left -> fun v -> k ppf (Some (`Left v))
  | Arg `Right -> fun v -> k ppf (Some (`Right v))
  | Arg `Zero -> fun v -> k ppf (Some (`Zero v))

let keval_precision : type ty v.
    formatter -> (ty, v) precision -> (formatter -> int option -> v) -> ty =
 fun ppf precision k ->
  match precision with
  | Nop -> k ppf None
  | Lit v -> k ppf (Some v)
  | Arg -> fun v -> k ppf (Some v)

let keval_order : type ty v.
    Hmap.t -> formatter -> (ty, v) order -> (formatter -> v) -> ty =
 fun pps ppf order k ->
  match order with
  | Ignore -> fun _ -> k ppf
  | Noop -> k ppf
  | Const (pp, v) ->
      pp ppf v;
      k ppf
  | Param ->
      fun pp v ->
        pp ppf v;
        k ppf
  | Atom (padding, precision, key) ->
      let k padding ppf precision v =
        let pp = Hmap.get key pps in
        pp ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k
  | String padding ->
      let k ppf padding v =
        pp_string ?padding ppf v;
        k ppf
      in
      keval_padding ppf padding k
  | Byte (conv, padding, precision) ->
      let k padding ppf precision v =
        pp_char ~conv ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k
  | Short (unsigned, conv, padding, precision) ->
      let k padding ppf precision v =
        pp_int ~unsigned ~conv ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k
  | Long (unsigned, conv, padding, precision) ->
      let k padding ppf precision v =
        pp_int32 ~unsigned ~conv ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k
  | Quad (unsigned, conv, padding, precision) ->
      let k padding ppf precision v =
        pp_int64 ~unsigned ~conv ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k
  | Float (padding, precision) ->
      let k padding ppf precision v =
        pp_float ?padding ?precision ppf v;
        k ppf
      in
      let k ppf padding = keval_precision ppf precision (k padding) in
      keval_padding ppf padding k

let rec keval : type ty v.
    Hmap.t -> formatter -> (ty, v) fmt -> (formatter -> v) -> ty =
 fun pps formatter fmt k ->
  match fmt with
  | [] -> k formatter
  | x :: r ->
      let k' formatter = keval pps formatter r k in
      keval_order pps formatter x k'
