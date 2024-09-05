type 'a t = Format.formatter -> 'a -> unit

let cut ppf _ = Format.pp_print_cut ppf ()
let fmt fmt ppf = Format.fprintf ppf fmt
let char ppf chr = Format.fprintf ppf "'\\%03d'" (Char.code chr)

let int ppf n =
  if n >= 0 then Format.fprintf ppf "%d" n
  else Format.fprintf ppf "@[<1>(%d)@]" n

let int32 ppf n =
  if n >= 0l then Format.fprintf ppf "%ldl" n
  else Format.fprintf ppf "@[<1>(%ldl)@]" n

let int64 ppf n =
  if n >= 0L then Format.fprintf ppf "%LdL" n
  else Format.fprintf ppf "@[<1>(%LdL)@]" n

let float ppf n = Format.fprintf ppf "%f" n

let ptime_span ppf span =
  let d, ps = Ptime.Span.to_d_ps span in
  Format.fprintf ppf "Conan.Ptime.Span.v @[<1>(%d, %LdL)@]" d ps

let string ppf str = Format.fprintf ppf "%S" str

let option pp ppf = function
  | None -> Format.fprintf ppf "None"
  | Some v -> Format.fprintf ppf "Some@ %a" pp v

let box ?(indent = 0) pp ppf v =
  Format.(
    pp_open_box ppf indent;
    pp ppf v;
    pp_close_box ppf ())

let surround s0 s1 pp ppf v =
  Format.(
    pp_print_string ppf s0;
    pp ppf v;
    pp_print_string ppf s1)

let brackets pp = box ~indent:1 (surround "[" "]" pp)
let parens pp = box ~indent:1 (surround "(" ")" pp)

let iter ?sep:(pp_sep = cut) iter pp ppf v =
  let is_first = ref true in
  let pp v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp ppf v
  in
  iter pp v

let ( ++ ) pp0 pp1 ppf v =
  pp0 ppf v;
  pp1 ppf v

let using f pp ppf v = pp ppf (f v)
let list ?sep pp = iter ?sep List.iter pp

let semi ppf _ =
  Format.pp_print_string ppf ";";
  Format.pp_print_space ppf ()

let comma ppf _ =
  Format.pp_print_string ppf ",";
  Format.pp_print_space ppf ()

let list pp = brackets (list ~sep:semi (box pp))
let pair pp0 pp1 = parens (using fst (box pp0) ++ comma ++ using snd (box pp1))

module Re_serialize = struct
  let rec set ppf cset =
    let len = ref 0 in
    let lst = ref [] in
    Re__Cset.iter cset ~f:(fun chr _chr' ->
        lst := Re__Cset.to_int chr :: !lst;
        incr len);
    let res = Bytes.create !len in
    List.iteri
      (fun pos chr -> Bytes.set res pos (Char.chr (chr land 0xff)))
      !lst;
    Format.fprintf ppf "@[<2>Re.set@ %S@]" (Bytes.unsafe_to_string res)

  and sequence ppf lst =
    Format.fprintf ppf "@[<2>Re.seq@ @[%a@]@]" (list view) lst

  and alternative ppf lst =
    Format.fprintf ppf "@[<2>Re.alt@ @[%a@]@]" (list view) lst

  and repeat ppf (t, i, j) =
    Format.fprintf ppf "@[<2>Re.repn@ @[%a@]@ %d@ @[%a@]@]" (parens view) t i
      (parens (option int))
      j

  and beg_of_line ppf () = Format.pp_print_string ppf "Re.bol"
  and end_of_line ppf () = Format.pp_print_string ppf "Re.eol"
  and beg_of_word ppf () = Format.pp_print_string ppf "Re.bow"
  and end_of_word ppf () = Format.pp_print_string ppf "Re.eow"
  and not_bound ppf () = Format.pp_print_string ppf "Re.not_boundary"
  and beg_of_str ppf () = Format.pp_print_string ppf "Re.bos"
  and end_of_str ppf () = Format.pp_print_string ppf "Re.eos"
  and last_end_of_line ppf () = Format.pp_print_string ppf "Re.leol"
  and start ppf () = Format.pp_print_string ppf "Re.start"
  and stop ppf () = Format.pp_print_string ppf "Re.stop"

  and sem ppf (sem, t) =
    match sem with
    | `Longest -> Format.fprintf ppf "@[<2>Re.longest@ @[%a@]@]" (parens view) t
    | `Shortest ->
        Format.fprintf ppf "@[<2>Re.shortest@ @[%a@]@]" (parens view) t
    | `First -> Format.fprintf ppf "@[<2>Re.first@ @[%a@]@]" (parens view) t

  and sem_greedy ppf (kind, t) =
    match kind with
    | `Greedy -> Format.fprintf ppf "@[<2>Re.greedy@ @[%a@]@]" (parens view) t
    | `Non_greedy ->
        Format.fprintf ppf "@[<2>Re.non_greedy@ @[%a@]@]" (parens view) t

  and group ppf t = Format.fprintf ppf "@[<2>Re.group @[%a@]@]" (parens view) t

  and no_group ppf t =
    Format.fprintf ppf "@[<2>Re.no_group @[%a@]@]" (parens view) t

  and nest ppf t = Format.fprintf ppf "@[<2>Re.nest @[%a@]@]" (parens view) t
  and case ppf t = Format.fprintf ppf "@[<2>Re.case @[%a@]@]" (parens view) t

  and no_case ppf t =
    Format.fprintf ppf "@[<2>Re.no_case @[%a@]@]" (parens view) t

  and intersection ppf ts =
    Format.fprintf ppf "@[<2>Re.inter@ @[%a@]@]" (list view) ts

  and complement ppf ts =
    Format.fprintf ppf "@[<2>Re.compl@ @[%a@]@]" (list view) ts

  and difference ppf (ta, tb) =
    Format.fprintf ppf "@[<2>Re.diff@ @[%a@]@ @[%a@]@]" (parens view) ta
      (parens view) tb

  and pmark ppf (_idx, t) =
    Format.fprintf ppf "@[<2>Re.mark@ @[%a@]@ |>@ snd@]" (parens view) t
  (* XXX(dinosaure): [assert false]? *)

  and view ppf = function
    | Re.View.Set cset -> set ppf cset
    | Re.View.Sequence ts -> sequence ppf (List.map Re.View.view ts)
    | Re.View.Alternative ts -> alternative ppf (List.map Re.View.view ts)
    | Re.View.Repeat (t, i, j) -> repeat ppf (Re.View.view t, i, j)
    | Re.View.Beg_of_line -> beg_of_line ppf ()
    | Re.View.End_of_line -> end_of_line ppf ()
    | Re.View.Beg_of_word -> beg_of_word ppf ()
    | Re.View.End_of_word -> end_of_word ppf ()
    | Re.View.Not_bound -> not_bound ppf ()
    | Re.View.Beg_of_str -> beg_of_str ppf ()
    | Re.View.End_of_str -> end_of_str ppf ()
    | Re.View.Last_end_of_line -> last_end_of_line ppf ()
    | Re.View.Start -> start ppf ()
    | Re.View.Stop -> stop ppf ()
    | Re.View.Sem (s, t) -> sem ppf (s, Re.View.view t)
    | Re.View.Sem_greedy (kind, t) -> sem_greedy ppf (kind, Re.View.view t)
    | Re.View.Group (_, t) -> group ppf (Re.View.view t)
    | Re.View.No_group t -> no_group ppf (Re.View.view t)
    | Re.View.Nest t -> nest ppf (Re.View.view t)
    | Re.View.Case t -> case ppf (Re.View.view t)
    | Re.View.No_case t -> no_case ppf (Re.View.view t)
    | Re.View.Intersection ts -> intersection ppf (List.map Re.View.view ts)
    | Re.View.Complement ts -> complement ppf (List.map Re.View.view ts)
    | Re.View.Difference (ta, tb) ->
        difference ppf (Re.View.view ta, Re.View.view tb)
    | Re.View.Pmark (mark, t) -> pmark ppf (mark, Re.View.view t)
end

let re ppf re = Re_serialize.view ppf (Re.View.view re)
