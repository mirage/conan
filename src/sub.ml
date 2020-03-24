(* Copyright (c) 2015 Daniel C. Bünzli. All rights reserved. *)

type t =
  { start : int
  ; stop : int
  ; s : string }

let v ?(start= 0) ?stop s =
  let s_len = String.length s in
  let stop = match stop with
    | None -> s_len
    | Some stop -> stop in
  if start < 0 || stop > s_len || stop < start
  then invalid_arg "Out of bounds" ;
  { start; stop; s; }

let empty = { start= 0; stop= 0; s= ""; }
let length { start; stop; _ } = stop - start

let head ?(rev = false) { s; start; stop; } =
  if start = stop then None else
  Some s.[if rev then stop - 1 else start]

let tail ?(rev= false) ({ s; start; stop; } as sub) =
  if start = stop then sub else
  if rev then { s; start; stop= stop - 1; }
  else { s; start= start + 1; stop; }

let is_empty { start; stop; _ } = stop - start = 0

let is_prefix ~affix:({ s= affix; start= astart; _ } as affix_sub) ({ s; start= sstart; _ } as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if affix.[astart + i] <> s.[sstart + i]
    then false
    else loop (i + 1)
  in loop 0

let is_suffix ~affix:({ s= affix; stop= astop; _ } as affix_sub) ({ s; stop= sstop; _ } as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let max_idx_a = astop - 1 in
  let max_idx_s = sstop - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if affix.[max_idx_a - i] <> s.[max_idx_s - i]
    then false
    else loop (i + 1)
  in loop 0

let fspan ~min ~max ~sat ({ s; start; stop; } as sub) =
  if min < 0 then invalid_arg "fspan" else
  if max < 0 then invalid_arg "fspan" else
  if min > max || max = 0 then ({ s; start; stop= start; }, sub) else
  let max_idx = stop - 1 in
  let max_idx =
    let k = start + max - 1 in (if k > max_idx || k < 0 then max_idx else k)
  in
  let need_idx = start + min in
  let rec loop i =
    if i <= max_idx && sat s.[i] then loop (i + 1) else
    if i < need_idx || i = 0 then ({ s; start; stop= start; }, sub) else
    if i = stop then (sub, { s; start= stop; stop; }) else
    { s; start; stop= i; }, { s; start= i; stop; }
  in loop start

let rspan ~min ~max ~sat ({ s; start; stop; } as sub) =
  if min < 0 then invalid_arg "rspan" else
  if max < 0 then invalid_arg "rspan" else
  if min > max || max = 0 then (sub, { s; start= stop; stop; }) else
  let max_idx = stop - 1 in
  let min_idx = let k = stop - max in if k < start then start else k in
  let need_idx = stop - min - 1 in
  let rec loop i =
    if i >= min_idx && sat s.[i] then loop (i - 1) else
    if i > need_idx || i = max_idx then (sub, { s; start= stop; stop; }) else
    if i = start - 1 then ({ s; start; stop= start; }, sub) else
    { s; start; stop= i + 1; }, { s; start= i + 1; stop; }
  in loop max_idx

let span ?(rev = false) ?(min = 0) ?(max = max_int) ?(sat = fun _ -> true) sub =
  match rev with
  | true  -> rspan ~min ~max ~sat sub
  | false -> fspan ~min ~max ~sat sub

let to_string { s; start; stop; } =
  if start = stop then "" else
  if start = 0 && stop = String.length s then s else
  String.sub s start (stop - start)

let rebase ({ start; stop; _ } as sub) = { s= to_string sub; start= 0; stop= stop - start; }

let concat ?sep:({ s= sep; start= sep_start; _ } as sep_sub = empty) = function
  | [] -> empty
  | [ s ] -> rebase s
  | ({ s; start; _ } as sub) :: ss ->
    let sub_len = length sub in
    let sep_len = length sep_sub in
    let rec cat_len sep_count l ss =
      if l < 0 then l else
      match ss with
      | s :: ss -> cat_len (sep_count + 1) (l + length s) ss
      | [] ->
          if sep_len = 0 then l else
          let max_sep_count = Sys.max_string_length / sep_len in
          if sep_count < 0 || sep_count > max_sep_count then -1 else
          sep_count * sep_len + l
    in
    let cat_len = cat_len 0 sub_len ss in
    if cat_len < 0 then invalid_arg "concat" else
    let b = Bytes.create cat_len in
    Bytes.blit_string s start b 0 sub_len ;
    let rec loop i = function
      | [] -> Bytes.unsafe_to_string b
      | ({ s= str; start= str_start; _ } as str_sub) :: ss ->
        let sep_pos = i in
        let str_pos = i + sep_len in
        let str_len = length str_sub in
        Bytes.blit_string sep sep_start b sep_pos sep_len;
        Bytes.blit_string str str_start b str_pos str_len;
        loop (str_pos + str_len) ss
    in { s= loop sub_len ss; start= 0; stop= cat_len }

let exists sat { s; start; stop; } =
  let rec loop i =
    if i > stop - 1 then false
    else if sat s.[i] then true
    else loop (succ i) in
  loop start

let for_all sat { s; start; stop; } =
  let rec loop i =
    if i > stop - 1 then true
    else if sat s.[i] then loop (succ i)
    else false in
  loop start

let is_white = function
  | ' ' | '\t' .. '\r' -> true | _ -> false

let trim ?(drop = is_white) ({ s; start; stop; } as sub) =
  let len = stop - start in
  if len = 0 then sub else
  let max_pos = stop in
  let max_idx = stop - 1 in
  let rec left_pos i =
    if i > max_idx then max_pos else
    if drop s.[i] then left_pos (i + 1) else i
  in
  let rec right_pos i =
    if i < start then start else
    if drop s.[i] then right_pos (i - 1) else (i + 1)
  in
  let left = left_pos start in
  if left = max_pos then { s; start= (start + stop) / 2; stop= (start + stop) / 2 } else
  let right = right_pos max_idx in
  if left = start && right = max_pos then sub else
  { s; start= left; stop= right; }

let equal_bytes { s= s0; start= start0; stop= stop0; } { s= s1; start= start1; stop= stop1; } =
  if s0 == s1 && start0 = start1 && stop0 = stop1 then true else
  let len0 = stop0 - start0 in
  let len1 = stop1 - start1 in
  if len0 <> len1 then false else
  let max_zidx = len0 - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if s0.[start0 + i] <> s1.[start1 + i]
    then false
    else loop (i + 1)
  in loop 0

let fcut ~sep:{ s= sep; start= sep_start; stop= sep_stop; } { s; start; stop; } =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg "fcut" else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ({ s; start; stop= i; }, { s; start= i + sep_len; stop; })
    else if s.[i + k] = sep.[sep_start + k]
    then check_sep i (k + 1)
    else scan (i + 1)
  and scan i =
    if i > max_s_idx then None else
    if s.[i] = sep.[sep_start]
    then check_sep i 1
    else scan (i + 1)
  in scan start

let rcut ~sep:{ s= sep; start= sep_start; stop= sep_stop; } { s; start; stop; } =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg "rcut" else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ({ s; start; stop= i; }, { s; start= i + sep_len; stop; })
    else if s.[i + k] = sep.[sep_start + k]
    then check_sep i (k + 1)
    else rscan (i - 1)
  and rscan i =
    if i < start then None else
    if s.[i] = sep.[sep_start]
    then check_sep i 1
    else rscan (i - 1)
  in rscan (max_s_idx - max_sep_zidx)

let cut ?(rev= false) ~sep s = match rev with
  | true  -> rcut ~sep s
  | false -> fcut ~sep s

let with_range ?(first = 0) ?(len = max_int) { s; start; stop; } =
  if len < 0 then invalid_arg "with_range" else
  let s_len = stop - start in
  let max_idx = s_len - 1 in
  let empty = function
    | first when first < 0 -> { s; start; stop= start; }
    | first when first > max_idx -> { s; start= stop; stop; }
    | first -> { s; start= start + first; stop= start + first; } in
  if len = 0 then empty first else
  let last (* index *) = match len with
    | len when len = max_int -> max_idx
    | len ->
      let last = first + len - 1 in
      if last > max_idx then max_idx else last in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty first else
  { s; start= start + first; stop= start + last + 1 (* position *); }

let drop ?(rev= false) ?min ?max ?sat t =
  (if rev then fst else snd) @@ span ~rev ?min ?max ?sat t

let add_sub ~no_empty s ~start ~stop acc =
  if start = stop then ( if no_empty then acc else { s; start; stop= start; } :: acc ) else
  { s; start; stop; } :: acc

let fcuts ~no_empty ~sep:{ s= sep; start= sep_start; stop= sep_stop; } ({ s; start; stop; } as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg "fcuts" else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep sstart i k acc =
    if k > max_sep_zidx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub ~no_empty s ~start:sstart ~stop:i acc)
    else
      if s.[i + k] = sep.[sep_start + k]
      then check_sep sstart i (k + 1) acc
      else scan sstart (i + 1) acc
  and scan sstart i acc =
    if i > max_s_idx then
      if sstart = start then (if no_empty && s_len = 0 then [] else [sub]) else
      List.rev (add_sub ~no_empty s ~start:sstart ~stop acc)
    else
      if s.[i] = sep.[sep_start]
      then check_sep sstart i 1 acc
      else scan sstart (i + 1) acc
  in scan start start []

let rcuts ~no_empty ~sep:{ s= sep; start= sep_start; stop= sep_stop; } ({ s; start; stop; } as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg "rcuts" else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep sstop i k acc =
    if k > max_sep_zidx then
      let start = i + sep_len in
      rscan i (i - sep_len) (add_sub ~no_empty s ~start ~stop:sstop acc)
    else
      if s.[i + k] = sep.[sep_start + k]
      then check_sep sstop i (k + 1) acc
      else rscan sstop (i - 1) acc
  and rscan sstop i acc =
    if i < start then
      if sstop = stop then (if no_empty && s_len = 0 then [] else [sub]) else
      add_sub ~no_empty s ~start ~stop:sstop acc
    else
      if s.[i] = sep.[sep_start]
      then check_sep sstop i 1 acc
      else rscan sstop (i - 1) acc in
  rscan stop (max_s_idx - max_sep_zidx) []

let cuts ?(rev = false) ?(empty = true) ~sep s = match rev with
  | true  -> rcuts ~no_empty:(not empty) ~sep s
  | false -> fcuts ~no_empty:(not empty) ~sep s
