open Sigs

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
