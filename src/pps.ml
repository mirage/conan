let char_to_int : char Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_char_to_int ?padding ?precision ppf v =
  Fmt.pp_int ~unsigned:false ~conv:Fmt.Conv_d ?padding ?precision ppf
    (Char.code v)

let int32_to_int : int32 Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_int32_to_int ?padding ?precision ppf v =
  Fmt.pp_int32 ~unsigned:false ~conv:Fmt.Conv_d ?padding ?precision ppf v

let int64_to_int : int64 Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_int64_to_int ?padding ?precision ppf v =
  Fmt.pp_int64 ~unsigned:false ~conv:Fmt.Conv_d ?padding ?precision ppf v

let ropes_to_string : Ropes.t Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_ropes_to_string ?padding ?precision:_ ppf v =
  let str, off, len = Ropes.to_string v in
  Fmt.pp_string ?padding ppf (String.sub str off len)

let v =
  let open Fmt.Hmap in
  empty
  |> add char_to_int pp_char_to_int
  |> add int32_to_int pp_int32_to_int
  |> add int64_to_int pp_int64_to_int
  |> add ropes_to_string pp_ropes_to_string
