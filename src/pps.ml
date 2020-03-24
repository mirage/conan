let char_to_int : char Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_char_to_int ?padding ?precision ppf v =
  Fmt.pp_int ?padding ?precision ppf (Char.code v)

let int32_to_int : int32 Fmt.Hmap.Key.key = Fmt.Hmap.Key.create ()

let pp_int32_to_int ?padding ?precision ppf v =
  Fmt.pp_int32 ?padding ?precision ppf v

let v =
  let open Fmt.Hmap in
  empty
  |> add char_to_int pp_char_to_int
  |> add int32_to_int pp_int32_to_int
