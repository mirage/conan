type t = Str of string * int * int | App of t * t * int * int

let empty = ""

let empty = Str (empty, 0, 0)

let of_string ?(off = 0) ?len str =
  let len = match len with Some len -> len | None -> String.length str - off in
  Str (str, off, len)

let length = function Str (_, _, len) -> len | App (_, _, len, _) -> len

let height = function Str _ -> 0 | App (_, _, _, height) -> height

let app = function
  | Str (_, _, 0), t | t, Str (_, _, 0) -> t
  | Str (s0, off0, len0), Str (s1, off1, len1) when len0 + len1 < 256 ->
      let s = Bytes.create (len0 + len1) in
      Bytes.blit_string s0 off0 s 0 len0 ;
      Bytes.blit_string s1 off1 s len0 len1 ;
      Str (Bytes.unsafe_to_string s, 0, len0 + len1)
  | App (t0, Str (s1, off1, len1), _len0, _height0), Str (s2, off2, len2)
    when len1 + len2 < 256 ->
      let s = Bytes.create (len1 + len2) in
      Bytes.blit_string s1 off1 s 0 len1 ;
      Bytes.blit_string s2 off2 s len1 len2 ;
      App
        ( t0,
          Str (Bytes.unsafe_to_string s, 0, len1 + len2),
          length t0 + len1 + len2,
          1 + height t0 )
  | Str (s0, off0, len0), App (Str (s1, off1, len1), t2, _len2, _height2)
    when len0 + len1 < 256 ->
      let s = Bytes.create (len0 + len1) in
      Bytes.blit_string s0 off0 s 0 len0 ;
      Bytes.blit_string s1 off1 s len0 len1 ;
      App
        ( Str (Bytes.unsafe_to_string s, 0, len0 + len1),
          t2,
          len0 + len1 + length t2,
          1 + height t2 )
  | t0, t1 -> App (t0, t1, length t0 + length t1, max (height t0) (height t1))

let append a b = app (a, b)

let concat ~sep:str lst =
  let sep = Str (str, 0, String.length str) in
  let rec go t = function
    | [] -> t
    | [ x ] -> append t x
    | x :: r -> go (append x sep) r in
  go empty lst

let rec unsafe_blit t buf off len =
  match t with
  | Str (str0, off0, len0) ->
      let len' = min len0 len in
      Bytes.blit_string str0 off0 buf off len'
  | App (Str (str0, off0, len0), t1, _, _) ->
      let len' = min len0 len in
      Bytes.blit_string str0 off0 buf off len' ;
      unsafe_blit t1 buf (off + len') (len - len')
  | App (t0, t1, _, _) ->
      let len' = min (length t0) len in
      unsafe_blit t0 buf off len' ;
      unsafe_blit t1 buf (off + len') (len - len')

let to_string = function
  | Str (s, off, len) -> (s, off, len)
  | App (t0, t1, len, _) ->
      let res = Bytes.create len in
      let l0 = length t0 in
      let l1 = length t1 in
      unsafe_blit t0 res 0 l1 ;
      unsafe_blit t1 res l0 l1 ;
      (Bytes.unsafe_to_string res, 0, len)
