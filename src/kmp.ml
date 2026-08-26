open Sigs

type finder = {
  f :
    't 'fd 'error.
    't scheduler ->
    get:('fd -> pos:int64 -> ((char, 'error) result, 't) io) ->
    ln:int64 ->
    'fd ->
    (((int64 * int64) list, 'error) result, 't) io;
}
[@@unboxed]

let is_space = function
  | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
  | _ -> false

let is_lower chr = chr >= 'a' && chr <= 'z'
let is_upper chr = chr >= 'A' && chr <= 'Z'

let kmp ~pattern =
  let nlen = String.length pattern in
  let next = Array.make nlen 0 in
  let i = ref 1 in
  let j = ref 0 in
  if nlen > 1 then
    while !i < nlen - 1 do
      if pattern.[!i] = pattern.[!j] then (
        incr i;
        incr j;
        next.(!i) <- !j)
      else if !j = 0 then incr i
      else j := next.(!j)
    done;
  {
    f =
      (fun { bind; return } ~get ~ln fd ->
        let ( >>= ) = bind in
        let ( >>? ) x f =
          x >>= function Ok x -> f x | Error _ as err -> return err
        in
        let rec go pos idx =
          if idx < nlen && pos < ln then
            get fd ~pos >>? fun chr ->
            if pattern.[idx] = chr then go (Int64.succ pos) (succ idx)
            else if idx = 0 then go (Int64.succ pos) idx
            else go pos next.(idx)
          else if idx = nlen then
            return (Ok [ (Int64.sub pos (Int64.of_int nlen), pos) ])
          else return (Ok [])
        in
        go 0L 0);
  }

(* XXX(dinosaure): Like [strncmp()] because we can not implement KMP algorithm
   due to [w] and [W]. It returns multiple solutions. *)
let scan ~lower_case_insensitive ~upper_case_insensitive ~compact_whitespaces
    ~optional_blank ~pattern =
  let nlen = String.length pattern in
  {
    f =
      (fun { bind; return } ~get ~ln fd ->
        let ( >>= ) = bind in
        let matches start =
          let rec go idx pos =
            if idx >= nlen then return (Ok (Some pos))
            else
              let chr = pattern.[idx] in
              if compact_whitespaces && is_space chr then
                get fd ~pos >>= function
                | Error _ -> return (Ok None)
                | Ok chr' ->
                    if is_space chr' then
                      let idx = succ idx and pos = Int64.succ pos in
                      if idx < nlen && is_space pattern.[idx] then go idx pos
                      else blanks idx pos
                    else return (Ok None)
              else if optional_blank && is_space chr then blanks (succ idx) pos
              else
                get fd ~pos >>= function
                | Error _ -> return (Ok None)
                | Ok chr' ->
                    let equal =
                      if lower_case_insensitive && is_lower chr then
                        Char.lowercase_ascii chr' = chr
                      else if upper_case_insensitive && is_upper chr then
                        Char.uppercase_ascii chr' = chr
                      else chr' = chr
                    in
                    if equal then go (succ idx) (Int64.succ pos)
                    else return (Ok None)
          and blanks idx pos =
            get fd ~pos >>= function
            | Ok chr' when is_space chr' -> blanks idx (Int64.succ pos)
            | _ -> go idx pos
          in
          go 0 start
        in
        let rec next start =
          if Int64.add start (Int64.of_int nlen) > ln then return (Ok [])
          else
            matches start >>= function
            | Ok (Some stop) -> return (Ok [ (start, stop) ])
            | Ok None -> next (Int64.succ start)
            | Error _ as err -> return err
        in
        if nlen = 0 then return (Ok []) else next 0L);
  }

let find_one ?(lower_case_insensitive = false) ?(upper_case_insensitive = false)
    ?(compact_whitespaces = false) ?(optional_blank = false) ~pattern () =
  if
    lower_case_insensitive || upper_case_insensitive || compact_whitespaces
    || optional_blank
  then
    scan ~lower_case_insensitive ~upper_case_insensitive ~compact_whitespaces
      ~optional_blank ~pattern
  else kmp ~pattern
