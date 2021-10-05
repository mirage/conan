open Sigs

type finder = {
  f :
    't 'fd 'error.
    't scheduler ->
    get:('fd -> pos:int64 -> ((char, 'error) result, 't) io) ->
    ln:int64 ->
    'fd ->
    ((int64 list, 'error) result, 't) io;
}
[@@unboxed]

let find_one ~pattern =
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
            return (Ok [ Int64.sub pos (Int64.of_int nlen) ])
          else return (Ok [])
        in
        go 0L 0);
  }
