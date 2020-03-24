type t =
  { output : string
  ; mime : string option }

let pf = Format.fprintf

let pp ppf t =
  let pp_option pp_val ppf = function Some v -> pp_val ppf v | None -> () in
  let pp_string ppf v = pf ppf "%s" v in
  pf ppf "{ @[<hov>output= %S;@ \
                   mime= %a;@] }"
    t.output (pp_option pp_string) t.mime

let with_mime mime t = { t with mime= Some mime }
let with_output output t = { t with output }

let output { output; _ } = output
let mime { mime; _ } = mime
let empty = { output= ""; mime= None }
