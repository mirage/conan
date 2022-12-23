type t = { output : string option; mime : string option }

let pf = Format.fprintf

let pp ppf t =
  let pp_option pp_val ppf = function Some v -> pp_val ppf v | None -> () in
  let pp_string ppf v = pf ppf "%s" v in
  pf ppf "{ @[<hov>output= %S;@ mime= %a;@] }"
    (Option.value ~default:"" t.output)
    (pp_option pp_string) t.mime

let with_mime mime t = { t with mime = Some mime }

let with_output output t =
  if output <> "" then
    match t.output with
    | Some _ -> { t with output = Some output }
    | None when output.[0] = ' ' ->
        {
          t with
          output = Some (String.sub output 1 (String.length output - 1));
        }
    | None -> { t with output = Some output }
  else t

let output { output; _ } = output
let clear { mime; _ } = { output = None; mime }
let mime { mime; _ } = mime
let empty = { output = None; mime = None }

let concat a0 a1 =
  let output =
    match (a0.output, a1.output) with
    | None, Some v -> Some v
    | Some v, None -> Some v
    | Some a, Some b -> Some (a ^ b)
    | None, None -> None
  in
  let mime =
    match (a0.mime, a1.mime) with
    | None, Some v -> Some v
    | Some v, None -> Some v
    | Some v, Some _ -> Some v
    | None, None -> None
  in
  { output; mime }
