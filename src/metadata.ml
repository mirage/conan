type t = {
  output : string option;
  mime : string option;
  extensions : string list;
}

let pf = Format.fprintf

let pp ppf t =
  let pp_option pp_val ppf = function Some v -> pp_val ppf v | None -> () in
  let pp_list pp_val ppf lst =
    let rec go = function
      | [] -> ()
      | [ x ] -> pp_val ppf x
      | x :: r ->
          Format.fprintf ppf "%a;@ " pp_val x;
          go r
    in
    Format.pp_open_box ppf 1;
    Format.fprintf ppf "[";
    go lst;
    Format.fprintf ppf "]";
    Format.pp_close_box ppf ()
  in
  let pp_string ppf v = pf ppf "%s" v in
  pf ppf "{ @[<hov>output= %S;@ mime= %a; extensions= %a;@] }"
    (Option.value ~default:"" t.output)
    (pp_option pp_string) t.mime (pp_list pp_string) t.extensions

let with_mime mime t = { t with mime = Some mime }

let with_extensions exts t =
  let extensions =
    List.fold_left
      (fun acc v -> if List.mem v acc then acc else v :: acc)
      t.extensions exts
  in
  { t with extensions }

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
let clear { mime; extensions; _ } = { output = None; mime; extensions }
let mime { mime; _ } = mime
let extensions { extensions; _ } = extensions
let empty = { output = None; mime = None; extensions = [] }

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
  let extensions = List.merge String.compare a0.extensions a1.extensions in
  let extensions = List.sort_uniq String.compare extensions in
  { output; mime; extensions }
