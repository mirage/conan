let ( / ) = Filename.concat
let identity x = x

let ocamlify s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | '-' | '.' -> Buffer.add_char b '_'
      | _ -> ())
    s ;
  let s' = Buffer.contents b in
  if String.length s' = 0 || ('0' <= s'.[0] && s'.[0] <= '9')
  then raise (Invalid_argument s) ;
  s'

let reserved_keyword = [ ("virtual", "virtual'") ]

let ocamlify s =
  match List.assoc s reserved_keyword with
  | s' -> s'
  | exception Not_found -> ocamlify s

let serialize_into_multiple_files directory filename tree =
  let rec go (idx, acc) = function
    | [] -> List.rev acc
    | (elt, tree) :: r ->
      let filename' = Format.asprintf "%s_%03d.ml" (ocamlify filename) idx in
      let oc = open_out (directory / filename') in
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "let tree = @[<2>%a@]\n%!" Conan.Tree.serialize tree ;
      close_out oc ; go (succ idx, elt :: acc) r in
  let[@warning "-8"] Conan.Tree.Node lst = tree in
  let elts = go (0, []) lst in
  let filename' = Format.asprintf "%s.ml" (ocamlify filename) in
  let oc = open_out (directory / filename') in
  let ppf = Format.formatter_of_out_channel oc in
  List.iteri (fun idx elt ->
    Format.fprintf ppf "let tree_%03d = (@[<1>%a,@ %s_%03d.tree@])\n%!"
      idx Conan.Tree.serialize_elt elt (String.capitalize_ascii (ocamlify filename)) idx) elts ;
  Format.fprintf ppf "let tree = Conan.Tree.Unsafe.node @[%a@]\n%!"
    Conan.Serialize.(list (fmt "tree_%03d")) (List.init (List.length elts) identity) ;
  close_out oc

let serialize directory filename =
  let ic = open_in (directory / filename) in
  let rs = Conan.Parse.parse_in_channel ic in
  close_in ic ;
  match rs with
  | Ok lines ->
      let _, tree =
        List.fold_left
          (fun (line, tree) v ->
            (succ line, Conan.Tree.append ~filename ~line tree v))
          (1, Conan.Tree.empty) lines in
      if Conan.Tree.weight tree >= 2000
      then serialize_into_multiple_files directory filename tree
      else
        let filename' = ocamlify filename ^ ".ml" in
        let oc = open_out (directory / filename') in
        let ppf = Format.formatter_of_out_channel oc in
        Format.fprintf ppf "let tree = @[<2>%a@]\n%!" Conan.Tree.serialize tree ;
        close_out oc
  | Error _err ->
      let filename' = ocamlify filename ^ ".ml" in
      let oc = open_out (directory / filename') in
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "let tree = Conan.Tree.empty\n%!" ;
      close_out oc

let run directory =
  let files = Sys.readdir directory in
  let files = Array.to_list files in
  List.iter (serialize directory) files ;
  let files = List.map ocamlify files in
  let oc = open_out (directory / "conan_database.ml") in
  let ppf = Format.formatter_of_out_channel oc in
  List.iter
    (fun filename ->
      Format.fprintf ppf "let %s = %s.tree\n%!" filename
        (String.capitalize_ascii filename))
    files ;
  Format.fprintf ppf
    "let tree = List.fold_left Conan.Tree.merge Conan.Tree.empty @[%a@]\n%!"
    Conan.Serialize.(list (fmt "%s"))
    files ;
  close_out oc

let directory = ref None

let anonymous_argument v =
  match !directory with None -> directory := Some v | Some _ -> ()

let usage = Format.asprintf "%s database\n%!" Sys.argv.(0)

let exit_success = 0

let exit_failure = 1

let () =
  Arg.parse [] anonymous_argument usage ;
  match !directory with
  | None ->
      Format.eprintf "%s" usage ;
      exit exit_failure
  | Some directory ->
      run directory ;
      exit exit_success
