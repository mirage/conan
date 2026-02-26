let name = ref "conan_magic_database.ml"
let ( / ) = Filename.concat
let identity x = x

let ocamlify s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | '-' | '.' -> Buffer.add_char b '_'
      | _ -> ())
    s;
  let s' = Buffer.contents b in
  if String.length s' = 0 || ('0' <= s'.[0] && s'.[0] <= '9') then
    raise (Invalid_argument s);
  s'

let reserved_keyword = [ ("virtual", "virtual'") ]

let ocamlify s =
  match List.assoc s reserved_keyword with
  | s' -> s'
  | exception Not_found -> ocamlify s

let serialize_into_multiple_files output filename tree =
  let rec go (idx, acc) = function
    | [] -> List.rev acc
    | (elt, tree) :: r ->
        let filename' =
          Format.asprintf "%s_%03d.ml" (ocamlify ("conan_" ^ filename)) idx
        in
        let oc = open_out (output / filename') in
        let ppf = Format.formatter_of_out_channel oc in
        Format.fprintf ppf "let tree = @[<2>%a@]\n%!" Conan.Tree.serialize tree;
        close_out oc;
        go (succ idx, elt :: acc) r
  in
  let[@warning "-8"] (Conan.Tree.Node lst) = tree in
  let elts = go (0, []) lst in
  let filename' = Format.asprintf "%s.ml" (ocamlify ("conan_" ^ filename)) in
  let oc = open_out (output / filename') in
  let ppf = Format.formatter_of_out_channel oc in
  List.iteri
    (fun idx elt ->
      Format.fprintf ppf "let tree_%03d = (@[<1>%a,@ %s_%03d.tree@])\n%!" idx
        Conan.Tree.serialize_elt elt
        (String.capitalize_ascii (ocamlify ("conan_" ^ filename)))
        idx)
    elts;
  Format.fprintf ppf "let tree = Conan.Tree.Unsafe.node @[%a@]\n%!"
    Conan.Serialize.(list (fmt "tree_%03d"))
    (List.init (List.length elts) identity);
  close_out oc

let serialize only_mime database output filename =
  let ic = open_in (database / filename) in
  let rs = Conan.Parse.parse_in_channel ic in
  close_in ic;
  match rs with
  | Ok lines ->
      let _, tree =
        List.fold_left
          (fun (line, tree) v ->
            (succ line, Conan.Tree.append ~filename ~line tree v))
          (1, Conan.Tree.empty) lines
      in
      let tree =
        if only_mime then
          Conan.Process.only_mime_paths (Conan.Process.database ~tree)
        else tree
      in
      if Conan.Tree.weight tree >= 2000 then
        serialize_into_multiple_files output filename tree
      else
        let filename' = ocamlify ("conan_" ^ filename) ^ ".ml" in
        let oc = open_out (output / filename') in
        let ppf = Format.formatter_of_out_channel oc in
        Format.fprintf ppf "let tree = @[<2>%a@]\n%!" Conan.Tree.serialize tree;
        close_out oc
  | Error _err ->
      let filename' = ocamlify ("conan_" ^ filename) ^ ".ml" in
      let oc = open_out (output / filename') in
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "let tree = Conan.Tree.empty\n%!";
      close_out oc

let simulate only_mime database output filename =
  let ic = open_in (database / filename) in
  let rs = Conan.Parse.parse_in_channel ic in
  close_in ic;
  match rs with
  | Ok lines ->
      let _, tree =
        List.fold_left
          (fun (line, tree) v ->
            (succ line, Conan.Tree.append ~filename ~line tree v))
          (1, Conan.Tree.empty) lines
      in
      let tree =
        if only_mime then
          Conan.Process.only_mime_paths (Conan.Process.database ~tree)
        else tree
      in
      if Conan.Tree.weight tree >= 2000 then
        let[@warning "-8"] (Conan.Tree.Node lst) = tree in
        (output / Format.asprintf "%s.ml" (ocamlify ("conan_" ^ filename)))
        :: List.mapi
             (fun idx _ ->
               output
               / Format.asprintf "%s_%03d.ml"
                   (ocamlify ("conan_" ^ filename))
                   idx)
             lst
      else [ output / (ocamlify ("conan_" ^ filename) ^ ".ml") ]
  | Error _err -> [ output / (ocamlify ("conan_" ^ filename) ^ ".ml") ]

let run only_mime database output =
  let files = Sys.readdir database in
  let files = Array.to_list files in
  let files = List.sort String.compare files in
  List.iter (serialize only_mime database output) files;
  let files = List.map (fun file -> ocamlify ("conan_" ^ file)) files in
  let oc = open_out (output / !name) in
  let ppf = Format.formatter_of_out_channel oc in
  List.iter
    (fun filename ->
      Format.fprintf ppf "let %s = %s.tree\n%!" filename
        (String.capitalize_ascii filename))
    files;
  Format.fprintf ppf
    "let tree = List.fold_left Conan.Tree.merge Conan.Tree.empty @[%a@]\n%!"
    Conan.Serialize.(list (fmt "%s"))
    files;
  close_out oc

let dry_run only_mime database output =
  let files = Sys.readdir database in
  let files = Array.to_list files in
  let files = List.sort String.compare files in
  let files = List.map (simulate only_mime database output) files in
  let files = List.concat files in
  List.iter (Format.printf "%s\n%!") ((output / !name) :: files)

let database = ref None
let output = ref None
let dry_run_flag = ref false
let only_mime_flag = ref false

let anonymous_argument v =
  match !database with None -> database := Some v | Some _ -> ()

let usage =
  Format.asprintf
    "%s [--only-mime] [--dry-run] [--name <name>] <database> [-o <output>]\n%!"
    Sys.argv.(0)

let spec =
  [
    ( "-o",
      Arg.String (fun str -> output := Some str),
      "The directory destination where the conan_database.ml/<name> will be \
       made. By default, we emit the OCaml code into the given database \
       directory." );
    ( "--only-mime",
      Arg.Set only_mime_flag,
      "Serialize only a tree which informs about the MIME type (a lite version \
       of the database)." );
    ( "--dry-run",
      Arg.Set dry_run_flag,
      "Show the list of produced OCaml files according to the given database \
       directory." );
    ( "--name",
      Arg.Set_string name,
      "Generate a <name> OCaml file instead of (by default), the \
       conan_database.ml which contains the whole database." );
  ]

let exit_success = 0
let exit_failure = 1

let () =
  Arg.parse spec anonymous_argument usage;
  match !database with
  | None ->
      Format.eprintf "%s" usage;
      exit exit_failure
  | Some database when !dry_run_flag ->
      let output =
        match !output with Some output -> output | None -> database
      in
      dry_run !only_mime_flag database output;
      exit exit_success
  | Some database ->
      let output =
        match !output with Some output -> output | None -> database
      in
      run !only_mime_flag database output;
      exit exit_success
