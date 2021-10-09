#load "unix.cma"

let pp_filename ppf str = Format.fprintf ppf "%s" str

let pp_module ppf str =
  Format.fprintf ppf "%s" Filename.(basename (remove_extension str))

let pp_sep ppf () = Format.fprintf ppf "@ "

let pp_list ~sep:pp_sep pp ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: r ->
        Format.fprintf ppf "%a%a" pp x pp_sep ();
        go r
  in
  go lst

let run database output =
  let modules =
    Unix.open_process_in
      (Format.asprintf "conan.serialize --dry-run %s -o %s" database output)
  in
  let modules =
    let rec go acc =
      match input_line modules with
      | m -> go (m :: acc)
      | exception _ -> List.rev acc
    in
    go []
  in
  let modules = List.sort String.compare modules in
  let oc = open_out "dune.inc.gen" in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf
    {dune|(rule
 (targets @[<hov>%a@])
 (deps %%{bin:conan.serialize} (source_tree %s))
 (action
  (run conan.serialize %s -o %s)))

(library
 (name conan_database)
 (modules @[<hov>%a@])
 (libraries conan))
%!|dune}
    (pp_list ~sep:pp_sep pp_filename)
    modules database database output
    (pp_list ~sep:pp_sep pp_module)
    modules;
  close_out oc

let database = ref None

let output = ref None

let anonymous_argument v =
  match !database with None -> database := Some v | Some _ -> ()

let usage = Format.asprintf "%s <database> [-o <output>]\n%!" Sys.argv.(0)

let spec =
  [
    ( "-o",
      Arg.String (fun str -> output := Some str),
      "The directory destination where the conan_database.ml will be made. By \
       default, we emit the OCaml code into the given database directory." );
  ]

let exit_success = 0

let exit_failure = 1

let () =
  Arg.parse spec anonymous_argument usage;
  match !database with
  | None ->
      Format.eprintf "%s" usage;
      exit exit_failure
  | Some database ->
      let output =
        match !output with Some output -> output | None -> database
      in
      run database output;
      exit exit_success
