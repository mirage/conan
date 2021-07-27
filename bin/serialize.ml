let run directory =
  let tree = Conan_unix.database ~directory in
  Conan.Tree.serialize Format.std_formatter tree ;
  Format.printf "%!"

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
