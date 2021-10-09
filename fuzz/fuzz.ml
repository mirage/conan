let ( / ) = Filename.concat

let conan_database =
  match Sys.getenv "CONAN" with
  | path -> path
  | exception Not_found -> "database"

let database =
  let rec go tree = function
    | [] -> tree
    | filename :: rest -> (
        let ic = open_in (conan_database / filename) in
        let rs = Conan.Parse.parse_in_channel ic in
        close_in ic;
        match rs with
        | Ok lines ->
            let _, tree =
              List.fold_left
                (fun (line, tree) v ->
                  (succ line, Conan.Tree.append ~filename ~line tree v))
                (1, tree) lines
            in
            go tree rest
        | _ -> go tree rest)
  in
  Sys.readdir conan_database |> Array.to_list |> go Conan.Tree.empty
  |> fun tree -> Conan.Process.database ~tree

open Crowbar

let () =
  add_test ~name:"lint (assert false / exception)" [ bytes ] @@ fun str ->
  match Conan_string.run ~database str with
  | Error (`Msg err) -> Fmt.epr "Got an error for the given input: %s.\n%!" err
  | Ok metadata -> (
      match (Conan.Metadata.mime metadata, Conan.Metadata.output metadata) with
      | Some mime, None -> Fmt.pr "Found MIME type: %S.\n%!" mime
      | None, Some output ->
          Fmt.pr "Found a solution for the given input: %S.\n%!" output
      | Some mime, Some output -> Fmt.pr "%S (%S)\n%!" output mime
      | None, None -> ())
