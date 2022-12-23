let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

open Sigs

let process_fmt :
    type test v. Metadata.t -> (test, v) Ty.t -> v Tree.fmt -> v -> Metadata.t =
 fun m t tree_fmt v ->
  match t with
  | Ty.Clear -> Metadata.clear m
  | _ ->
      let buf = Buffer.create 16 in
      let ppf = Format.formatter_of_buffer buf in
      Fmt.keval Pps.v ppf
        Fmt.([ String Nop ] ^^ Tree.fmt tree_fmt ())
        (fun ppf ->
          Format.fprintf ppf "%!";
          Metadata.with_output (Buffer.contents buf) m)
        (Option.value ~default:"" (Metadata.output m))
        v

let process :
    type s fd error.
    s scheduler ->
    (fd, error, s) syscall ->
    fd ->
    int64 ->
    Metadata.t ->
    Tree.operation ->
    ( ( int64 * Metadata.t,
        [> `Syscall of error | `Invalid_test | `No_process ] )
      result,
      s )
    io =
 fun ({ bind; return } as scheduler) syscall fd abs_offset metadata operation ->
  match operation with
  | Tree.Name _ -> return (Error `No_process)
  | Tree.Use _ -> return (Error `No_process)
  | Tree.MIME v -> return (Ok (abs_offset, Metadata.with_mime v metadata))
  | Tree.Rule (offset, ty, test, fmt) -> (
      let ( >>= ) = bind in
      let ( >|= ) x f = x >>= fun x -> return (f x) in
      let ( >?= ) x f =
        x >>= function Ok x -> f x | Error err -> return (Error err)
      in
      Offset.process scheduler syscall fd offset abs_offset
      >|= reword_error (fun err -> `Syscall err)
      >?= fun abs_offset ->
      Ty.process scheduler syscall fd abs_offset ty >?= fun v ->
      match Test.process ty test v with
      | Some v ->
          let metadata = process_fmt metadata ty fmt v in
          return (Ok (abs_offset, metadata))
      | None -> return (Error `Invalid_test))

let descending_walk ({ bind; return } as scheduler) syscall db fd abs_offset
    metadata root =
  let ( >>= ) = bind in

  (* ugly, as f*ck! *)
  let rec go ~level syscall abs_offset candidate0 = function
    | Tree.Done -> return candidate0
    | Tree.Node lst ->
        let lst =
          List.rev_map (fun (elt, sub) -> (Tree.operation elt, sub)) lst
        in
        iter ~level [] syscall abs_offset candidate0 lst
  and iter ~level results syscall abs_offset candidate1 = function
    | [] -> return candidate1
    | (Tree.Name _, _) :: rest ->
        iter ~level results syscall abs_offset candidate1 rest
    | (Tree.Use { offset; invert = false; name }, Tree.Done) :: rest -> (
        Offset.process scheduler syscall fd offset abs_offset >>= function
        | Ok shift ->
            let seek fd abs_offset where =
              syscall.seek fd (Int64.add abs_offset shift) where
            in
            if not (Hashtbl.mem db name) then
              invalid_arg "%s does not exist" name;
            let tree = Hashtbl.find db name in
            go { syscall with seek } ~level:(succ level) 0L
              (* XXX(dinosaure): or [abs_offset]? *) candidate1 tree
            >>= fun candidate2 ->
            iter ~level results syscall abs_offset candidate2 rest
        | Error _ -> iter ~level results syscall abs_offset candidate1 rest)
    | (Tree.Use { offset; invert = true; name }, Tree.Done) :: rest -> (
        Offset.process scheduler syscall fd offset abs_offset >>= function
        | Ok shift ->
            let seek fd abs_offset where =
              syscall.seek fd (Int64.add abs_offset shift) where
            in
            if not (Hashtbl.mem db name) then
              invalid_arg "%s does not exist" name;
            let tree = Hashtbl.find db name in
            go
              (Size.invert scheduler { syscall with seek })
              ~level:(succ level) 0L
              (* XXX(dinosaure): or [abs_offset]? *) candidate1 tree
            >>= fun candidate2 ->
            iter ~level results syscall abs_offset candidate2 rest
        | Error _ -> iter ~level results syscall abs_offset candidate1 rest)
    | (Tree.Rule (offset, Ty.Indirect `Rel, _, _), Tree.Done) :: rest -> (
        Offset.process scheduler syscall fd offset abs_offset >>= function
        | Ok shift ->
            let seek fd abs_offset where =
              syscall.seek fd (Int64.add abs_offset shift) where
            in
            let metadata = Metadata.empty in
            go { syscall with seek } ~level:(succ level) abs_offset metadata
              root
            >>= fun metadata ->
            let candidate1 = Metadata.concat candidate1 metadata in
            iter ~level (candidate1 :: results) syscall
              (Int64.add abs_offset shift)
              candidate1 rest
        | Error _ -> iter ~level results syscall abs_offset candidate1 rest)
    | ((Tree.Rule (_, Ty.Default, _, _) as operation), tree) :: rest -> (
        match results with
        | _ :: _ -> iter ~level results syscall abs_offset candidate1 rest
        | [] -> (
            process scheduler syscall fd abs_offset candidate1 operation
            >>= function
            | Ok (abs_offset, candidate2) ->
                go syscall ~level:(succ level) abs_offset candidate2 tree
                >>= fun candidate3 ->
                iter ~level (candidate3 :: results) syscall abs_offset
                  candidate3 rest
            | Error _ -> iter ~level [] syscall abs_offset candidate1 rest))
    | (operation, tree) :: rest -> (
        process scheduler syscall fd abs_offset candidate1 operation
        >>= function
        | Ok (abs_offset, candidate1) ->
            go syscall ~level:(succ level) abs_offset candidate1 tree
            >>= fun candidate2 ->
            iter ~level (candidate2 :: results) syscall abs_offset candidate2
              rest
        | Error _ -> iter ~level results syscall abs_offset candidate1 rest)
  in
  go ~level:0 syscall abs_offset metadata root

type database = (string, Tree.t) Hashtbl.t * Tree.t

let rec fill_db db = function
  | Tree.Done -> ()
  | Tree.Node lst ->
      let rec go = function
        | [] -> ()
        | (Tree.Name (_, name), tree) :: rest ->
            (* XXX(dinosaure): /offset/ name value
               should appear only at the first level. *)
            Hashtbl.add db name tree;
            fill_db db tree;
            go rest
        | (_, tree) :: rest ->
            fill_db db tree;
            go rest
      in
      go (List.rev_map (fun (elt, sub) -> (Tree.operation elt, sub)) lst)

let database ~tree : database =
  let db = Hashtbl.create 0x10 in
  fill_db db tree;
  (db, tree)

let append ~tree (db, tree') : database =
  fill_db db tree;
  (db, Tree.merge tree tree')

let descending_walk scheduler syscall fd (db, tree) =
  descending_walk scheduler syscall db fd 0L Metadata.empty tree

let has_mime_tag (db, tree) =
  let visited = Hashtbl.create 0x10 in
  let rec iter has_mime_tag = function
    | [] -> ()
    | (Tree.Name _, tree) :: rest ->
        go has_mime_tag tree;
        if not !has_mime_tag then iter has_mime_tag rest
    | (Tree.Use { name; _ }, tree) :: rest ->
        (if not (Hashtbl.mem visited name) then
         match Hashtbl.find_opt db name with
         | Some tree' ->
             Hashtbl.add visited name ();
             go has_mime_tag tree'
         | None -> ());
        if not !has_mime_tag then go has_mime_tag tree;
        if not !has_mime_tag then iter has_mime_tag rest
    | (Tree.MIME _, _) :: _rest -> has_mime_tag := true
    | (Tree.Rule _, tree) :: rest ->
        go has_mime_tag tree;
        if not !has_mime_tag then iter has_mime_tag rest
  and go has_mime_tag = function
    | Tree.Done -> ()
    | Tree.Node lst ->
        let lst =
          List.rev_map (fun (elt, sub) -> (Tree.operation elt, sub)) lst
        in
        (iter [@tailcall]) has_mime_tag lst
  in
  let has_mime_tag = ref false in
  go has_mime_tag tree;
  !has_mime_tag

let only_mime_paths (db, tree) =
  let rec go = function
    | Tree.Done -> Tree.Unsafe.leaf
    | Tree.Node lst ->
        let lst =
          List.rev_map (fun (elt, sub) -> (Tree.operation elt, sub)) lst
        in
        let f acc (operation, sub) =
          let sub = go sub in
          let sub_has_mime_tag = has_mime_tag (db, sub) in
          match operation with
          | Tree.MIME _ -> (Tree.Unsafe.elt operation, sub) :: acc
          | _ when sub_has_mime_tag -> (Tree.Unsafe.elt operation, sub) :: acc
          | _ -> acc
        in
        Tree.Unsafe.node (List.fold_left f [] lst)
  in
  go tree

let rec ascending_walk ({ bind; return } as scheduler) syscall db fd results
    queue =
  let ( >>= ) = bind in

  match Queue.pop queue with
  | _, candidate, Tree.Done ->
      ascending_walk scheduler syscall db fd (candidate :: results) queue
  | abs_offset, candidate, Tree.Node lst ->
      let lst =
        List.rev_map (fun (elt, sub) -> (Tree.operation elt, sub)) lst
      in
      let rec go candidate = function
        | [] -> return ()
        | (Tree.Name (_, name), tree) :: rest ->
            Hashtbl.add db name tree;
            go candidate rest
        | (Tree.Use { name; _ }, Tree.Done) :: rest ->
            let tree = Hashtbl.find db name in
            Queue.push (abs_offset, candidate, tree) queue;
            go candidate rest
        | (operation, tree) :: rest -> (
            process scheduler syscall fd abs_offset candidate operation
            >>= function
            | Ok (abs_offset, candidate) ->
                Queue.push (abs_offset, candidate, tree) queue;
                go candidate rest
            | Error _ -> go candidate rest)
      in
      go candidate lst >>= fun () ->
      ascending_walk scheduler syscall db fd results queue
  | exception Queue.Empty -> return (List.rev results)

let ascending_walk scheduler syscall fd tree =
  let queue = Queue.create () in
  let db = Hashtbl.create 0x10 in
  Queue.push (0L, Metadata.empty, tree) queue;
  ascending_walk scheduler syscall db fd [] queue
