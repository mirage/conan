[@@@warning "-27"]

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let ( <.> ) f g = fun x -> f (g x)

let ok x = Ok x

let newline = "\n" (* TODO *)

let reword_error f = function
  | Ok _ as v -> v
  | Error err -> Error (f err)

open Sigs

type metadata =
  { name : string option
  ; mime : string option
  ; output : string }

let pp_option pp_val ppf = function
  | Some v -> pp_val ppf v
  | None -> Format.fprintf ppf "<none>"

let pp_string ppf = Format.fprintf ppf "%s"

let pp_metadata ppf metadata =
  Format.fprintf ppf "{ @[<hov>name= %a;@ \
                               mime= %a;@ \
                               output= %S@] }"
    (pp_option pp_string) metadata.name
    (pp_option pp_string) metadata.mime
    metadata.output

type error = [ `Invalid_test ]

let process_fmt
  : type v. metadata -> (_, v) Ty.t -> v Tree.fmt -> v -> metadata
  = fun metadata ty { Tree.fmt } v ->
    Format.kasprintf (fun output -> { metadata with output }) ("%s" ^^ (fmt ()))
      metadata.output v

let process
  : type s fd error.
     s scheduler
  -> (fd, error, s) syscall
  -> fd
  -> int64
  -> metadata
  -> Tree.operation
  -> ((int64 * metadata, [> `Syscall of error
                         |  `Invalid_test
                         |  `No_process ]) result, s) io
  = fun ({ bind; return; } as scheduler) syscall fd abs_offset metadata -> function
    | Tree.Name _ -> return (Error `No_process)
    | Tree.Use _ -> return (Error `No_process)
    | Tree.Rule (offset, ty, test, fmt) ->
      let ( >>= ) = bind in
      let ( >|= ) x f = x >>= fun x -> return (f x) in
      let ( >?= ) x f = x >>= function
        | Ok x -> f x
        | Error err -> return (Error err) in
      Offset.process scheduler syscall fd offset abs_offset >|=
      reword_error (fun err -> `Syscall err) >?= fun abs_offset ->
      Ty.process scheduler syscall fd abs_offset ty >?= fun v ->
      match Test.process ty test v with
      | Some v ->
        let metadata = process_fmt metadata ty fmt v in
        return (Ok (abs_offset, metadata))
      | None ->
        return (Error `Invalid_test)

let descending_walk
  = fun ({ bind; return; } as scheduler) syscall db fd abs_offset metadata root ->
    let ( >>= ) = bind in

    let rec go ~level syscall abs_offset candidate0 = function
      | Tree.Done -> return candidate0
      | Tree.Node lst ->
        let lst = List.rev lst in
        iter ~level ~default:true candidate0 lst
    and iter ~level ~default candidate1 = function
      | [] -> return candidate1
      | (Tree.Name (_, name), tree) :: rest ->
        iter ~level ~default candidate1 rest
      | (Tree.Use (offset, name), Tree.Done) :: rest ->
        ( Offset.process scheduler syscall fd offset abs_offset >>= function
            | Ok shift ->
              let seek fd abs_offset where =
                syscall.seek fd (Int64.add abs_offset shift) where in
              let tree = Hashtbl.find db name in
              go { syscall with seek } ~level:(succ level) abs_offset candidate1 tree
            | Error _ -> iter ~level ~default candidate1 rest )
      | (Tree.Rule (offset, Ty.Indirect `Rel, _, _), Tree.Done) :: rest ->
        ( Offset.process scheduler syscall fd offset abs_offset >>= function
            | Ok shift ->
              let seek fd abs_offset where =
                syscall.seek fd (Int64.add abs_offset shift) where in
              go { syscall with seek } ~level:(succ level) abs_offset candidate1 root
            | Error _ -> iter ~level ~default candidate1 rest )
      | (Tree.Rule (_, Ty.Default, _, _) as operation, tree) :: rest ->
        if default
        then
          process scheduler syscall fd abs_offset candidate1 operation >>= function
          | Ok (abs_offset, candidate2) ->
            go syscall ~level:(succ level) abs_offset candidate2 tree >>= fun candidate3 ->
            iter ~level ~default candidate3 rest
          | Error _ -> iter ~level ~default candidate1 rest
        else iter ~level ~default candidate1 rest
      | (Tree.Rule (_, Ty.Clear, _, _) as _operation, tree) :: rest ->
        iter ~level ~default:true candidate1 rest (* TODO: compute [operation]? *)
      | (operation, tree) :: rest ->
        Format.eprintf "[-] process %s%a %!" (String.make level '>') Tree.pp_operation operation ;
        process scheduler syscall fd abs_offset candidate1 operation >>= function
        | Ok (abs_offset, candidate) ->
          Format.eprintf "[ok].\n%!" ;
          go syscall ~level:(succ level) abs_offset candidate tree >>= fun candidate ->
          iter ~level ~default:false candidate1 rest
        | Error _ ->
          Format.eprintf "[error].\n%!" ;
          iter ~level ~default candidate1 rest in
    go ~level:0 syscall abs_offset metadata root

let descending_walk ?(db= Hashtbl.create 0x10) ({ bind; return; } as scheduler) syscall fd tree =
  let ( >>= ) = bind in
  let ( >|= ) x f = x >>= fun x -> return (f x) in
  descending_walk scheduler syscall db fd 0L { name= None; mime= None; output= "" } tree >|= fun metadata ->
  [ metadata ]

let fill_db db = function
  | Tree.Done -> ()
  | Tree.Node lst ->
    let rec go = function
      | [] -> ()
      | (Tree.Name (_, name), tree) :: rest ->
        (* XXX(dinosaure): /offset/ name value
           should appear only at the first level. *)
        Hashtbl.add db name tree ; go rest
      | _ :: rest -> go rest in
    go (List.rev lst)

let rec ascending_walk
  = fun ({ bind; return; } as scheduler) syscall db fd results queue ->
    let ( >>= ) = bind in

    match Queue.pop queue with
    | _, candidate, Tree.Done -> ascending_walk scheduler syscall db fd (candidate :: results) queue
    | abs_offset, candidate, (Tree.Node lst) ->
      let lst = List.rev lst in
      let rec go candidate = function
        | [] -> return ()
        | (Tree.Name (_, name), tree) :: rest ->
          Format.eprintf "[+] record %s.\n%!" name ;
          Hashtbl.add db name tree ; go candidate rest
        | (Tree.Use (_, name), _) :: rest ->
          Format.eprintf "[+] use %s.\n%!" name ;
          let tree = Hashtbl.find db name in
          Queue.push (abs_offset, candidate, tree) queue ; go candidate rest
        | (operation, tree) :: rest ->
          Format.eprintf "[-] process %a %!" Tree.pp_operation operation ;
          process scheduler syscall fd abs_offset candidate operation >>= function
          | Ok (abs_offset, candidate) ->
            Format.eprintf "[ok].\n%!" ;
            Queue.push (abs_offset, candidate, tree) queue ; go candidate rest
          | Error err ->
            Format.eprintf "[error].\n%!" ;
            go candidate rest in
      go candidate lst >>= fun () -> ascending_walk scheduler syscall db fd results queue
    | exception Queue.Empty -> return (List.rev results)

let ascending_walk scheduler syscall fd tree =
  let queue = Queue.create () in
  let db = Hashtbl.create 0x10 in
  Queue.push (0L, { name= None; mime= None; output= "" }, tree) queue ;
  ascending_walk scheduler syscall db fd [] queue
