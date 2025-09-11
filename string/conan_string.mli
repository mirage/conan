(** {1:File recognition.}

    [Conan_string] is a system-free implementation of the {i file} recognition.
    This implementation does not uses any {i syscalls} - and it can be safely
    used into a MirageOS example.

    So [Conan_string] does not have an access to a {i database}. The user must
    rebuild a {!Conan.Tree.t} by himself/herself via {!tree_of_string} for
    example. Then, from this [tree], the user is able to recognize a {i payload}
    as a simple [string] with {!run}:

    {[
      let v =
        {file|0       byte    0x66
      >1      byte    0x6f
      >>1     byte    0x6f    foo header
      |file}

      let tree =
        match Conan_string.tree_of_string v with
        | Ok tree -> tree
        | Error (`Msg err) -> failwith err

      let database = Conan.Tree.database ~tree

      let m =
        match Conan_string.run ~database contents with
        | Ok m -> m
        | Error (`Msg err) -> failwith err
    ]} *)

val tree_of_string : string -> (Conan.Tree.t, [> `Msg of string ]) result
(** [tree_of_string str] tries to parse the given [str] as a decision tree
    according to the [libmagic] format. *)

val run :
  database:Conan.Process.database ->
  string ->
  (Conan.Metadata.t, [> `Msg of string ]) result
(** [run ~database contents] tries to recognize the given [contents] with the
    help of [database]. The databse can be made via
    {!val:Conan.Process.database}. Indeed, a {!Conan.Tree.t} can have some
    indirect decision paths so we must pre-process the tree to agglomerate such
    indirections. *)
