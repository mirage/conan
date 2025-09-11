(** {1 File recognition.}

    [Conan_unix] provides few functions to recognize the MIME type of a given
    file. It implements {i unix} routines to execute a decision tree
    {!Conan.Tree.t} into a file accessible via a given {i path}.

    The {!Conan.Tree.t} can come from a {i database} available into a specific
    directory (see [conan-cli]) or the user can build/unserialize it from
    somewhere else.

    Let's play with [Conan_unix] such as ["/home/conan/database"] contains our
    database (see [conan] or [man magic] to understand the format of the
    database). You must unserialize it to get the {i decision tree} via:

    {[
      let tree = Conan_unix.tree ~directory:"/home/conan/database"
    ]}

    From this [tree], you can process it with a file accessible via a path.
    Let's say that, we want to recognize the type of ["/home/conan/file"], we
    just need:

    {[
      let metadata =
        match Conan_unix.run_with_tree tree "/home/conan/file" with
        | Ok m -> m
        | Error (`Msg err) -> failwith err
    ]}

    An equivalent of the code above is:

    {[
      let metadata =
        match
          Conan_unix.run ~database:"/home/conan/database" "/home/conan/file"
        with
        | Ok m -> m
        | Error (`Msg err) -> failwith err
    ]}

    Finally, you are able to generate a {i decision tree} from a [string]
    instead of a directory (as before) with {!tree_of_string}. Then, you are
    able to {i merge} it with another decision tree via {!Conan.Tree.merge}.

    {2 Metadata.}

    The returned {i metadata} gives you two informations: 1) a possible output
    which describes the given file 2) a {b possible} MIME type

    You can get the output description {!Conan.Metadata.output} and the MIME
    type via {!Conan.Metadata.mime}. These informations are optional - the
    decision tree can finish without any information. *)

val tree : directory:string -> Conan.Tree.t
(** [tree ~directory] generates a {!Conan.Tree.t} from the given [directory]. It
    will scan {b any} files from the directory (but it does not do a
    {b recursive} traverse) and it ignores any malformed files. *)

val tree_of_string : string -> (Conan.Tree.t, [> `Msg of string ]) result
(** [tree_of_string str] tries to construct from the given [str] a
    {!Conan.Tree.t}. *)

val run_with_tree :
  Conan.Tree.t -> string -> (Conan.Metadata.t, [> `Msg of string ]) result
(** [run_with_tree tree filename] executes the given decision [tree] on the file
    accessible via the path [filename]. *)

val run :
  database:string -> string -> (Conan.Metadata.t, [> `Msg of string ]) result
(** [run ~database filename] executes the {!Conan.Tree.t} which results from the
    given directory [database] on the file accessible via the path [filename].
*)
