open Sigs

type database

val descending_walk :
  't scheduler ->
  ('fd, 'error, 't) syscall ->
  'fd ->
  database ->
  (Metadata.t, 't) io

val ascending_walk :
  't scheduler ->
  ('fd, 'error, 't) syscall ->
  'fd ->
  Tree.t ->
  (Metadata.t list, 't) io

val database : tree:Tree.t -> database
val append : tree:Tree.t -> database -> database
val only_mime_paths : database -> Tree.t
