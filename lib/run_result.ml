(** The run result type. *)

open! Core

type t = OK | INSTRUCTION | WALL | WORLDUNDERFLOW | BAGUNDERFLOW | STACK
[@@deriving sexp, enum, enumerate]
