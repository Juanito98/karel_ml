(** The run result type. *)

open! Core

type t = OK | INSTRUCTION | WALL | WORLDUNDERFLOW | BAGUNDERFLOW | STACK
[@@deriving yojson, sexp, enum, enumerate]

let of_string s = sexp_of_string s |> t_of_sexp
let to_string t = sexp_of_t t |> string_of_sexp

let t_of_yojson = function
  | `String s -> of_string s
  | _ -> failwith "Could not convert Run_result.t to yojson"

let yojson_of_t t = `String (to_string t)
