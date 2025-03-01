(* File main.ml *)
open! Core

let parse input =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parserjs.program Lexerjs.token lexbuf in
    Ok result
  with
  | Lexerjs.InvalidToken ->
      (* Get line information. *)
      let pos = lexbuf.lex_curr_p in
      Error
        (Error.of_string
           (Printf.sprintf "Invalid token at line %d, character %d\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
  | Parserjs.Error ->
      let pos = lexbuf.lex_curr_p in
      Error
        (Error.of_string
           (Printf.sprintf "Syntax error at line %d, character %d\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
