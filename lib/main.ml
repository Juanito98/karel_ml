(* File main.ml *)
open! Core

let parse input =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.program Lexer.token lexbuf in
    Ok result
  with
  | Lexer.InvalidToken ->
      (* Get line information. *)
      let pos = lexbuf.lex_curr_p in
      Error
        (Error.of_string
           (Printf.sprintf "Invalid token at line %d, character %d\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Error
        (Error.of_string
           (Printf.sprintf "Syntax error at line %d, character %d\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
