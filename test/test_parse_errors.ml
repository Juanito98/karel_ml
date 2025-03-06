open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser

let parse_and_print input =
  let lexbuf = Lexing.from_string input in
  try
    let result = lexbuf |> Parser.program Lexer.token in
    print_s [%sexp (result : Ast.program)]
  with
  | Lexer.InvalidToken ->
      (* Get line information. *)
      let pos = lexbuf.lex_curr_p in
      printf "Invalid token at line %d, character %d\n" pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      printf "Syntax error at line %d, character %d\n" pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)

let%expect_test "invalid token" =
  parse_and_print {|
class program {
  *
}
  |};
  [%expect {| Invalid token at line 3, character 3 |}]

let%expect_test "missing class program" =
  parse_and_print {|
program() {
  turnoff();
}
  |};
  [%expect {| Syntax error at line 2, character 7 |}]

let%expect_test "redefinition of program" =
  parse_and_print
    {|
class program {
  void program() {
    turnoff();
  }

  program() {
    turnoff();
  }
}
  |};
  [%expect {| Syntax error at line 3, character 14 |}]
