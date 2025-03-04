open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser

let parse_and_print input =
  let result = Karel_compiler.Main.parse input in
  print_s [%sexp (result : Ast.program Or_error.t)]

let%expect_test "invalid token" =
  parse_and_print {|
class program {
  *
}
  |};
  [%expect {| (Error "Invalid token at line 3, character 3\n") |}]

let%expect_test "missing class program" =
  parse_and_print {|
program() {
  turnoff();
}
  |};
  [%expect {| (Error "Syntax error at line 2, character 7\n") |}]

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
  [%expect {| (Error "Syntax error at line 3, character 14\n") |}]
