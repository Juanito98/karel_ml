open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexerjs
module Parser = Karel_compiler.Parserjs

let parse_and_print input =
  let lexbuf = Lexing.from_string input in
  let result = Parser.program Lexer.token lexbuf in
  print_s [%sexp (result : Ast.program)]

let%expect_test "simple program" =
  parse_and_print {|
class program {
  program() {
    turnoff();
  }
}
  |};
  [%expect {| (Program () (Main (TurnOff))) |}]

let%expect_test "simple statements" =
  parse_and_print
    {|
class program {
  program() {
    move();
    pickbeeper();
    putbeeper();
    turnleft();
    turnoff();
  }
}
  |};
  [%expect
    {| (Program () (Main (Move PickBeeper PutBeeper TurnLeft TurnOff))) |}]

let%expect_test "simple if" =
  parse_and_print
    {|
class program {
  program() {
    if (frontIsClear) {
      move();
    }
    turnoff();
  }
}
  |};
  [%expect
    {| (Program () (Main ((If FrontIsClear (Block (Move)) (Block ())) TurnOff))) |}]

let%expect_test "if else" =
  parse_and_print
    {|
class program {
  program() {
    if (frontIsClear) {
      move();
    } else {
      turnleft();
    }
    turnoff();
  }
}
  |};
  [%expect
    {|
      (Program ()
       (Main ((If FrontIsClear (Block (Move)) (Block (TurnLeft))) TurnOff))) |}]

let%expect_test "nested if/else" =
  parse_and_print
    {|
class program {
  program() {
    if (frontIsClear)
      if (nextToABeeper) move();
      else turnleft();
    else putbeeper();
  }
}
  |};
  [%expect
    {|
      (Program ()
       (Main ((If FrontIsClear (If NextToABeeper Move TurnLeft) PutBeeper)))) |}]

let%expect_test "simple while" =
  parse_and_print
    {|
class program {
  program() {
    while (frontIsClear) {
      move();
    }
    turnoff();
  }
}
  |};
  [%expect
    {| (Program () (Main ((While FrontIsClear (Block (Move))) TurnOff))) |}]

let%expect_test "multiple conditions" =
  parse_and_print
    {|
class program {
  program() {
    if (!frontIsClear && (leftIsClear || notFacingNorth)) {
      move();
    }
  }
}
  |};
  [%expect
    {|
    (Program ()
     (Main
      ((If (And (Not FrontIsClear) (Or LeftIsClear (Not FacingNorth)))
        (Block (Move)) (Block ()))))) |}]

let%expect_test "simple iterate" =
  parse_and_print
    {|
class program {
  program() {
    iterate (5) {
      move();
    }
    turnoff();
  }
}
  |};
  [%expect {| (Program () (Main ((Iterate (Int 5) (Block (Move))) TurnOff))) |}]

let%expect_test "simple call" =
  parse_and_print
    {|
class program {
  void turnright() {
    iterate (3) {
      turnleft();
    }
  }
  
  void turn(x) {
    iterate (x) {
      turnleft();
    }
  }
  
  program() {
    turnright();
    turn(2);
    turnoff();
  }
}
  |};
  [%expect
    {|
    (Program
     ((turnright () ((Iterate (Int 3) (Block (TurnLeft)))))
      (turn (x) ((Iterate (Var x) (Block (TurnLeft))))))
     (Main ((Call turnright ()) (Call turn ((Int 2))) TurnOff))) |}]
