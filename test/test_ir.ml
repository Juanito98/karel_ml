open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser
module Ir = Karel_compiler.Ir

let parse_and_print input =
  let lexbuf = Lexing.from_string input in
  let parsed = Parser.program Lexer.token lexbuf in
  let result = Ir.ast_to_ir parsed in
  print_s [%sexp (result : Ir.ir)]

let%expect_test "simple program" =
  parse_and_print {|
class program {
  program() {
    turnoff();
  }
}
  |};
  [%expect {| ((defs ()) (main (HALT))) |}]

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
    {|
      ((defs ())
       (main
        (WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD WORLDBUZZERS
         (EZ WORLDUNDERFLOW) PICKBUZZER BAGBUZZERS (EZ BAGUNDERFLOW) LEAVEBUZZER
         LEFT HALT))) |}]

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
    {|
      ((defs ())
       (main
        (WORLDWALLS ORIENTATION MASK AND NOT (JZ 7) WORLDWALLS ORIENTATION MASK AND
         NOT (EZ WALL) FORWARD HALT))) |}]

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
      ((defs ())
       (main
        (WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND
         NOT (EZ WALL) FORWARD (JMP 1) LEFT HALT))) |}]

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
      ((defs ())
       (main
        (WORLDWALLS ORIENTATION MASK AND NOT (JZ 15) WORLDBUZZERS (LOAD 0) EQ NOT
         (JZ 8) WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP 1) LEFT
         (JMP 3) BAGBUZZERS (EZ BAGUNDERFLOW) LEAVEBUZZER))) |}]

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
    {|
      ((defs ())
       (main
        (WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND
         NOT (EZ WALL) FORWARD (JMP -13) HALT))) |}]

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
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT NOT WORLDWALLS ORIENTATION ROTL MASK
       AND NOT ORIENTATION (LOAD 1) EQ NOT OR AND (JZ 7) WORLDWALLS ORIENTATION
       MASK AND NOT (EZ WALL) FORWARD))) |}]

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
  [%expect {|
    ((defs ())
     (main
      ((LOAD 5) DUP (LOAD 0) EQ NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND NOT
       (EZ WALL) FORWARD (JMP -12) POP HALT))) |}]

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
  [%expect{|
    ((defs
      (((name turnright) (arg ())
        (body ((LOAD 3) DUP (LOAD 0) EQ NOT (JZ 2) LEFT (JMP -6) POP)))
       ((name turn) (arg (x))
        (body ((PARAM x) DUP (LOAD 0) EQ NOT (JZ 2) LEFT (JMP -6) POP)))))
     (main ((LOAD 0) (CALL turnright) (LOAD 2) (CALL turn) HALT))) |}]
