open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser
module IR = Karel_compiler.Ir
module Compiler = Karel_compiler.Compiler
module Instruction = Karel_compiler.Instruction

let compile_and_print code =
  let code = String.strip code in
  let print_header = printf "===\t\t%-*s\t===\n" 5 in

  let lexbuf = Lexing.from_string code in
  let ast = Parser.program Lexer.token lexbuf in
  print_header "AST";
  print_s [%sexp (ast : Ast.program)];

  let ir = IR.ast_to_ir ast in
  print_header "IR";
  print_s [%sexp (ir : IR.ir)];

  let program = Compiler.link ir in
  print_header "EXE";
  print_s [%sexp (program : Instruction.t list)];
  print_endline "\n"

let%expect_test "simple program" =
  compile_and_print {|
class program {
  program() {
    turnoff();
  }
}
  |};
  [%expect
    {|
    ===		AST  	===
    (Program () (Main ((TurnOff <opaque>))))
    ===		IR   	===
    ((defs ()) (main ((LINE 3) HALT)))
    ===		EXE  	===
    ((LINE 3) HALT) |}]

let%expect_test "simple statements" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((Move <opaque>) (PickBeeper <opaque>) (PutBeeper <opaque>)
       (TurnLeft <opaque>) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 4)
       WORLDBUZZERS (EZ WORLDUNDERFLOW) PICKBUZZER (LINE 5) BAGBUZZERS
       (EZ BAGUNDERFLOW) LEAVEBUZZER (LINE 6) LEFT (LINE 7) HALT)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 4)
     WORLDBUZZERS (EZ WORLDUNDERFLOW) PICKBUZZER (LINE 5) BAGBUZZERS
     (EZ BAGUNDERFLOW) LEAVEBUZZER (LINE 6) LEFT (LINE 7) HALT) |}]

let%expect_test "simple if" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((If <opaque> FrontIsClear (Block ((Move <opaque>))) (Block ()))
       (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) (LINE 4) WORLDWALLS
       ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 6) HALT)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) (LINE 4) WORLDWALLS
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 6) HALT) |}]

let%expect_test "if else" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((If <opaque> FrontIsClear (Block ((Move <opaque>)))
        (Block ((TurnLeft <opaque>))))
       (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 9) (LINE 4) WORLDWALLS
       ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP 2) (LINE 6) LEFT (LINE 8)
       HALT)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 9) (LINE 4) WORLDWALLS
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP 2) (LINE 6) LEFT (LINE 8)
     HALT) |}]

let%expect_test "nested if/else" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((If <opaque> FrontIsClear
        (If <opaque> NextToABeeper (Move <opaque>) (TurnLeft <opaque>))
        (PutBeeper <opaque>)))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 18) (LINE 4) WORLDBUZZERS
       (LOAD 0) EQ NOT (JZ 9) (LINE 4) WORLDWALLS ORIENTATION MASK AND NOT
       (EZ WALL) FORWARD (JMP 2) (LINE 5) LEFT (JMP 4) (LINE 6) BAGBUZZERS
       (EZ BAGUNDERFLOW) LEAVEBUZZER)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 18) (LINE 4) WORLDBUZZERS
     (LOAD 0) EQ NOT (JZ 9) (LINE 4) WORLDWALLS ORIENTATION MASK AND NOT
     (EZ WALL) FORWARD (JMP 2) (LINE 5) LEFT (JMP 4) (LINE 6) BAGBUZZERS
     (EZ BAGUNDERFLOW) LEAVEBUZZER) |}]

let%expect_test "simple while" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((While <opaque> FrontIsClear (Block ((Move <opaque>))))
       (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 9) (LINE 4) WORLDWALLS
       ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP -15) (LINE 6) HALT)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT (JZ 9) (LINE 4) WORLDWALLS
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP -15) (LINE 6) HALT) |}]

let%expect_test "multiple conditions" =
  compile_and_print
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
    ===		AST  	===
    (Program ()
     (Main
      ((If <opaque> (And (Not FrontIsClear) (Or LeftIsClear (Not FacingNorth)))
        (Block ((Move <opaque>))) (Block ())))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT NOT WORLDWALLS ORIENTATION
       ROTL MASK AND NOT ORIENTATION (LOAD 1) EQ NOT OR AND (JZ 8) (LINE 4)
       WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD)))
    ===		EXE  	===
    ((LINE 3) WORLDWALLS ORIENTATION MASK AND NOT NOT WORLDWALLS ORIENTATION ROTL
     MASK AND NOT ORIENTATION (LOAD 1) EQ NOT OR AND (JZ 8) (LINE 4) WORLDWALLS
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD) |}]

let%expect_test "simple iterate" =
  compile_and_print
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
  [%expect
    {|
    ===		AST  	===
    (Program ()
     (Main
      ((Iterate <opaque> (Int 5) (Block ((Move <opaque>)))) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) (LOAD 5) DUP (LOAD 0) EQ NOT (JZ 10) (LINE 4) WORLDWALLS
       ORIENTATION MASK AND NOT (EZ WALL) FORWARD DEC (JMP -15) POP (LINE 6)
       HALT)))
    ===		EXE  	===
    ((LINE 3) (LOAD 5) DUP (LOAD 0) EQ NOT (JZ 10) (LINE 4) WORLDWALLS
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD DEC (JMP -15) POP (LINE 6) HALT) |}]

let%expect_test "simple call" =
  compile_and_print
    {|
class program {
  void turnright() {
    iterate (3) {
      turnleft();
    }
  }
  
  program() {
    turnright();
    turnoff();
  }
}
  |};
  [%expect
    {|
    ===		AST  	===
    (Program
     ((Def <opaque> turnright ()
       ((Iterate <opaque> (Int 3) (Block ((TurnLeft <opaque>)))))))
     (Main ((Call <opaque> turnright ()) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs
      (((name turnright) (arg ())
        (body
         ((LINE 3) (LOAD 3) DUP (LOAD 0) EQ NOT (JZ 4) (LINE 4) LEFT DEC
          (JMP -9) POP RET))
        (line 2))))
     (main ((LINE 9) (LOAD 0) (CALL turnright) (LINE 10) HALT)))
    ===		EXE  	===
    ((LINE 9) (LOAD 0) (CALL (5 turnright)) (LINE 10) HALT (LINE 3) (LOAD 3) DUP
     (LOAD 0) EQ NOT (JZ 4) (LINE 4) LEFT DEC (JMP -9) POP RET) |}]

let%expect_test "simple call with arg" =
  compile_and_print
    {|
class program {  
  void turn(x) {
    iterate (x) {
      turnleft();
    }
  }
  
  program() {
    turn(2);
    turnoff();
  }
}
  |};
  [%expect
    {|
    ===		AST  	===
    (Program
     ((Def <opaque> turn (x)
       ((Iterate <opaque> (Var x) (Block ((TurnLeft <opaque>)))))))
     (Main ((Call <opaque> turn ((Int 2))) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs
      (((name turn) (arg (x))
        (body
         ((LINE 3) (PARAM x) DUP (LOAD 0) EQ NOT (JZ 4) (LINE 4) LEFT DEC
          (JMP -9) POP RET))
        (line 2))))
     (main ((LINE 9) (LOAD 2) (CALL turn) (LINE 10) HALT)))
    ===		EXE  	===
    ((LINE 9) (LOAD 2) (CALL (5 turn)) (LINE 10) HALT (LINE 3) (PARAM 0) DUP
     (LOAD 0) EQ NOT (JZ 4) (LINE 4) LEFT DEC (JMP -9) POP RET) |}]
