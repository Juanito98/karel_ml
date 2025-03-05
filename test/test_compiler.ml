open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser
module IR = Karel_compiler.Ir
module Compiler = Karel_compiler.Compiler
module Instruction = Karel_compiler.Instruction

let parse_and_print (name, code) =
  let print_header = printf "===\t\t%-*s\t===\n" 5 in
  Utils.print_program (name, code);

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
  printf "\n"

let%expect_test _ =
  List.iter Utils.programs ~f:parse_and_print;
  [%expect
    {|
    simple program
    ---------------
     1| class program {
     2|   program() {
     3|     turnoff();
     4|   }
     5| }
    ===		AST  	===
    (Program () (Main ((TurnOff <opaque>))))
    ===		IR   	===
    ((defs ()) (main ((LINE 3) HALT)))
    ===		EXE  	===
    ((LINE 3) HALT)

    simple statements
    ------------------
     1| class program {
     2|   program() {
     3|     move();
     4|     pickbeeper();
     5|     putbeeper();
     6|     turnleft();
     7|     turnoff();
     8|   }
     9| }
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
     (EZ BAGUNDERFLOW) LEAVEBUZZER (LINE 6) LEFT (LINE 7) HALT)

    simple if
    ----------
     1| class program {
     2|   program() {
     3|     if (frontIsClear) {
     4|       move();
     5|     }
     6|     turnoff();
     7|   }
     8| }
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
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 6) HALT)

    if else
    --------
     1| class program {
     2|   program() {
     3|     if (frontIsClear) {
     4|       move();
     5|     } else {
     6|       turnleft();
     7|     }
     8|     turnoff();
     9|   }
    10| }
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
     HALT)

    nested if/else
    ---------------
     1| class program {
     2|   program() {
     3|     if (frontIsClear)
     4|       if (nextToABeeper) move();
     5|       else turnleft();
     6|     else putbeeper();
     7|   }
     8| }
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
     (EZ BAGUNDERFLOW) LEAVEBUZZER)

    simple while
    -------------
     1| class program {
     2|   program() {
     3|     while (frontIsClear) {
     4|       move();
     5|     }
     6|     turnoff();
     7|   }
     8| }
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
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP -15) (LINE 6) HALT)

    multiple conditions
    --------------------
     1| class program {
     2|   program() {
     3|     if (!frontIsClear && (leftIsClear || notFacingNorth)) {
     4|       move();
     5|     }
     6|   }
     7| }
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
     ORIENTATION MASK AND NOT (EZ WALL) FORWARD)

    simple iterate
    ---------------
     1| class program {
     2|   program() {
     3|     iterate (5) {
     4|       move();
     5|     }
     6|     turnoff();
     7|   }
     8| }
    ===		AST  	===
    (Program ()
     (Main
      ((Iterate <opaque> (Int 5) (Block ((Move <opaque>)))) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs ())
     (main
      ((LINE 3) (LOAD 5) DUP (LOAD 0) EQ NOT (JZ 9) (LINE 4) WORLDWALLS
       ORIENTATION MASK AND NOT (EZ WALL) FORWARD DEC (JMP -14) POP (LINE 6)
       HALT)))
    ===		EXE  	===
    ((LINE 3) (LOAD 5) DUP (LOAD 0) EQ NOT (JZ 9) (LINE 4) WORLDWALLS ORIENTATION
     MASK AND NOT (EZ WALL) FORWARD DEC (JMP -14) POP (LINE 6) HALT)

    simple call
    ------------
     1| class program {
     2|   void turnright() {
     3|     iterate (3) {
     4|       turnleft();
     5|     }
     6|   }
     7|
     8|   program() {
     9|     turnright();
    10|     turnoff();
    11|   }
    12| }
    ===		AST  	===
    (Program
     ((Def <opaque> turnright ()
       ((Iterate <opaque> (Int 3) (Block ((TurnLeft <opaque>)))))))
     (Main ((Call <opaque> turnright ()) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs
      (((name turnright) (arg ())
        (body
         ((LINE 3) (LOAD 3) DUP (LOAD 0) EQ NOT (JZ 3) (LINE 4) LEFT DEC
          (JMP -8) POP RET))
        (line 2))))
     (main ((LINE 9) (LOAD 0) (CALL turnright) (LINE 10) HALT)))
    ===		EXE  	===
    ((LINE 9) (LOAD 0) (CALL (5 turnright)) (LINE 10) HALT (LINE 3) (LOAD 3) DUP
     (LOAD 0) EQ NOT (JZ 3) (LINE 4) LEFT DEC (JMP -8) POP RET)

    simple call with arg
    ---------------------
     1| class program {
     2|   void turn(x) {
     3|     iterate (x) {
     4|       turnleft();
     5|     }
     6|   }
     7|
     8|   program() {
     9|     turn(2);
    10|     turnoff();
    11|   }
    12| }
    ===		AST  	===
    (Program
     ((Def <opaque> turn (x)
       ((Iterate <opaque> (Var x) (Block ((TurnLeft <opaque>)))))))
     (Main ((Call <opaque> turn ((Int 2))) (TurnOff <opaque>))))
    ===		IR   	===
    ((defs
      (((name turn) (arg (x))
        (body
         ((LINE 3) (PARAM x) DUP (LOAD 0) EQ NOT (JZ 3) (LINE 4) LEFT DEC
          (JMP -8) POP RET))
        (line 2))))
     (main ((LINE 9) (LOAD 2) (CALL turn) (LINE 10) HALT)))
    ===		EXE  	===
    ((LINE 9) (LOAD 2) (CALL (5 turn)) (LINE 10) HALT (LINE 3) (PARAM 0) DUP
     (LOAD 0) EQ NOT (JZ 3) (LINE 4) LEFT DEC (JMP -8) POP RET) |}]
