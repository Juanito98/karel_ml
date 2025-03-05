open! Core
module Ast = Karel_compiler.Ast
module Lexer = Karel_compiler.Lexer
module Parser = Karel_compiler.Parser
module IR = Karel_compiler.Ir

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
    (Program () (Main (TurnOff)))
    ===		IR   	===
    ((defs ()) (main (HALT)))

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
    (Program () (Main (Move PickBeeper PutBeeper TurnLeft TurnOff)))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD WORLDBUZZERS
       (EZ WORLDUNDERFLOW) PICKBUZZER BAGBUZZERS (EZ BAGUNDERFLOW) LEAVEBUZZER
       LEFT HALT)))

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
    (Program () (Main ((If FrontIsClear (Block (Move)) (Block ())) TurnOff)))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT (JZ 7) WORLDWALLS ORIENTATION MASK AND
       NOT (EZ WALL) FORWARD HALT)))

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
     (Main ((If FrontIsClear (Block (Move)) (Block (TurnLeft))) TurnOff)))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND
       NOT (EZ WALL) FORWARD (JMP 1) LEFT HALT)))

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
     (Main ((If FrontIsClear (If NextToABeeper Move TurnLeft) PutBeeper))))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT (JZ 15) WORLDBUZZERS (LOAD 0) EQ NOT
       (JZ 8) WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD (JMP 1) LEFT
       (JMP 3) BAGBUZZERS (EZ BAGUNDERFLOW) LEAVEBUZZER)))

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
    (Program () (Main ((While FrontIsClear (Block (Move))) TurnOff)))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND
       NOT (EZ WALL) FORWARD (JMP -13) HALT)))

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
      ((If (And (Not FrontIsClear) (Or LeftIsClear (Not FacingNorth)))
        (Block (Move)) (Block ())))))
    ===		IR   	===
    ((defs ())
     (main
      (WORLDWALLS ORIENTATION MASK AND NOT NOT WORLDWALLS ORIENTATION ROTL MASK
       AND NOT ORIENTATION (LOAD 1) EQ NOT OR AND (JZ 7) WORLDWALLS ORIENTATION
       MASK AND NOT (EZ WALL) FORWARD)))

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
    (Program () (Main ((Iterate (Int 5) (Block (Move))) TurnOff)))
    ===		IR   	===
    ((defs ())
     (main
      ((LOAD 5) DUP (LOAD 0) EQ NOT (JZ 8) WORLDWALLS ORIENTATION MASK AND NOT
       (EZ WALL) FORWARD DEC (JMP -13) POP HALT)))

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
    (Program ((Def turnright () ((Iterate (Int 3) (Block (TurnLeft))))))
     (Main ((Call turnright ()) TurnOff)))
    ===		IR   	===
    ((defs
      (((name turnright) (arg ())
        (body ((LOAD 3) DUP (LOAD 0) EQ NOT (JZ 2) LEFT DEC (JMP -7) POP)))))
     (main ((LOAD 0) (CALL turnright) HALT)))

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
    (Program ((Def turn (x) ((Iterate (Var x) (Block (TurnLeft))))))
     (Main ((Call turn ((Int 2))) TurnOff)))
    ===		IR   	===
    ((defs
      (((name turn) (arg (x))
        (body ((PARAM x) DUP (LOAD 0) EQ NOT (JZ 2) LEFT DEC (JMP -7) POP)))))
     (main ((LOAD 2) (CALL turn) HALT))) |}]
