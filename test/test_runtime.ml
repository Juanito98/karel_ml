open! Core
module Run_result = Karel_compiler.Run_result
module Runtime = Karel_compiler.Runtime
module Compiler = Karel_compiler.Compiler
module World = Karel_compiler.World

let run ?(runtime = Runtime.create ~world:(World.create ()) ()) code =
  let program = Compiler.compile (String.strip code) in
  let result = Runtime.run runtime (Array.Permissioned.of_list program) in
  print_s [%sexp (result : Run_result.t)];
  print_s [%sexp (runtime : Runtime.t)];
  Runtime.world runtime |> Karel_compiler.Renderer.world_img
  |> Notty_unix.output_image

let%expect_test "simple program" =
  let runtime =
    Runtime.create
      ~world:
        (World.create ~width:2 ~height:2
           ~beepers:
             (Hashtbl.of_alist_exn (module World.Position) [ ((1, 1), 3) ])
           ())
      ()
  in
  run ~runtime
    {|
class program {
  void turn(x) {
    iterate(x) turnleft();
  }

  program() {
    pickbeeper();
    turn(3);
    move();
    putbeeper();
    turnleft();
    turnoff();
  }
}
  |};
  [%expect
    {|
    OK
    ((pc 22) (line 12) (function_stack ()) (expression_stack ())
     (world
      ((width 2) (height 2) (position (2 1)) (orientation North) (bag 0)
       (beepers (((1 1) 2) ((2 1) 1))) (walls ())))
     (ic ((counter 15) (limit (10000000))))
     (left_counter ((counter 4) (limit ())))
     (forward_counter ((counter 1) (limit ())))
     (pickbuzzer_counter ((counter 1) (limit ())))
     (leavebuzzer_counter ((counter 1) (limit ()))))
    x-x-x  Bag: 0
    |. .|  Worldbuzzers: 1
    x x x  Position: (2, 1)
    |2 ^|
    x-x-x |}]
