open! Core
module Run_result = Karel_compiler.Run_result
module Runtime = Karel_compiler.Runtime
module Compiler = Karel_compiler.Compiler
module World = Karel_compiler.World

let run ?(world = World.create ~width:2 ~height:2 ()) code =
  let runtime = Runtime.create ~world () in
  let program = Compiler.compile code in
  let result = Runtime.run runtime (Array.Permissioned.of_list program) in
  print_s [%sexp (result : Run_result.t), (world : World.t)]

let%expect_test "simple program" =
  let world =
    World.create ~width:2 ~height:2
      ~beepers:(Hashtbl.of_alist_exn (module World.Position) [ ((1, 1), 1) ])
      ()
  in
  run ~world
    {|
class program {
  void turn() {
    iterate(3) turnleft();
  }

  program() {
    pickbeeper();
    turn();
    move();
    putbeeper();
    turnleft();
    turnoff();
  }
}
  |};
  [%expect{|
    (OK
     ((width 2) (height 2) (position (2 1)) (orientation North) (bag 0)
      (beepers (((2 1) 1))) (walls ()))) |}]
