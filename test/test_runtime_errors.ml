open! Core
module Runtime = Karel_compiler.Runtime
module World = Karel_compiler.World

let%expect_test "hit wall" =
  let runtime = Runtime.create ~world:(World.create ~width:1 ~height:1 ()) () in
  Test_runtime.run ~runtime
    {|
class program {
  program() {
    move();
    turnoff();
  }
}
  |};
  [%expect
    {|
    WALL
    ((pc 6) (line 3) (function_stack ()) (expression_stack ())
     (world
      ((width 1) (height 1) (position (1 1)) (orientation North) (bag 0)
       (beepers ()) (walls ())))
     (ic ((counter 0) (limit (10000000))))
     (left_counter ((counter 0) (limit ())))
     (forward_counter ((counter 0) (limit ())))
     (pickbuzzer_counter ((counter 0) (limit ())))
     (leavebuzzer_counter ((counter 0) (limit ()))))
    x-x  Bag: 0
    |^|  Worldbuzzers: 0
    x-x  Position: (1, 1) |}]

let%expect_test "no beeper to pick" =
  let runtime = Runtime.create ~world:(World.create ~width:1 ~height:1 ()) () in
  Test_runtime.run ~runtime
    {|
class program {
  program() {
    pickbeeper();
    turnoff();
  }
}
  |};
  [%expect
    {|
    WORLDUNDERFLOW
    ((pc 2) (line 3) (function_stack ()) (expression_stack ())
     (world
      ((width 1) (height 1) (position (1 1)) (orientation North) (bag 0)
       (beepers ()) (walls ())))
     (ic ((counter 0) (limit (10000000))))
     (left_counter ((counter 0) (limit ())))
     (forward_counter ((counter 0) (limit ())))
     (pickbuzzer_counter ((counter 0) (limit ())))
     (leavebuzzer_counter ((counter 0) (limit ()))))
    x-x  Bag: 0
    |^|  Worldbuzzers: 0
    x-x  Position: (1, 1) |}]

let%expect_test "no beeper to put" =
  let runtime = Runtime.create ~world:(World.create ~width:1 ~height:1 ()) () in
  Test_runtime.run ~runtime
    {|
class program {
  program() {
    putbeeper();
    turnoff();
  }
}
  |};
  [%expect
    {|
    BAGUNDERFLOW
    ((pc 2) (line 3) (function_stack ()) (expression_stack ())
     (world
      ((width 1) (height 1) (position (1 1)) (orientation North) (bag 0)
       (beepers ()) (walls ())))
     (ic ((counter 0) (limit (10000000))))
     (left_counter ((counter 0) (limit ())))
     (forward_counter ((counter 0) (limit ())))
     (pickbuzzer_counter ((counter 0) (limit ())))
     (leavebuzzer_counter ((counter 0) (limit ()))))
    x-x  Bag: 0
    |^|  Worldbuzzers: 0
    x-x  Position: (1, 1) |}]

let%expect_test "instruction limit reached" =
  let runtime =
    Runtime.create ~leavebuzzer_limit:9
      ~world:(World.create ~width:1 ~height:1 ~bag:10 ())
      ()
  in
  Test_runtime.run ~runtime
    {|
class program {
  program() {
    iterate (10) {
      putbeeper();
    }
    turnoff();
  }
}
  |};
  [%expect
    {|
    INSTRUCTION
    ((pc 11) (line 4) (function_stack ()) (expression_stack (2))
     (world
      ((width 1) (height 1) (position (1 1)) (orientation North) (bag 1)
       (beepers (((1 1) 9))) (walls ())))
     (ic ((counter 26) (limit (10000000))))
     (left_counter ((counter 0) (limit ())))
     (forward_counter ((counter 0) (limit ())))
     (pickbuzzer_counter ((counter 0) (limit ())))
     (leavebuzzer_counter ((counter 9) (limit (9)))))
    x-x  Bag: 1
    |^|  Worldbuzzers: 9
    x-x  Position: (1, 1) |}]
