open! Core
module Run_result = Karel_compiler.Run_result

let%expect_test "Run result codes" =
  let all = Run_result.all in
  List.iter all ~f:(fun result ->
      print_s [%sexp (result : Run_result.t), (Run_result.to_enum result : int)]);
  [%expect
    {|
    (OK 0)
    (INSTRUCTION 1)
    (WALL 2)
    (WORLDUNDERFLOW 3)
    (BAGUNDERFLOW 4)
    (STACK 5) |}]
