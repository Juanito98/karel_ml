open! Core
module Instruction = Karel_compiler.Instruction

let program_of_yojson = function
  | `List instructions -> List.map instructions ~f:Instruction.t_of_yojson
  | _ -> failwith "Expected list"

let%expect_test _ =
  let input =
    {|[["LINE",2],["WORLDWALLS"],["ORIENTATION"],["MASK"],["AND"],["NOT"],["EZ","WALL"],["FORWARD"],["LINE",3],["HALT"],["LINE",6],["HALT"]]|}
  in
  let json = Yojson.Safe.from_string input in

  let program = program_of_yojson json in
  print_s [%sexp (program : Instruction.t list)];
  [%expect
    {|
    ((LINE 2) WORLDWALLS ORIENTATION MASK AND NOT (EZ WALL) FORWARD (LINE 3) HALT
     (LINE 6) HALT) |}]
