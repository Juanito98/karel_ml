open! Core

type stack_frame = { pc : int; arg : int  (** The argument to the function. *) }
[@@deriving sexp_of]

module Instruction_counter = struct
  type t = { mutable counter : int; limit : int option } [@@deriving sexp_of]

  let create ~limit = { limit; counter = 0 }

  let limit_reached t =
    match t.limit with Some limit -> t.counter >= limit | None -> false

  let incr t = t.counter <- t.counter + 1
end

let default_instruction_limit = 10000000

type t = {
  mutable pc : int; (* The program counter. *)
  mutable line : int; (* The current line number. *)
  function_stack : stack_frame Stack.t; (* The function stack. *)
  expression_stack : int Stack.t; (* The expression stack. *)
  world : World.t; (* The Karel world. *)
  (* Limits *)
  ic : Instruction_counter.t; (* The instruction counter. *)
  left_counter : Instruction_counter.t;
  forward_counter : Instruction_counter.t;
  pickbuzzer_counter : Instruction_counter.t;
  leavebuzzer_counter : Instruction_counter.t;
}
[@@deriving sexp_of, fields ~getters ~setters]

let create ?(instruction_limit = default_instruction_limit) ?left_limit
    ?forward_limit ?pickbuzzer_limit ?leavebuzzer_limit ~world () =
  {
    pc = 0;
    line = 0;
    function_stack = Stack.create ();
    expression_stack = Stack.create ();
    world;
    ic = Instruction_counter.create ~limit:(Some instruction_limit);
    left_counter = Instruction_counter.create ~limit:left_limit;
    forward_counter = Instruction_counter.create ~limit:forward_limit;
    pickbuzzer_counter = Instruction_counter.create ~limit:pickbuzzer_limit;
    leavebuzzer_counter = Instruction_counter.create ~limit:leavebuzzer_limit;
  }

let step t (program : (Instruction.t, Perms.Read.t) Array.Permissioned.t) =
  let module Array = Array.Permissioned in
  let continue () =
    (* Returns continue, but checks if no violation was made. *)
    t.pc <- t.pc + 1;
    if
      List.exists
        [
          t.ic;
          t.left_counter;
          t.forward_counter;
          t.pickbuzzer_counter;
          t.leavebuzzer_counter;
        ]
        ~f:Instruction_counter.limit_reached
    then Continue_or_stop.Stop Run_result.INSTRUCTION
    else if t.pc >= Array.length program then Stop Run_result.OK
    else Continue_or_stop.Continue ()
  in
  let instruction = Array.nget program t.pc in
  match instruction with
  | HALT -> Continue_or_stop.Stop Run_result.OK
  | LINE line ->
      t.line <- line;
      continue ()
  | LEFT ->
      Instruction_counter.incr t.ic;
      Instruction_counter.incr t.left_counter;
      World.turnleft t.world;
      continue ()
  | WORLDWALLS ->
      Stack.push t.expression_stack
        (World.wall_mask t.world |> World.Walls.to_int);
      continue ()
  | ORIENTATION ->
      Stack.push t.expression_stack
        (World.orientation t.world |> World.Orientation.to_int);
      continue ()
  | ROTL ->
      let o = Stack.pop_exn t.expression_stack |> World.Orientation.of_int in
      Stack.push t.expression_stack
        (World.Orientation.left o |> World.Orientation.to_int);
      continue ()
  | ROTR ->
      let o = Stack.pop_exn t.expression_stack |> World.Orientation.of_int in
      Stack.push t.expression_stack
        (World.Orientation.right o |> World.Orientation.to_int);
      continue ()
  | MASK ->
      let i = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (1 lsl i);
      continue ()
  | NOT ->
      let b = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (if b = 0 then 1 else 0);
      continue ()
  | AND ->
      let e1 = Stack.pop_exn t.expression_stack in
      let e2 = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (e1 land e2);
      continue ()
  | OR ->
      let e1 = Stack.pop_exn t.expression_stack in
      let e2 = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (e1 land e2);
      continue ()
  | EQ ->
      let e1 = Stack.pop_exn t.expression_stack in
      let e2 = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (if e1 = e2 then 1 else 0);
      continue ()
  | EZ result ->
      if Stack.pop_exn t.expression_stack = 0 then Stop result else continue ()
  | JMP jmp ->
      Instruction_counter.incr t.ic;
      t.pc <- t.pc + jmp;
      continue ()
  | JZ jmp ->
      Instruction_counter.incr t.ic;
      if Stack.pop_exn t.expression_stack = 0 then t.pc <- t.pc + jmp;
      continue ()
  | FORWARD ->
      Instruction_counter.incr t.ic;
      Instruction_counter.incr t.forward_counter;
      World.forward t.world;
      continue ()
  | WORLDBUZZERS ->
      Stack.push t.expression_stack (World.beepers t.world);
      continue ()
  | BAGBUZZERS ->
      Stack.push t.expression_stack (World.bag t.world);
      continue ()
  | PICKBUZZER ->
      Instruction_counter.incr t.ic;
      Instruction_counter.incr t.pickbuzzer_counter;
      World.pickbeeper t.world;
      continue ()
  | LEAVEBUZZER ->
      Instruction_counter.incr t.ic;
      Instruction_counter.incr t.leavebuzzer_counter;
      World.putbeeper t.world;
      continue ()
  | LOAD i ->
      Stack.push t.expression_stack i;
      continue ()
  | POP ->
      let (_ : int) = Stack.pop_exn t.expression_stack in
      continue ()
  | DUP ->
      Stack.push t.expression_stack (Stack.top_exn t.expression_stack);
      continue ()
  | DEC ->
      let i = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (i - 1);
      continue ()
  | INC ->
      let i = Stack.pop_exn t.expression_stack in
      Stack.push t.expression_stack (i + 1);
      continue ()
  | PARAM (_ : int) ->
      let frame = Stack.top_exn t.function_stack in
      Stack.push t.expression_stack frame.arg;
      continue ()
  | CALL (pc, (_ : string)) ->
      Instruction_counter.incr t.ic;
      let arg = Stack.pop_exn t.expression_stack in
      let frame = { pc = t.pc; arg } in
      Stack.push t.function_stack frame;
      t.pc <- pc - 1;
      continue ()
  | RET -> (
      match Stack.pop t.function_stack with
      | None ->
          (* It is a return from the main function. *)
          Stop OK
      | Some frame ->
          t.pc <- frame.pc;
          continue ())

let[@tailrec] rec line_step t
    (program : (Instruction.t, Perms.Read.t) Array.Permissioned.t) =
  match step t program with
  | Continue_or_stop.Stop result -> Continue_or_stop.Stop result
  | Continue () -> (
      let instruction = Array.Permissioned.nget program t.pc in
      match instruction with
      | Instruction.LINE _ -> Continue ()
      | _ -> line_step t program)

let[@tailrec] rec run t
    (program : (Instruction.t, Perms.Read.t) Array.Permissioned.t) =
  match step t program with
  | Continue_or_stop.Continue () -> run t program
  | Stop result -> result
