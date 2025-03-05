(** Converts the AST to an intermediate representation. *)

open! Core

(* An internal representation of the instruction, where the CALL and
PARAM variants are not linked yet. *)
type instruction_ir = (string, string) Instruction.t' [@@deriving sexp_of]

type def_ir = {
  name : string;
  arg : string option;
  body : instruction_ir list;
  line : int;
      (* The line in the source code where the function definition was found. *)
}
[@@deriving sexp_of]

type ir = { defs : def_ir list; main : instruction_ir list }
[@@deriving sexp_of]

let start_line_of_loc loc =
  let start = fst loc in
  start.Ast.pos_lnum

let rec bool_expr_ir = function
  | Ast.FrontIsClear -> [ Instruction.WORLDWALLS; ORIENTATION; MASK; AND; NOT ]
  | LeftIsClear -> [ Instruction.WORLDWALLS; ORIENTATION; ROTL; MASK; AND; NOT ]
  | RightIsClear ->
      [ Instruction.WORLDWALLS; ORIENTATION; ROTR; MASK; AND; NOT ]
  | FacingWest -> [ Instruction.ORIENTATION; LOAD 0; EQ ]
  | FacingNorth -> [ Instruction.ORIENTATION; LOAD 1; EQ ]
  | FacingEast -> [ Instruction.ORIENTATION; LOAD 2; EQ ]
  | FacingSouth -> [ Instruction.ORIENTATION; LOAD 3; EQ ]
  | NextToABeeper -> [ Instruction.WORLDBUZZERS; LOAD 0; EQ; NOT ]
  | AnyBeepersInBeeperBag -> [ Instruction.BAGBUZZERS; LOAD 0; EQ; NOT ]
  | And (e1, e2) -> bool_expr_ir e1 @ bool_expr_ir e2 @ [ AND ]
  | Or (e1, e2) -> bool_expr_ir e1 @ bool_expr_ir e2 @ [ OR ]
  | Not e -> bool_expr_ir e @ [ NOT ]
  | IsZero int_expr -> int_expr_ir int_expr @ [ Instruction.NOT ]

and int_expr_ir = function
  | Int i -> [ Instruction.LOAD i ]
  | Succ e -> int_expr_ir e @ [ INC ]
  | Pred e -> int_expr_ir e @ [ DEC ]
  | Var x -> [ PARAM x ]

let rec statement_to_ir = function
  | Ast.Block block -> block_to_ir block
  | Move loc ->
      [ Instruction.LINE (start_line_of_loc loc) ]
      @ bool_expr_ir Ast.FrontIsClear
      @ [ EZ Run_result.WALL; FORWARD ]
  | PickBeeper loc ->
      [
        LINE (start_line_of_loc loc);
        WORLDBUZZERS;
        EZ Run_result.WORLDUNDERFLOW;
        PICKBUZZER;
      ]
  | PutBeeper loc ->
      [
        LINE (start_line_of_loc loc);
        BAGBUZZERS;
        EZ Run_result.BAGUNDERFLOW;
        LEAVEBUZZER;
      ]
  | Return loc -> [ LINE (start_line_of_loc loc); RET ]
  | TurnLeft loc -> [ LINE (start_line_of_loc loc); LEFT ]
  | TurnOff loc -> [ LINE (start_line_of_loc loc); HALT ]
  | If (loc, cond, i, e) -> (
      let cond_ir = bool_expr_ir cond in
      let if_ir = statement_to_ir i in
      match statement_to_ir e with
      | [] ->
          [ Instruction.LINE (start_line_of_loc loc) ]
          @ cond_ir
          @ [ Instruction.JZ (List.length if_ir) ]
          @ if_ir
      | else_ir ->
          [ Instruction.LINE (start_line_of_loc loc) ]
          @ cond_ir
          @ [ Instruction.JZ (List.length if_ir + 1) ]
          @ if_ir
          @ [ Instruction.JMP (List.length else_ir) ]
          @ else_ir)
  | While (loc, cond, statement) ->
      let cond_ir = bool_expr_ir cond in
      let statement_ir = statement_to_ir statement in
      [ Instruction.LINE (start_line_of_loc loc) ]
      @ cond_ir
      @ [ Instruction.JZ (List.length statement_ir + 1) ]
      @ statement_ir
      @ [ JMP (-(1 + List.length cond_ir + 1 + List.length statement_ir)) ]
  | Iterate (loc, int_expr, statement) ->
      let expr_ir = int_expr_ir int_expr in
      let statement_ir = statement_to_ir statement in
      [ Instruction.LINE (start_line_of_loc loc) ]
      @ expr_ir
      @ [ Instruction.DUP; LOAD 0; EQ; NOT; JZ (List.length statement_ir + 1) ]
      @ statement_ir
      @ [ Instruction.DEC; JMP (-(5 + List.length statement_ir + 1)); POP ]
  | Call (loc, arg, int_expr) ->
      let expr_ir =
        match int_expr with
        | None -> [ Instruction.LOAD 0 ]
        | Some int_expr -> int_expr_ir int_expr
      in
      [ Instruction.LINE (start_line_of_loc loc) ] @ expr_ir @ [ CALL arg ]

and block_to_ir block = List.map block ~f:statement_to_ir |> List.concat

let def_to_ir = function
  | Ast.Def (loc, name, arg, body) ->
      {
        name;
        arg;
        body = block_to_ir body @ [ RET ];
        line = start_line_of_loc loc;
      }

let main_to_ir = function Ast.Main body -> block_to_ir body

let ast_to_ir = function
  | Ast.Program (defs, main) ->
      { defs = List.map defs ~f:def_to_ir; main = main_to_ir main }
