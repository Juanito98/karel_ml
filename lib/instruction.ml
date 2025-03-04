(** A Karel instruction. Consisting of the name of the opcode and up to two *
    optional arguments. *)

open! Core

let int_of_yojson = function `Int n -> n | _ -> failwith "Expected an int"
let yojson_of_int i = `Int i

let string_of_yojson = function
  | `String s -> s
  | _ -> failwith "Could not convert Run_result.t to yojson"

let yojson_of_string t = `String t

type ('param, 'call) t' =
  | HALT (* Finish execution. *)
  | LINE of int (* Record the line number. *)
  | LEFT (* Turn left *)
  | WORLDWALLS
  (* Push the wall mask of Karel's current position onto the expression stack. *)
  | ORIENTATION
  (* Push the Karel's current orientation onto the expression stack. *)
  | ROTL
  (* Rotate left the top of the expression stack, assuming the top represents an orientation.
    The stack top is replaced with this value. *)
  | ROTR
  (* Rotate right the top of the expression stack, assuming the top represents an orientation.
    The stack top is replaced with this value. *)
  | MASK
  (* A left shift of 1 by the value at the top of the expression stack, useful for converting orientation into a wall mask.
    The stack top is replaced with this value. *)
  | NOT
  (* Logical negation of the top of the expression stack.
    The stack top is replaced with this value. *)
  | AND
  (* Logical AND of the top two values on the expression stack.
    The two stack top values are replaced with this value. *)
  | OR
  (* Logical OR of the top two values on the expression stack.
    The two stack top values are replaced with this value. *)
  | EQ
  (* Equality of the top two values on the expression stack. 1 if equal, 0 otherwise.
    The two stack top values are replaced with this value. *)
  | EZ of Run_result.t
  (* Throws the [Run_result.t] if the top of the expression stack is zero.
    Pops the top of the stack. *)
  | JMP of int
  (* Jumps the pc a number of lines *)
  | JZ of int
  (* Jumps the pc a number of lines if the top of the expression stack is zero. 
    Pops the top of the stack. *)
  | FORWARD
  (* Move forward. *)
  | WORLDBUZZERS
  (* Push the number of beepers in Karel's current position onto the expression stack. *)
  | BAGBUZZERS
  (* Pusth the number of beepers in bag onto the expression stack. *)
  | PICKBUZZER
  (* Pickbeeper. *)
  | LEAVEBUZZER
  (* Putbeeper*)
  | LOAD of int
  (* Loads the int onto the expression stack. *)
  | POP
  (* Pops the top of the expression stack. *)
  | DUP
  (* Duplicates the top of the expression stack. *)
  | DEC
  (* Decrements the top of the expression stack by 1. *)
  | INC
  (* Increments the top of the expression stack by 1. *)
  | PARAM of 'param
  (* Loads the param of the top of the function stack onto the expression stack. 
    The argument is used only on linking phase. *)
  | CALL of 'call
  (* Function call. Pops the top of the expression stack as the arg and push a stack frame in the function stack.
    Moves the pc to the [int] line, where the called fun begins. 
    The [string] argument is used only on linking phase. *)
  | RET
[@@deriving sexp, yojson]

type t = (int, int * string) t' [@@deriving sexp, yojson]
