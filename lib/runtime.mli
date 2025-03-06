(** A class that holds the state of computation and executes opcodes.

    The Karel Virtual Machine is a simple, stack-based virtual machine with a
    small number of opcodes, based loosely on the Java Virtual Machine. All
    opcodes are represented as an array where the first element is the opcode
    name, followed by zero or one parameters. *)

open! Core

type t [@@deriving sexp_of]

val create :
  ?instruction_limit:int ->
  ?left_limit:int ->
  ?forward_limit:int ->
  ?pickbuzzer_limit:int ->
  ?leavebuzzer_limit:int ->
  world:World.t ->
  unit ->
  t

val world : t -> World.t

val step :
  t ->
  (Instruction.t, Perms.Read.t) Array.Permissioned.t ->
  (unit, Run_result.t) Continue_or_stop.t
(** Executes a single step of computation. *)

val line_step :
  t ->
  (Instruction.t, Perms.Read.t) Array.Permissioned.t ->
  (unit, Run_result.t) Continue_or_stop.t
(** Executes steps until the next LINE instruction. *)

val run :
  t -> (Instruction.t, Perms.Read.t) Array.Permissioned.t -> Run_result.t
(** Executes the full program. *)
