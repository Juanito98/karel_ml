open! Core

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving sexp_of]

type loc = position * position [@@deriving sexp_of]
type identifier = string [@@deriving sexp_of]

type int_expr =
  | Int of int
  | Var of string
  | Succ of int_expr
  | Pred of int_expr
[@@deriving sexp_of]

type bool_expr =
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr
  | IsZero of int_expr
  | FrontIsClear
  | LeftIsClear
  | RightIsClear
  | FacingNorth
  | FacingSouth
  | FacingEast
  | FacingWest
  | NextToABeeper
  | AnyBeepersInBeeperBag
[@@deriving sexp_of]

type block = statement list [@@deriving sexp_of]

and statement =
  | Block of block
  | Move
  | PickBeeper
  | PutBeeper
  | Return
  | TurnLeft
  | TurnOff
  | If of bool_expr * statement * statement
  | While of bool_expr * statement
  | Iterate of int_expr * statement
  | Call of identifier * int_expr option
[@@deriving sexp_of]

type def = Def of identifier * identifier option * block [@@deriving sexp_of]
type program_def = Main of block [@@deriving sexp_of]
type program = Program of def list * program_def [@@deriving sexp_of]
