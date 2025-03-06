open! Core

type t [@@deriving sexp_of]

module Orientation : sig
  type t = West | North | East | South

  val to_int : t -> int
  val left : t -> t
  val right : t -> t

  val of_int : int -> t
  (** It only considers the first 2 bits *)
end

module Walls : sig
  type t

  val to_int : t -> int
  val wall_in : t -> Orientation.t -> bool
end

module Position : sig
  type t = int * int [@@deriving sexp, compare, hash]
  (* (x, y) -> (column, row) *)
end

val orientation : t -> Orientation.t
(** Returns the Karel's current orientation. *)

val position : t -> Position.t
(** Returns the Karel's current position. *)

val bag : t -> int
val turnleft : t -> unit
val forward : t -> unit
val pickbeeper : t -> unit
val putbeeper : t -> unit

val beepers : t -> int
(** Returns the number of beepers in Karel's current position. *)

val beepers_of : t -> Position.t -> int

val wall_mask : t -> Walls.t
(** Returns the wall mask of the Karel's current position.*)

val wall_mask_of : t -> Position.t -> Walls.t

val create :
  ?width:int ->
  ?height:int ->
  ?position:Position.t ->
  ?orientation:Orientation.t ->
  ?bag:int ->
  ?beepers:(Position.t, int) Base.Hashtbl.t ->
  ?walls:(Position.t, int) Base.Hashtbl.t ->
  unit ->
  t

val width : t -> int
val height : t -> int
