type t

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
end

module Position : sig
  type t = int * int (* (x, y) -> (column, row) *)
end

val orientation : t -> Orientation.t
(** Returns the Karel's current orientation.*)

val bag : t -> int
val turnleft : t -> unit
val forward : t -> unit
val pickbeeper : t -> unit
val putbeeper : t -> unit
val beepers : t -> int

val wall_mask : t -> Walls.t
(** Returns the wall mask of the Karel's current position.*)

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
