(** The state of the Karel world. *)

open! Core

let k_infinity = Int.max_value

module Orientation = struct
  type t = West | North | East | South [@@deriving sexp_of, enum]

  let left = function
    | North -> West
    | West -> South
    | South -> East
    | East -> North

  let right = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let to_int = to_enum
  let of_int i = i land 3 |> of_enum |> Option.value_exn
end

module Walls = struct
  type t = int [@@deriving sexp_of]

  let to_int t : t = t
  let empty : t = 0

  let add t ~orientation =
    let b = 1 lsl Orientation.to_int orientation in
    t lor b

  let wall_in t orientation =
    let b = 1 lsl Orientation.to_int orientation in
    t land b > 0
end

module Position = struct
  type t = int * int [@@deriving sexp, hash, compare]

  let x = fst
  let y = snd

  let move ((x, y) : t) orientation : t =
    match orientation with
    | Orientation.North -> (x, y + 1)
    | South -> (x, y - 1)
    | East -> (x + 1, y)
    | West -> (x - 1, y)
end

let default_width = 100
let default_height = 100
let default_position : Position.t = (1, 1)
let default_orientation = Orientation.North
let default_bag = 0

type t = {
  width : int;
  height : int;
  mutable position : Position.t;  (** Karel's position. *)
  mutable orientation : Orientation.t;  (** Karel's orientation. *)
  mutable bag : int;  (** The number of beepers in the Karel's beeper bag. *)
  beepers : (Position.t, int) Hashtbl.t;
      (** The number of beepers at each position. *)
  walls : (Position.t, Walls.t) Hashtbl.t;
      (** The wall mask at each position. (Excluding world boundaries ) *)
}
[@@deriving sexp_of, fields ~getters ~setters]

let create ?(width = default_width) ?(height = default_height)
    ?(position = default_position) ?(orientation = default_orientation)
    ?(bag = default_bag) ?(beepers = Hashtbl.create (module Position))
    ?(walls = Hashtbl.create (module Position)) () =
  { width; height; position; orientation; bag; beepers; walls }

let turnleft t = set_orientation t (Orientation.left t.orientation)
let forward t = set_position t (Position.move t.position t.orientation)

let beepers_of t position =
  Hashtbl.find t.beepers position |> Option.value ~default:0

let beepers t = beepers_of t t.position

let incr_world_beepers t delta =
  Hashtbl.change t.beepers t.position ~f:(fun beepers ->
      match Option.value beepers ~default:0 |> Int.( + ) delta with
      | 0 -> None
      | beepers -> Some beepers)

let incr_bag t delta = if not (t.bag = k_infinity) then t.bag <- t.bag + delta

let pickbeeper t =
  incr_bag t 1;
  incr_world_beepers t (-1)

let putbeeper t =
  incr_bag t (-1);
  incr_world_beepers t 1

let out_of_boundaries t (x, y) = x > t.width || y > t.height

let wall_mask_of t position =
  let wall_mask =
    Hashtbl.find t.walls position |> Option.value ~default:Walls.empty
  in
  let wall_mask =
    if Position.x position = 1 || out_of_boundaries t position then
      Walls.add wall_mask ~orientation:West
    else wall_mask
  in
  let wall_mask =
    if Position.x position = t.width || out_of_boundaries t position then
      Walls.add wall_mask ~orientation:East
    else wall_mask
  in
  let wall_mask =
    if Position.y position = 1 || out_of_boundaries t position then
      Walls.add wall_mask ~orientation:South
    else wall_mask
  in
  let wall_mask =
    if Position.y position = t.height || out_of_boundaries t position then
      Walls.add wall_mask ~orientation:North
    else wall_mask
  in
  wall_mask

let wall_mask t = wall_mask_of t t.position
