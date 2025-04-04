open! Core
module I = Notty.I
module A = Notty.A

let with_notty f =
  let term = Notty_unix.Term.create () in
  let x = f term in
  Notty_unix.Term.release term;
  x

let grid cells = List.map cells ~f:I.hcat |> I.vcat

let cell_img world position =
  let wall_mask = World.wall_mask_of world position in
  let v_wall =
    if World.Walls.wall_in wall_mask World.Orientation.West then
      I.char A.empty '|' 1 1
    else I.void 1 1
  in
  let cell_img =
    if [%compare.equal: int * int] (World.position world) position then
      let orientation =
        match World.orientation world with
        | World.Orientation.North -> '^'
        | South -> 'v'
        | East -> '>'
        | West -> '<'
      in
      I.char A.(fg blue ++ st bold) orientation 1 1
    else
      match World.beepers_of world position with
      | 0 -> I.char A.empty '.' 1 1
      | x when x >= 10 -> I.char A.empty 'B' 1 1
      | x -> I.strf "%d" x
  in
  let h_wall =
    if World.Walls.wall_in wall_mask World.Orientation.South then
      I.char A.empty '-' 1 1
    else I.void 1 1
  in
  grid [ [ v_wall; cell_img ]; [ I.char A.empty 'x' 1 1; h_wall ] ]

let world_img world =
  let m = World.width world in
  let n = World.height world in
  let body =
    I.tabulate (m + 1) (n + 1) (fun x y -> cell_img world (x + 1, n - y + 1))
    |> I.crop ~t:1 ~r:1
  in
  let info =
    I.vcat
      [
        I.strf "Bag: %d" (World.bag world);
        I.strf "Worldbuzzers: %d" (World.beepers world);
        I.strf "Position: (%d, %d)"
          (World.position world |> fst)
          (World.position world |> snd);
      ]
  in
  I.hcat [ body; I.void 2 1; info ]

let render runtime program =
  with_notty (fun term ->
      let[@tailrec] rec loop () =
        match Runtime.line_step runtime program with
        | Continue_or_stop.Continue () ->
            Notty_unix.Term.image term (world_img (Runtime.world runtime));
            let (_ : float) = Core_unix.nanosleep 0.05 in
            loop ()
        | Stop result -> result
      in
      Notty_unix.Term.image term (world_img (Runtime.world runtime));
      loop ())
