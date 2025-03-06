module World = Karel_compiler.World

let%expect_test _ =
  let world = World.create ~width:10 ~height:5 () in
  let img = Karel_compiler.Renderer.world_img world in
  Notty_unix.output_image img;
  [%expect
    {|
    x-x-x-x-x-x-x-x-x-x-x  Bag: 0
    |. . . . . . . . . .|  Worldbuzzers: 0
    x x x x x x x x x x x  Position: (1, 1)
    |. . . . . . . . . .|
    x x x x x x x x x x x
    |. . . . . . . . . .|
    x x x x x x x x x x x
    |. . . . . . . . . .|
    x x x x x x x x x x x
    |^ . . . . . . . . .|
    x-x-x-x-x-x-x-x-x-x-x |}]
