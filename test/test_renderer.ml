module World = Karel_compiler.World

let%expect_test _ =
  let world = World.create ~width:5 ~height:5 () in
  let img = Karel_compiler.Renderer.world_img world in
  Notty_unix.output_image img;
  [%expect
    {|
    x-x-x-x-x-x
    |. . . . .|
    x x x x x x
    |. . . . .|
    x x x x x x
    |. . . . .|
    x x x x x x
    |. . . . .|
    x x x x x x
    |^ . . . .|
    x-x-x-x-x-x |}]
