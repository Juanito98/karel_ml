open! Core

(** A list of programs we would like to test for all stages *)
let programs =
  [
    ( "simple program",
      {|
class program {
  program() {
    turnoff();
  }
}
  |} );
    ( "simple statements",
      {|
class program {
  program() {
    move();
    pickbeeper();
    putbeeper();
    turnleft();
    turnoff();
  }
}
  |}
    );
    ( "simple if",
      {|
class program {
  program() {
    if (frontIsClear) {
      move();
    }
    turnoff();
  }
}
        |}
    );
    ( "if else",
      {|
class program {
  program() {
    if (frontIsClear) {
      move();
    } else {
      turnleft();
    }
    turnoff();
  }
}
        |}
    );
    ( "nested if/else",
      {|
class program {
  program() {
    if (frontIsClear)
      if (nextToABeeper) move();
      else turnleft();
    else putbeeper();
  }
}
        |}
    );
    ( "simple while",
      {|
class program {
  program() {
    while (frontIsClear) {
      move();
    }
    turnoff();
  }
}
    |}
    );
    ( "multiple conditions",
      {|
class program {
  program() {
    if (!frontIsClear && (leftIsClear || notFacingNorth)) {
      move();
    }
  }
}
    |}
    );
    ( "simple iterate",
      {|
class program {
  program() {
    iterate (5) {
      move();
    }
    turnoff();
  }
}
    |}
    );
    ( "simple call",
      {|
class program {
  void turnright() {
    iterate (3) {
      turnleft();
    }
  }
  
  program() {
    turnright();
    turnoff();
  }
}
    |}
    );
    ( "simple call with arg",
      {|
class program {  
  void turn(x) {
    iterate (x) {
      turnleft();
    }
  }
  
  program() {
    turn(2);
    turnoff();
  }
}
    |}
    );
  ]
  |> List.map ~f:(fun (name, code) -> (name, String.strip code))

let print_program (name, code) =
  let lines = String.split_lines code in
  print_endline name;
  print_endline (String.make (String.length name + 1) '-');
  lines |> List.iteri ~f:(fun i line -> printf "%*d| %s\n" 2 (i + 1) line)
