open! Core

let find_arg args arg =
  List.find_map args ~f:(fun (tag, value) ->
      if String.equal tag arg then Some value else None)

let find_arg_exn args arg = find_arg args arg |> Option.value_exn
let raise xml = failwith [%string "Could not parse %{(Xml.to_string_fmt xml)}"]

let load_xml input =
  (* For world *)
  let width = ref None in
  let height = ref None in
  let position = ref None in
  let orientation = ref None in
  let bag = ref None in
  let beepers = Hashtbl.create (module World.Position) in
  let walls = Hashtbl.create (module World.Position) in

  (* For runtime. *)
  let instruction_limit = ref None in

  (match Xml.parse_string input with
  | Element ("ejecucion", [], ejecucion) ->
      List.iter ejecucion ~f:(function
        | Element ("condiciones", tags, []) ->
            instruction_limit :=
              Some
                (find_arg_exn tags "instruccionesMaximasAEjecutar"
                |> Int.of_string);
            let stack_limit =
              find_arg_exn tags "longitudStack" |> Int.of_string
            in
            printf "stack limit: %d\n" stack_limit
        | Element ("mundos", [], [ Element ("mundo", args, mundo) ]) ->
            width := Some (find_arg_exn args "ancho" |> Int.of_string);
            height := Some (find_arg_exn args "alto" |> Int.of_string);
            List.iter mundo ~f:(function
              | Element ("monton", tags, []) ->
                  let x = find_arg_exn tags "x" |> Int.of_string in
                  let y = find_arg_exn tags "y" |> Int.of_string in
                  let b =
                    match find_arg_exn tags "zumbadores" with
                    | "INFINITO" -> World.k_infinity
                    | x -> Int.of_string x
                  in
                  Hashtbl.set beepers ~key:(x, y) ~data:b
              | Element ("pared", tags, []) -> (
                  let add_wall (x, y) orientation =
                    Hashtbl.change walls (x, y) ~f:(fun wall_mask ->
                        let wall_mask =
                          Option.value wall_mask ~default:World.Walls.empty
                        in
                        Some (World.Walls.add wall_mask ~orientation))
                  in
                  match
                    ( find_arg tags "x1",
                      find_arg tags "x2",
                      find_arg tags "y1",
                      find_arg tags "y2" )
                  with
                  | Some _x1, Some x2, Some y1, _ ->
                      let x = Int.of_string x2 in
                      let y = Int.of_string y1 in
                      add_wall (x, y) North;
                      add_wall (x, y + 1) South
                  | Some x1, _, Some _y1, Some y2 ->
                      let x = Int.of_string x1 in
                      let y = Int.of_string y2 in
                      add_wall (x, y) East;
                      add_wall (x + 1, y) West
                  | _ ->
                      failwithf "Invalid wall args: %s"
                        (List.map
                           ~f:(fun (name, arg) -> [%string "%{name}:%{arg}"])
                           tags
                        |> String.concat ~sep:",")
                        ())
              | Element ("posicionDump", _tags, []) -> ()
              | xml -> raise xml)
        | Element ("programas", _, [ Element ("programa", args, programa) ]) ->
            let x_karel = find_arg_exn args "xKarel" |> Int.of_string in
            let y_karel = find_arg_exn args "yKarel" |> Int.of_string in
            position := Some (x_karel, y_karel);
            orientation :=
              Some
                (match find_arg_exn args "direccionKarel" with
                | "NORTE" -> World.Orientation.North
                | "SUR" -> South
                | "ESTE" -> East
                | "OESTE" -> West
                | s -> failwithf "Invalid orientation %s" s ());
            bag :=
              Some
                (match find_arg_exn args "mochilaKarel" with
                | "INFINITO" -> World.k_infinity
                | x -> Int.of_string x);
            List.iter programa ~f:(function
              | Element ("despliega", _tags, []) -> ()
              | xml -> raise xml)
        | xml -> raise xml)
  | xml -> raise xml);

  let world =
    World.create ?width:!width ?height:!height ?position:!position
      ?orientation:!orientation ?bag:!bag ~beepers ~walls ()
  in

  Runtime.create ?instruction_limit:!instruction_limit ~world ()
