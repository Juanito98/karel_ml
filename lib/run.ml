open! Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let exec_command =
  Command.basic ~summary:"Executes the program"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file)
     and interactive = flag "-i" no_arg ~doc:"BOOL interactive mode" in
     fun () ->
       let program =
         In_channel.read_all filename
         |> Compiler.compile |> Array.Permissioned.of_list
       in
       let runtime =
         In_channel.input_all In_channel.stdin |> World_in.load_xml
       in
       let result =
         if interactive then Renderer.render runtime program
         else Runtime.run runtime program
       in
       print_s [%sexp (result : Run_result.t)];
       print_s [%sexp (runtime : Runtime.t)])

let run_command =
  Command.basic ~summary:"Runs the compiled karel code"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file) in
     fun () ->
       let world = World.create () in
       let runtime = Runtime.create ~world () in
       let program = In_channel.read_all filename |> Sexp.of_string in
       let program =
         [%of_sexp: Instruction.t list] program |> Array.Permissioned.of_list
       in

       let result = Runtime.run runtime program in
       print_s [%sexp (result : Run_result.t)];
       print_s [%sexp (world : World.t)])

let compile_command =
  Command.basic ~summary:"Compiles source file"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file)
     and output_file =
       flag "-output-file" ~aliases:[ "-o" ] (optional string)
         ~doc:"FILENAME of the output"
     in
     fun () ->
       let output_file =
         match output_file with
         | Some output_file -> output_file
         | None -> [%string "%{Filename.chop_extension filename}.kx"]
       in
       let input = In_channel.read_all filename in
       let program = Compiler.compile input in
       Out_channel.write_all output_file
         ~data:(Sexp.to_string_hum [%sexp (program : Instruction.t list)]))

let command =
  Command.group ~summary:"Karel the compiler"
    [
      ("compile", compile_command);
      ("run", run_command);
      ("execute", exec_command);
    ]

let run () = Command_unix.run ~verbose_on_parse_error:true command
