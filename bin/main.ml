open Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let compile_command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file) in
     fun () ->
       let input = In_channel.read_all filename in
       let program = Karel_compiler.Compiler.compile input in
       print_s [%sexp (program : Karel_compiler.Instruction.t list)])

let render_command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command _ = flag "-f" no_arg ~doc:"" in
     fun () -> Karel_compiler.Renderer.render ())

let () =
  Command.group ~summary:"Karel the compiler"
    [ ("compile", compile_command); ("render", render_command) ]
  |> Command_unix.run ~verbose_on_parse_error:true
