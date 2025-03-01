open Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command filename = anon ("filename" %: regular_file) in
     fun () ->
       let input = In_channel.read_all filename in
       let ast = Karel_compiler.Main.parse input |> ok_exn in
       print_s [%sexp (ast : Karel_compiler.Ast.program)])

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
