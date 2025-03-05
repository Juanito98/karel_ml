open! Core

(** Performs the final linking step on a program. It first performs semantic
    validation of the function declarations and calls, and then stitches
    together all the function definitions, resolves calls, and ensures that all
    parameters have been correctly resolved. *)
let link ({ defs; main } : Ir.ir) =
  let _, functions =
    (* The function definitions will be appended below the main instructions.
    This calculates the offset in the final instruction list where the function will
    begin. *)
    List.fold_map ~init:(List.length main) defs
      ~f:(fun offset { name; arg; body; line = _ } ->
        (offset + List.length body, (name, (offset, arg))))
  in
  let functions =
    match Map.of_alist (module String) functions with
    | `Duplicate_key k -> failwithf "Function %s redefined." k ()
    | `Ok functions -> functions
  in

  let transform body arg ~line =
    let (_ : int), res =
      List.fold_map body ~init:line ~f:(fun acc instruction ->
          match instruction with
          | Instruction.CALL f -> (
              match Map.find functions f with
              | None -> failwithf "Function %s not found " f ()
              | Some (offset, _arg) -> (acc, Instruction.CALL (offset, f)))
          | PARAM x -> (
              match arg with
              | Some arg when String.equal arg x -> (acc, PARAM 0)
              | Some _ | None -> failwithf "Undefined variable %s" x ())
          | LINE line -> (line, Instruction.LINE line)
          | ( EZ _ | JMP _ | JZ _ | HALT | LEFT | WORLDWALLS | ORIENTATION
            | ROTL | ROTR | MASK | NOT | AND | OR | EQ | FORWARD | WORLDBUZZERS
            | BAGBUZZERS | PICKBUZZER | LEAVEBUZZER | LOAD _ | POP | DUP | DEC
            | INC | RET ) as instruction ->
              (acc, instruction))
    in
    res
  in

  transform main None ~line:1
  @ (List.map defs ~f:(fun { name = _; line; arg; body } ->
         transform body arg ~line)
    |> List.concat)
