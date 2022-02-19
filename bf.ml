(*
 * A really simple brainfuck interpreter written in OCaml.
 *
 * Copyright 2022 Shawn Anastasio
 * This code is licensed under the MIT license (see LICENSE.txt for details)
 *)

exception BadOperation

type operation =
  | IncrementPtr
  | DecrementPtr
  | Increment
  | Decrement
  | Output
  | Input
  | Jump
  | Land

let operation_to_char = function
  | IncrementPtr -> '>'
  | DecrementPtr -> '<'
  | Increment -> '+'
  | Decrement -> '-'
  | Output -> '.'
  | Input -> ','
  | Jump -> '['
  | Land -> ']'

let operation_from_char = function
  | '>' -> IncrementPtr
  | '<' -> DecrementPtr
  | '+' -> Increment
  | '-' -> Decrement
  | '.' -> Output
  | ',' -> Input
  | '[' -> Jump
  | ']' -> Land
  | _ -> raise BadOperation

let load_program_from_file path =
  let input = open_in path in
  let rec load_file acc =
    match operation_from_char (input_char input) with
    | exception End_of_file -> acc
    | exception BadOperation -> load_file acc
    | op -> load_file (op :: acc)
  in

  List.rev (load_file [])

let dump_program program =
  Printf.fprintf stderr "[INFO] Dumping brainfuck program:\n";
  List.iter (fun c -> Printf.fprintf stderr "%c" (operation_to_char c)) program;
  Printf.fprintf stderr "\n"

type bfstate = {
  memory : char array;
  program : operation array;
  dp : int; (* Data pointer *)
  ip : int; (* Instruction pointer *)
  jump_stack : int list;
}

let bfstate_make program : bfstate =
  { memory = Array.make 30000 (char_of_int 0); program = Array.of_list program; dp = 0; ip = 0; jump_stack = [] }

let bfstate_run starting_bfstate =
  (* helpers *)
  let getmem bfstate = int_of_char (Array.get bfstate.memory bfstate.dp) in
  let setmem bfstate value = Array.set bfstate.memory bfstate.dp (char_of_int (value land 0xFF)) in

  let exception TargetNotFound in
  (* find next Land after given ip *)
  let rec find_next_land bfstate i n =
    if i >= Array.length bfstate.program then raise TargetNotFound
    else
      match Array.get bfstate.program i with
      | Land when n = 0 -> i (* match found! *)
      | Land -> find_next_land bfstate (i + 1) (n - 1) (* not the Land we want *)
      | Jump -> find_next_land bfstate (i + 1) (n + 1) (* increment n to skip this jump's land*)
      | _ -> find_next_land bfstate (i + 1) n
  in

  (* Recursive Brainfuck opcode interpreter*)
  let rec exec_op = function
    | bfstate when bfstate.ip >= Array.length bfstate.program -> bfstate
    | bfstate -> (
        (* Increment ip *)
        let bfstate = { bfstate with ip = bfstate.ip + 1 } in

        (* Execute opcode at program[ip-1] *)
        match Array.get bfstate.program (bfstate.ip - 1) with
        | IncrementPtr -> exec_op { bfstate with dp = bfstate.dp + 1 }
        | DecrementPtr -> exec_op { bfstate with dp = bfstate.dp - 1 }
        | Increment ->
            setmem bfstate (getmem bfstate + 1);
            exec_op bfstate
        | Decrement ->
            setmem bfstate (getmem bfstate - 1);
            exec_op bfstate
        | Output ->
            print_char (char_of_int (getmem bfstate));
            exec_op bfstate
        | Input ->
            setmem bfstate (int_of_char (input_char stdin));
            exec_op bfstate
        | Jump -> (
            match getmem bfstate with
            | 0x00 -> exec_op { bfstate with ip = 1 + find_next_land bfstate bfstate.ip 0 }
            | _ -> exec_op { bfstate with jump_stack = bfstate.ip :: bfstate.jump_stack })
        | Land -> (
            match getmem bfstate with
            | 0x00 -> exec_op { bfstate with jump_stack = List.tl bfstate.jump_stack }
            | _ -> exec_op { bfstate with ip = List.hd bfstate.jump_stack }))
  in

  (* GO! *)
  exec_op starting_bfstate

let () =
  let usage = "bf <program.bf>" in
  let input_file = ref "" in
  let anon_fun filename = input_file := filename in
  let speclist = [] in
  Arg.parse speclist anon_fun usage;

  match !input_file with
  | "" ->
      Printf.fprintf stderr "[ERROR] No input file specified. See --help.\n";
      Stdlib.exit 1
  | _ ->
      Printf.fprintf stderr "[INFO] Opening file %s\n" !input_file;

      let program = load_program_from_file !input_file in
      dump_program program;
      flush_all ();

      let _ = bfstate_run (bfstate_make program) in
      ()
