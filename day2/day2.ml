open Base
open Stdio

let input = [| 1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;13;1;19;1;19;10;23;2;10;23;27;1;27;6;31;1;13;31;35;1;13;35;39;1;39;10;43;2;43;13;47;1;47;9;51;2;51;13;55;1;5;55;59;2;59;9;63;1;13;63;67;2;13;67;71;1;71;5;75;2;75;13;79;1;79;6;83;1;83;5;87;2;87;6;91;1;5;91;95;1;95;13;99;2;99;6;103;1;5;103;107;1;107;9;111;2;6;111;115;1;5;115;119;1;119;2;123;1;6;123;0;99;2;14;0;0 |]

let target_output = 19690720

let nouns = List.init 100 ~f:(fun x -> x)
let verbs = List.init 100 ~f:(fun x -> x)

exception Weird of string

let rec run_prog prog index =
  let op_code = prog.(index) in
  match op_code with
  | 99 -> prog.(0) (* terminate and return value at index 0 *)
  | 1 -> prog.(prog.(index + 3)) <- prog.(prog.(index + 1)) + prog.(prog.(index + 2)); run_prog prog (index + 4)
  | 2 -> prog.(prog.(index + 3)) <- prog.(prog.(index + 1)) * prog.(prog.(index + 2)); run_prog prog (index + 4)
  | _ -> raise (Weird "incorrect op code")

let cartesian a b = 
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e,e')) b) a)

let noun_verb_pairs = cartesian nouns verbs

let initialize_program (noun, verb) = 
  let new_program = Array.copy input in
  new_program.(1) <- noun;
  new_program.(2) <- verb;
  new_program

let is_winner pair = 
  let new_program = initialize_program pair in
  let result = run_prog new_program 0 in
  result = target_output

let (noun, verb) = 
  match List.find ~f:is_winner noun_verb_pairs with
  | None -> raise (Weird "Didn't find a valid noun verb")
  | Some winner -> winner
  
let () =
  printf "Part 1: Answer: %i\n" (run_prog input 0)

let () =
  printf "Part 2: Noun: %i -- Verb: %i\n" noun verb

