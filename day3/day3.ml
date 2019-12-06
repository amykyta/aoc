
let wire_1_input = String.split_on_char ',' "R8,U5,L5,D3"

let wire_2_input = String.split_on_char ',' "U7,R6,D4,L4"

let parse_wire_path path = 
  let parse str =
    (str.[0], int_of_string(String.sub str 1 (String.length str - 1))) in
  List.map parse path

let path_1 = parse_wire_path wire_1_input 
let path_2 = parse_wire_path wire_2_input

exception Weird of string

let vector_to_coordinate vector =
  let (direction, magnitude) = vector in
  match direction with
    | 'L' -> (-magnitude, 0)
    | 'R' -> (magnitude, 0)
    | 'D' -> (0, -magnitude)
    | 'U' -> (0, magnitude)
    | _ -> raise (Weird "invalid direction")

let next_coordinate accum point_delta =
  match accum with
  | [] -> [point_delta]
  | (x,y)::_ ->
      let (x_d, y_d) = point_delta in
      (x + x_d, y + y_d) :: accum

let path_1_trace = List.rev(List.fold_left next_coordinate [] (List.map vector_to_coordinate path_1))

let path_2_trace = List.rev(List.fold_left next_coordinate [] (List.map vector_to_coordinate path_2))

let rec no_duplicates l =
  match l with
  | [] -> []
  | hd :: tl ->
      if (List.mem hd tl) then (no_duplicates tl) else hd :: (no_duplicates tl)

let unduped_1 = no_duplicates path_1_trace
let unduped_2 = no_duplicates path_2_trace

let intersection unduped_1 unduped_2 =
  let (intersections, _) = List.partition (fun x -> List.mem x unduped_2) unduped_1 in intersections

     

  
