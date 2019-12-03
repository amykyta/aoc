open Base
open Stdio

let rec fuel_requirement x = 
  let requirement = x / 3 - 2 in
  match requirement with
  | _ when requirement <= 0 -> 0
  | requirement -> requirement + (fuel_requirement requirement)

let rec total_fuel_requirement accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> total_fuel_requirement (accum + fuel_requirement (Int.of_string x))

let () =
  printf "Total: %i\n" (total_fuel_requirement 0)
