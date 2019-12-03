open Base
open Stdio

let fuel_requirement x = x / 3  - 2

let rec total_fuel_requirement accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> total_fuel_requirement (accum + fuel_requirement (Int.of_string x))

let () =
  printf "Total: %i\n" (total_fuel_requirement 0)


