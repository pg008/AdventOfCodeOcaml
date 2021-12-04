open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type direction = Forward | Up | Down

  type move = Move of (direction * int)

  type location = Location of (int * int)

  let move (Location (x, y)) (Move (dir, n)) =
    match dir with
    | Forward -> Location (x + n, y)
    | Up -> Location (x, y - n)
    | Down -> Location (x, y + n)

  let move_aim (aim, (Location (x, y) as loc)) (Move (dir, n)) =
    match dir with
    | Forward -> (aim, Location (x + n, y + (aim * n)))
    | Up -> (aim - n, loc)
    | Down -> (aim + n, loc)

  let parse_direction = function
    | "forward" -> Forward
    | "down" -> Down
    | "up" -> Up
    | _ -> failwith "INVALID FORMAT"

  let parse_instruction s =
    Scanf.sscanf s "%s %d" (fun d n -> Move (parse_direction d, n))

  let naloga1 data =
    let lines = data |> List.lines |> List.map parse_instruction in
    let (Location (x, y)) = List.fold_left move (Location (0, 0)) lines in
    x * y |> string_of_int

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.map parse_instruction in
    let _, Location (x, y) =
      List.fold_left move_aim (0, Location (0, 0)) lines
    in
    x * y |> string_of_int
end
