open Solvers.Signature
open Utils.List_utils
open Utils.Map_utils

module Solver : Solver = struct
  module CMap = Map.Make (Char)

  let points =
    CMap.of_bindings [ (')', 3); (']', 57); ('}', 1197); ('>', 25137) ]

  let points_missing =
    CMap.of_bindings [ (')', 1); (']', 2); ('}', 3); ('>', 4) ]

  let pairs = [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

  let open_close =
    CMap.of_bindings [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

  let _close_open = pairs |> List.map (fun (x, y) -> (y, x)) |> CMap.of_bindings

  let part1 line =
    let rec solve line stack =
      (* let x = List.hd line in
         Printf.printf "x:  %c: " x;
         List.iter (Printf.printf "%c, ") stack;
         Printf.printf "\n"; *)
      match (line, stack) with
      | [], _ -> None
      | x :: xs, _ when CMap.mem x open_close ->
          solve xs (CMap.find x open_close :: stack)
      | x :: xs, y :: ys when CMap.mem x points && x = y -> solve xs ys
      | x :: _, _ -> Some (CMap.find x points)
      (* | _ -> failwith "Unmatched close" *)
    in
    solve line []

  let part2 line =
    let rec solve line stack =
      (* let x = List.hd line in
         Printf.printf "x:  %c: " x;
         List.iter (Printf.printf "%c, ") stack;
         Printf.printf "\n"; *)
      match (line, stack) with
      | [], _ -> Some stack
      | x :: xs, _ when CMap.mem x open_close ->
          solve xs (CMap.find x open_close :: stack)
      | x :: xs, y :: ys when CMap.mem x points && x = y -> solve xs ys
      | _ -> None
      (* | _ -> failwith "Unmatched close" *)
    in
    solve line []

  let score_stack l =
    List.fold_left (fun s c -> (s * 5) + CMap.find c points_missing) 0 l

  let naloga1 (data : string) =
    let data = data |> List.lines |> List.map List.list_of_string in
    data |> List.filter_map part1 |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let data = data |> List.lines |> List.map List.list_of_string in
    let data =
      data |> List.filter_map part2 |> List.map score_stack |> List.sort compare
    in
    List.nth data (List.length data / 2) |> string_of_int
end
