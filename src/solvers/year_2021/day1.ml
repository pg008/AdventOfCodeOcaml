open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec process = function
    | x :: y :: xs -> (if y > x then 1 else 0) + process (y :: xs)
    | _ -> 0

  let naloga1 data =
    let lines = data |> List.lines |> List.int_list in
    process lines |> string_of_int

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.int_list in
    let _, r2 = List.split lines 1 in
    let _, r3 = List.split r2 1 in
    List.map2_unsafe ( + ) r3 (List.map2_unsafe ( + ) lines r2)
    |> process |> string_of_int
end
