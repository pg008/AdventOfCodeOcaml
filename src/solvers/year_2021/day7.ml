open Solvers.Signature
module CrabCounter = Utils.Map_utils.Counter.Make (Int)
open Utils.List_utils

module Solver : Solver = struct
  type crabs = int CrabCounter.t

  let parse data : crabs =
    data |> String.split_on_char ',' |> List.map int_of_string
    |> List.fold_left (fun c x -> CrabCounter.plus x 1 c) CrabCounter.empty

  let cost crabs point =
    CrabCounter.fold
      (fun loc num cost -> cost + (abs (loc - point) * num))
      crabs 0

  let cost2 crabs point =
    CrabCounter.fold
      (fun loc num cost ->
        let ch = abs (loc - point) in
        cost + (ch * (ch + 1) / 2 * num))
      crabs 0

  let process cost crabs : int =
    let mi = CrabCounter.min_binding crabs |> fst in
    let ma = CrabCounter.max_binding crabs |> fst in
    List.fold_right
      (fun point s ->
        let new_cost = cost crabs point in
        min s new_cost)
      (List.init (ma - mi + 1) (fun x -> x + mi))
      Int.max_int

  let naloga1 data =
    let crabs = data |> parse in
    crabs |> process cost |> string_of_int

  let naloga2 data _part1 =
    let crabs = data |> parse in
    crabs |> process cost2 |> string_of_int
end
