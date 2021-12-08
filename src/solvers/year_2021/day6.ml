open Solvers.Signature
module FishCounter = Utils.Map_utils.Counter.Make (Int)
open Utils.List_utils

module Solver : Solver = struct
  type fish_set = int FishCounter.t

  let parse data : fish_set =
    data |> String.split_on_char ',' |> List.map int_of_string
    |> List.fold_left (fun c x -> FishCounter.plus x 1 c) FishCounter.empty

  let live counter = if counter = 0 then [ 6; 8 ] else [ counter - 1 ]

  let rec multiply fish day =
    if day = 0 then fish
    else
      multiply
        (FishCounter.fold
           (fun day num new_fish ->
             List.fold_right
               (fun x -> FishCounter.plus x num)
               (live day) new_fish)
           fish FishCounter.empty)
        (day - 1)

  let naloga1 data =
    let fish = data |> parse in
    let after = multiply fish 80 in
    after |> FishCounter.values |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let fish = data |> parse in
    let after = multiply fish 256 in
    after |> FishCounter.values |> List.sum |> string_of_int
end
