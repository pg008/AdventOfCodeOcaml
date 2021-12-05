open Solvers.Signature
open Utils.List_utils
module Gu = Utils.General_utils

module Solver : Solver = struct
  let most_common l = 1 + List.count '1' l > List.count '0' l

  let naloga1 (data : string) =
    let lines = data |> List.lines |> List.map List.list_of_string in
    let counter = List.transpose lines |> List.map most_common in
    let gamma = counter |> Gu.bin_to_decimal in
    let epsilon = counter |> List.map not |> Gu.bin_to_decimal in
    string_of_int (gamma * epsilon)

  let rec process tie (lines_t : (char list * char list) list) =
    match lines_t with
    | [] -> failwith "Error"
    | [ (orig, _) ] -> orig |> List.map (fun x -> x = '1') |> Gu.bin_to_decimal
    | lines_t ->
        let f =
          lines_t |> List.map snd
          |> List.map (fun l -> List.nth l 0)
          |> most_common
        in
        let f = if tie f then '1' else '0' in
        process tie
          (List.filter_map
             (fun (orig, c) ->
               match c with x :: xs when x = f -> Some (orig, xs) | _ -> None)
             lines_t)

  let naloga2 data _part1 =
    let lines_t = data |> List.lines |> List.map List.list_of_string in
    let dup = List.map (fun x -> (x, x)) lines_t in
    let oxy = process (fun x -> x) dup in
    let co2 = process not dup in
    oxy * co2 |> string_of_int
end
