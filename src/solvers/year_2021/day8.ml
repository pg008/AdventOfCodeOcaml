open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  module Cset = Set.Make (Char)
  module CSetMap = Map.Make (Cset)

  type problem = { train : Cset.t list; test : Cset.t list }

  let parse (line : string) : problem =
    Scanf.sscanf line "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s"
      (fun d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 t1 t2 t3 t4 ->
        {
          train =
            [ d0; d1; d2; d3; d4; d5; d6; d7; d8; d9 ]
            |> List.map (fun x -> x |> List.list_of_string |> Cset.of_list);
          test =
            [ t1; t2; t3; t4 ]
            |> List.map (fun x -> x |> List.list_of_string |> Cset.of_list);
        })

  let naloga1 (data : string) =
    let data = data |> List.lines |> List.map parse in
    data
    |> List.map (fun x -> x.test)
    |> List.concat |> List.map Cset.cardinal
    |> List.filter (fun x -> List.mem x [ 2; 3; 4; 7 ])
    |> List.length |> string_of_int

  let[@warning "-8-27"] detect problem =
    let one = problem.train |> List.find (fun x -> Cset.cardinal x = 2) in
    let seven = problem.train |> List.find (fun x -> Cset.cardinal x = 3) in
    let four = problem.train |> List.find (fun x -> Cset.cardinal x = 4) in
    let eight = problem.train |> List.find (fun x -> Cset.cardinal x = 7) in
    let [ h1 ] = Cset.diff seven one |> Cset.elements in
    let rh = Cset.inter seven one in
    assert (Cset.cardinal rh = 2);
    let known = [ one; seven; four; eight ] in
    let rest =
      problem.train |> List.filter (fun x -> List.mem x known |> not)
    in
    assert (List.length rest = 6);
    let three =
      List.find
        (fun x ->
          Cset.inter x rh |> Cset.cardinal = 2
          && Cset.cardinal x = 5
          && Cset.inter x four |> Cset.cardinal = 3)
        rest
    in
    let two =
      List.find
        (fun x -> Cset.cardinal x = 5 && Cset.inter x four |> Cset.cardinal = 2)
        rest
    in
    let five =
      List.find
        (fun x ->
          Cset.inter x rh |> Cset.cardinal = 1
          && Cset.cardinal x = 5
          && Cset.inter x four |> Cset.cardinal = 3)
        rest
    in
    let known = known @ [ three; two; five ] in
    let mmd = Cset.diff (Cset.inter three four) rh in
    assert (Cset.cardinal mmd = 1);
    let [ zero ] =
      problem.train
      |> List.filter (fun x ->
             Cset.inter mmd x |> Cset.is_empty && List.mem x known |> not)
    in
    let known = known @ [ zero ] in
    let nine =
      List.find
        (fun x ->
          Cset.cardinal x = 6
          && Cset.inter x rh |> Cset.cardinal = 2
          && List.mem x known |> not)
        problem.train
    in
    let six =
      List.find
        (fun x ->
          Cset.cardinal x = 6
          && Cset.inter x rh |> Cset.cardinal = 1
          && List.mem x known |> not)
        problem.train
    in
    let converter =
      [
        (zero, 0);
        (one, 1);
        (two, 2);
        (three, 3);
        (four, 4);
        (five, 5);
        (six, 6);
        (seven, 7);
        (eight, 8);
        (nine, 9);
      ]
      |> List.to_seq |> CSetMap.of_seq
    in
    problem.test
    |> List.map (fun x -> converter |> CSetMap.find x)
    |> List.map string_of_int |> String.concat "" |> int_of_string

  let naloga2 data _part1 =
    let data = data |> List.lines |> List.map parse in
    data |> List.map detect |> List.sum |> string_of_int
end
