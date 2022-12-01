open Solvers.Signature
open Utils.List_utils
module Gu = Utils.General_utils
module IntSet = Set.Make (Int)

module Solver : Solver = struct
  type board = { rows : IntSet.t list; columns : IntSet.t list }

  type game = { numbers : int list; boards : board list }

  let parse_board lst =
    assert (List.length lst = 6);
    let[@warning "-8"] (_ :: lst) = lst in
    let boards =
      List.map (fun l -> l |> List.split_white |> List.int_list) lst
    in
    List.iter (fun x -> assert (List.length x = 5)) boards;
    let rows = List.map IntSet.of_list boards in
    let columns = List.map IntSet.of_list (List.transpose boards) in
    { rows; columns }

  let parse_data data =
    let[@warning "-8"] (numbers :: boards) = List.lines data in
    let numbers = numbers |> String.split_on_char ',' |> List.int_list in
    let boards = List.map parse_board (List.chunkify 6 boards) in
    { numbers; boards }

  let play_number number { rows; columns } =
    let remover = List.map (IntSet.remove number) in
    let rows = remover rows in
    let columns = remover columns in
    { rows; columns }

  let is_finished { rows; columns } =
    List.exists IntSet.is_empty rows || List.exists IntSet.is_empty columns

  let rec play { numbers; boards } =
    match numbers with
    | [] -> failwith "Run out of numbers"
    | x :: xs ->
        let new_boards = List.map (play_number x) boards in
        if List.exists is_finished new_boards then (x, new_boards)
        else play { numbers = xs; boards = new_boards }

  let sum_n { rows; _ } =
    List.fold_right ( + ) (List.map (fun s -> IntSet.fold ( + ) s 0) rows) 0

  let rec play_last { numbers; boards } =
    match numbers with
    | [] -> failwith "Run out of numbers"
    | x :: xs -> (
        let new_boards = List.map (play_number x) boards in
        let win, keep = List.partition is_finished new_boards in
        match (win, keep) with
        | [ b ], [] -> (x, b)
        | _, [] -> failwith "Bug in implementation"
        | _, new_boards -> play_last { numbers = xs; boards = new_boards } )

  let naloga1 (data : string) =
    let game = parse_data data in
    let num, boards = play game in
    let win = List.find is_finished boards in
    num * sum_n win |> string_of_int

  let naloga2 data _part1 =
    let game = parse_data data in
    let num, win = play_last game in
    num * sum_n win |> string_of_int
end
