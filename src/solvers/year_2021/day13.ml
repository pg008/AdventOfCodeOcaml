open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type data = { board : bool Array.t Array.t; folder : (int * bool) list }

  let parse data =
    let lines, folder = data |> List.lines |> List.split_on (fun x -> x = "") in
    let lines =
      lines |> List.map (fun x -> Scanf.sscanf x "%d,%d" (fun x y -> (x, y)))
    in
    let folder =
      folder
      |> List.map (fun x ->
             Scanf.sscanf x "fold along %s" (fun x ->
                 let[@warning "-8"] [ x; n ] = String.split_on_char '=' x in

                 (int_of_string n, x = "x")))
    in
    let m_x = (lines |> List.map fst |> List.maximum) + 1 in
    let m_y = (lines |> List.map snd |> List.maximum) + 1 in
    let board = Array.make_matrix m_y m_x false in
    List.iter (fun (y, x) -> board.(x).(y) <- true) lines;
    { board; folder }

  let count_points m =
    m |> Array.map Array.to_list |> Array.to_list |> List.concat
    |> List.count true

  let board_to_string board =
    board
    |> Array.map (fun line ->
           Array.map (fun x -> if x then "█" else " ") line
           |> Array.to_list |> String.concat "")
    |> Array.to_list |> String.concat "\n"

  (* let print_board board = Printf.printf "BOARD: \n%s\n" (board_to_string board) *)

  let solve { board; folder } limit =
    let process board (num, reverse) =
      let board = if reverse then List.transpose_matrix board else board in
      let h = Array.length board in
      let w = Array.length board.(0) in
      let folded = Array.make_matrix num w false in
      Array.blit board 0 folded 0 num;
      let folded = Array.copy folded in
      (* print_board board; *)
      (* print_board folded; *)
      assert (num = h / 2);
      for j = num + 1 to h - 1 do
        for i = 0 to w - 1 do
          folded.(h - j - 1).(i) <- board.(j).(i) || folded.(h - j - 1).(i)
        done
      done;
      let folded = if reverse then List.transpose_matrix folded else folded in
      (* print_board folded; *)
      folded
    in
    let folds = List.take limit folder in
    List.fold_left process board folds

  let naloga1 (data : string) =
    let data = parse data in
    let sol = solve data 1 in
    sol |> count_points |> string_of_int

  (* Has an error, but can be read *)
  let naloga2 data _part1 =
    let data = parse data in
    let sol = solve data (List.length data.folder) in
    board_to_string sol
end
