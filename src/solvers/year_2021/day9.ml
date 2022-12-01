open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  module IntCounter = Utils.Map_utils.Counter.Make (Int)

  let parse data =
    data
    |> List.map (fun x ->
           x |> String.to_seq |> List.of_seq
           |> List.map (fun c -> int_of_char c - int_of_char '0')
           |> Array.of_list)
    |> Array.of_list

  let find_low matrix =
    let lower = ref [] in
    let h = Array.length matrix in
    let w = Array.length matrix.(0) in
    let get (i, j) =
      if i < 0 || i >= h || j < 0 || j >= w then 10 else matrix.(i).(j)
    in
    let neighbours i j =
      List.map get [ (i, j - 1); (i, j + 1); (i - 1, j); (i + 1, j) ]
    in
    for i = 0 to h - 1 do
      let row = matrix.(i) in
      for j = 0 to w - 1 do
        let cell = row.(j) in
        let n = neighbours i j in
        if List.for_all (fun n -> cell < n) n then
          lower := (cell, (i, j)) :: !lower
      done
    done;
    !lower |> List.map (fun (x, _) -> x + 1) |> List.sum

  let find_basin matrix =
    let h = Array.length matrix in
    let w = Array.length matrix.(0) in
    let num = ref (-1) in
    let get (i, j) =
      if i < 0 || i >= h || j < 0 || j >= w then 10 else matrix.(i).(j)
    in
    let neighbours i j = [ (i, j - 1); (i, j + 1); (i - 1, j); (i + 1, j) ] in
    let valid (i, j) =
      let cell = get (i, j) in
      cell >= 0 && cell < 9
    in
    let rec flood_fill num (i, j) =
      if valid (i, j) then (
        matrix.(i).(j) <- num;
        List.iter (flood_fill num) (neighbours i j) )
      else ()
    in
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        if valid (i, j) then (
          flood_fill !num (i, j);
          num := !num - 1 )
      done
    done;
    let line = matrix |> Array.to_list |> Array.concat in
    let counter =
      Array.fold_left
        (fun c x -> if x < 0 then IntCounter.plus x 1 c else c)
        IntCounter.empty line
    in
    let[@warning "-8"] ((_, v1) :: (_, v2) :: (_, v3) :: _) =
      IntCounter.most_common counter
    in
    v1 * v2 * v3

  let naloga1 (data : string) =
    let data = data |> List.lines |> parse in
    find_low data |> string_of_int

  let naloga2 data _part1 =
    let data = data |> List.lines |> parse in
    find_basin data |> string_of_int
end
