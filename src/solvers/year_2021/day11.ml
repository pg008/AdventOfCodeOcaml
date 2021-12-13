open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let parse data =
    data
    |> List.map (fun x ->
           x |> String.to_seq |> List.of_seq
           |> List.map (fun c -> int_of_char c - int_of_char '0')
           |> Array.of_list)
    |> Array.of_list

  let neighbours (i, j) =
    [
      (i - 1, j - 1);
      (i - 1, j);
      (i - 1, j + 1);
      (i, j - 1);
      (i, j + 1);
      (i + 1, j - 1);
      (i + 1, j);
      (i + 1, j + 1);
    ]

  let part1 matrix =
    let h = Array.length matrix in
    let w = Array.length matrix.(0) in
    let flash_eff (i, j) =
      if i < 0 || i >= h || j < 0 || j >= w then None
      else
        let cur = matrix.(i).(j) + 1 in
        matrix.(i).(j) <- cur;
        if cur = 10 then Some (i, j) else None
    in
    let rec step step_num =
      if step_num = 0 then []
      else
        let flashed = ref [] in
        (* Process time step *)
        for i = 0 to h - 1 do
          let row = matrix.(i) in
          for j = 0 to w - 1 do
            let cell = row.(j) + 1 in
            row.(j) <- cell;
            if cell = 10 then
              (* Flash neighbours *)
              flashed := (i, j) :: !flashed
          done
        done;
        let rec process flashed =
          match flashed with
          | [] -> []
          | x :: xs ->
              x :: process (List.filter_map flash_eff (neighbours x) @ xs)
        in
        let flashed = process !flashed in
        List.iter (fun (i, j) -> matrix.(i).(j) <- 0) flashed;
        flashed :: step (step_num - 1)
    in
    let flashes = step 100 in
    flashes

  let part2 matrix =
    let h = Array.length matrix in
    let w = Array.length matrix.(0) in
    let flash_eff (i, j) =
      if i < 0 || i >= h || j < 0 || j >= w then None
      else
        let cur = matrix.(i).(j) + 1 in
        matrix.(i).(j) <- cur;
        if cur = 10 then Some (i, j) else None
    in
    let rec step_until step_num =
      let flashed = ref [] in
      (* Process time step *)
      for i = 0 to h - 1 do
        let row = matrix.(i) in
        for j = 0 to w - 1 do
          let cell = row.(j) + 1 in
          row.(j) <- cell;
          if cell = 10 then (* Flash neighbours *)
            flashed := (i, j) :: !flashed
        done
      done;
      let rec process flashed =
        match flashed with
        | [] -> []
        | x :: xs -> x :: process (List.filter_map flash_eff (neighbours x) @ xs)
      in
      let flashed = process !flashed in
      List.iter (fun (i, j) -> matrix.(i).(j) <- 0) flashed;
      if List.length flashed = w * h then Some step_num
      else step_until (step_num + 1)
    in
    let flashes = step_until 1 in
    flashes

  let naloga1 (data : string) =
    let data = data |> List.lines |> parse in
    part1 data |> List.map List.length |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let data = data |> List.lines |> parse in
    match part2 data with
    | Some s -> s |> string_of_int
    | None -> failwith "no solution"
end
