open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec vsi_elementi_razlicni = function
    | x :: xs -> (not (List.mem x xs)) && vsi_elementi_razlicni xs
    | [] -> true

  let prvih_n_elementov n =
    let rec aux i acc lst =
      match (i, lst) with
      | 0, _ -> List.rev acc
      | i, x :: xs -> aux (i - 1) (x :: acc) xs
      | _, [] -> failwith "Seznam nima dovolj elementov"
    in
    aux n []

  let indeks_n_razlicnih n =
    let rec aux indeks = function
      | x :: xs ->
          if vsi_elementi_razlicni (x :: prvih_n_elementov n xs) then indeks
          else aux (indeks + 1) xs
      | _ -> failwith "Ne najdem markerja"
    in
    aux n

  let indeks_markerja = indeks_n_razlicnih 4

  let indeks_sporocila = indeks_n_razlicnih 14

  let naloga1 data =
    data |> List.list_of_string |> indeks_markerja |> string_of_int

  let naloga2 data _part1 =
    data |> List.list_of_string |> indeks_sporocila |> string_of_int
end
