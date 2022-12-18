open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type vektor3 = int * int * int

  type stranica = vektor3 * vektor3

  module StranicaSet = Set.Make (struct
    type t = stranica

    let compare = compare
  end)

  let vektor_iz_niza niz =
    match String.split_on_char ',' niz |> List.map int_of_string with
    | [ k1; k2; k3 ] -> (k1, k2, k3)
    | _ -> failwith ("Napačen zapis vektorja: " ^ niz)

  let stranica_iz_vektorjev (v1, v2) = if v1 < v2 then (v1, v2) else (v2, v1)

  let stranice_vektorja (v1, v2, v3) =
    let sosednje_kocke =
      [
        (v1 + 1, v2, v3);
        (v1 - 1, v2, v3);
        (v1, v2 + 1, v3);
        (v1, v2 - 1, v3);
        (v1, v2, v3 + 1);
        (v1, v2, v3 - 1);
      ]
    in
    List.combine sosednje_kocke (List.init 6 (fun _ -> (v1, v2, v3)))
    |> List.map stranica_iz_vektorjev

  let naloga1 data =
    let kocke = List.lines data |> List.map vektor_iz_niza in
    let stranice = Hashtbl.create (List.length kocke / 2) in
    List.iter
      (fun v ->
        stranice_vektorja v
        |> List.iter (fun s ->
               if Hashtbl.mem stranice s then Hashtbl.replace stranice s false
               else Hashtbl.add stranice s true))
      kocke;
    Hashtbl.fold
      (fun _ zunanja acc -> if zunanja then acc + 1 else acc)
      stranice 0
    |> string_of_int

  let naloga2 data _part1 = ""
end
