open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let preberi_ukaz vr ukaz =
    match String.split_on_char ' ' ukaz with
    | [ "noop" ] -> (vr, [ vr ])
    | [ "addx"; n ] -> (vr + int_of_string n, [ vr; vr ])
    | _ -> failwith "Napačen ukaz"

  let izrisi_vrstico lst =
    lst
    |> List.mapi (fun i v -> if abs (i - v) <= 1 then "#" else ".")
    |> String.concat ""

  let vrednosti_registra ukazi =
    ukazi |> List.fold_left_map preberi_ukaz 1 |> snd |> List.flatten

  let naloga1 data =
    data |> List.lines |> vrednosti_registra
    |> List.mapi (fun i v -> (i + 1) * v)
    |> List.filteri (fun i _ -> (i + 1) mod 40 = 20)
    |> List.filteri (fun i _ -> i <= 5)
    |> List.sum |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> vrednosti_registra |> List.chunkify 40
    |> List.map izrisi_vrstico |> String.concat "\n"
end
