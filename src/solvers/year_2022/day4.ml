open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type obmocje = { zacetek : int; konec : int }

  let obseg_obmocja o = o.konec - o.zacetek + 1

  let rec vsebuje_drugo_obmocje o1 o2 =
    if obseg_obmocja o1 >= obseg_obmocja o2 then
      o1.zacetek <= o2.zacetek && o2.konec <= o1.konec
    else vsebuje_drugo_obmocje o2 o1

  let rec se_obmocji_prekrivata o1 o2 =
    if o1.zacetek <= o2.zacetek then o2.zacetek <= o1.konec
    else se_obmocji_prekrivata o2 o1

  let obmocje_iz_niza n =
    match String.split_on_char '-' n |> List.map int_of_string with
    | [ z; k ] -> { zacetek = z; konec = k }
    | _ -> failwith ("Neprimeren format območja: " ^ n ^ ".")

  let obmocji_iz_niza n =
    match String.split_on_char ',' n |> List.map obmocje_iz_niza with
    | [ o1; o2 ] -> (o1, o2)
    | _ -> failwith ("Neprimerno število območij" ^ n ^ ".")

  let naloga1 data =
    List.lines data |> List.map obmocji_iz_niza
    |> List.map (fun (o1, o2) -> vsebuje_drugo_obmocje o1 o2)
    |> List.count true |> string_of_int

  let naloga2 data _part1 =
    List.lines data |> List.map obmocji_iz_niza
    |> List.map (fun (o1, o2) -> se_obmocji_prekrivata o1 o2)
    |> List.count true |> string_of_int
end
