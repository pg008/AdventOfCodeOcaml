open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let prednost c =
    let koda = Char.code c in
    if 65 <= koda && koda <= 90 then koda - 64 + 26
    else if 97 <= koda && koda <= 122 then koda - 96
    else failwith ("Napačen znak: " ^ Char.escaped c)

  let razdeli_na_pol l = List.split_on_n l (List.length l / 2)

  let rec podvojeni_element l1 = function
    | x :: xs -> if List.mem x l1 then x else podvojeni_element l1 xs
    | [] -> 0

  let rec podvojeni_element_treh l1 l2 = function
    | x :: xs ->
        if List.mem x l1 && List.mem x l2 then x
        else podvojeni_element_treh l1 l2 xs
    | [] -> 0

  let skupine = List.chunkify 3

  let vrednost_niza n =
    let l1, l2 = razdeli_na_pol (List.list_of_string n |> List.map prednost) in
    podvojeni_element l1 l2

  let vrednost_skupine skupina =
    match
      skupina |> List.map List.list_of_string |> List.map (List.map prednost)
    with
    | [ s1; s2; s3 ] -> podvojeni_element_treh s1 s2 s3
    | _ -> failwith "Napačna skupina"

  let naloga1 data =
    List.lines data |> List.map vrednost_niza |> List.sum |> string_of_int

  let naloga2 data _part1 =
    List.lines data |> skupine |> List.map vrednost_skupine |> List.sum
    |> string_of_int
end
