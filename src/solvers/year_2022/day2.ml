open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type oblika = Kamen | Skarje | List

  type izid = Poraz | Remi | Zmaga

  let oblika_iz_crke = function
    | "A" | "X" -> Kamen
    | "B" | "Y" -> List
    | "C" | "Z" -> Skarje
    | _ as t -> failwith ("ne poznam oblike " ^ t)

  let izid_iz_crke = function
    | "X" -> Poraz
    | "Y" -> Remi
    | "Z" -> Zmaga
    | _ as t -> failwith ("ne pozna izida " ^ t)

  let vrednost = function Kamen -> 1 | List -> 2 | Skarje -> 3

  let izid_igre a b =
    match (a, b) with
    | a, b when a = b -> Remi
    | Kamen, Skarje | Skarje, List | List, Kamen -> Poraz
    | _ -> Zmaga

  let tocke_za_izid a b =
    match izid_igre a b with Remi -> 3 | Poraz -> 0 | Zmaga -> 6

  let predvideni_obliki a izid =
    match (a, izid) with
    | t, Remi -> (a, t)
    | Kamen, i -> if i = Zmaga then (a, List) else (a, Skarje)
    | Skarje, i -> if i = Zmaga then (a, Kamen) else (a, List)
    | List, i -> if i = Zmaga then (a, Skarje) else (a, Kamen)

  let tocke_igre a b = tocke_za_izid a b + vrednost b

  let obliki_iz_niza n =
    match String.split_on_char ' ' n with
    | a :: b :: _ -> (oblika_iz_crke a, oblika_iz_crke b)
    | _ -> failwith "napačen niz"

  let pravi_obliki_iz_niza n =
    match String.split_on_char ' ' n with
    | a :: b :: _ -> predvideni_obliki (oblika_iz_crke a) (izid_iz_crke b)
    | _ -> failwith "napačen niz"

  let skupni_sestevek f poteze =
    List.fold_left
      (fun xs n ->
        let a, b = f n in
        xs + tocke_igre a b)
      0 poteze

  let naloga1 data =
    let lines = List.lines data in
    lines |> skupni_sestevek obliki_iz_niza |> string_of_int

  let naloga2 data _part1 =
    let lines = List.lines data in
    lines |> skupni_sestevek pravi_obliki_iz_niza |> string_of_int
end
