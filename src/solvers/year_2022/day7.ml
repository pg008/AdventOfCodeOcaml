open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type drevo = Datoteka of string * int | Mapa of string * drevo list

  let prazna_mapa (ime : string) : drevo = Mapa (ime, [])

  let element_iz_niza niz =
    match String.split_on_char ' ' niz with
    | "$" :: _ -> failwith "to je ukaz, ne datoteka ali mapa"
    | "dir" :: ime :: _ -> prazna_mapa ime
    | velikost :: ime :: _ -> Datoteka (ime, int_of_string velikost)
    | _ -> failwith ("Ne razumem niza: " ^ niz)

  let je_ukaz niz =
    match String.split_on_char ' ' niz with "$" :: _ -> true | _ -> false

  let rec vsebina_mape = function
    | u :: _ when je_ukaz u -> []
    | u :: us -> element_iz_niza u :: vsebina_mape us
    | [] -> []

  let rec preostanek_ukazov = function
    | u :: us when je_ukaz u -> u :: us
    | _ :: us -> preostanek_ukazov us
    | [] -> []

  let odpri_mapo ime_mape (Mapa (i, v)) =
    let rec aux = function
      | (Mapa (ime, _) as m) :: ms -> if ime = ime_mape then m else aux ms
      | _ :: ms -> aux ms
      | [] -> failwith "Ne najdem mape"
    in
    aux v

  let zamenjaj_mapo (Mapa (i1, _) as m1) (Mapa (i2, v) as m2) =
    let rec aux = function
      | Mapa (ime, _) :: ms when ime = i1 -> m1 :: ms
      | m :: ms -> m :: aux ms
      | [] -> failwith "Ne najdem mape"
    in
    Mapa (i2, aux v)

  let rec dodaj_elemente elementi (Mapa (i, v)) =
    match elementi with
    | e :: es -> dodaj_elemente es (Mapa (i, e :: v))
    | [] -> Mapa (i, v)

  let rec velikost_elementa = function
    | Datoteka (_, v) -> v
    | Mapa (_, v) -> List.fold_left ( + ) 0 (List.map velikost_elementa v)

  let rec velikosti_map = function
    | Mapa (_, v) as m ->
        velikost_elementa m :: List.concat (List.map velikosti_map v)
    | Datoteka _ -> []

  let preberi_drevo ukazi =
    let rec aux trenutno_drevo ukazi =
      match (trenutno_drevo, ukazi) with
      | Datoteka _, _ -> failwith "Ne morem odpreti datoteke"
      | (Mapa (ime, _) as m), u :: us -> (
          match String.split_on_char ' ' u with
          | "$" :: "cd" :: "/" :: _ -> aux m (if ime = "/" then us else u :: us)
          | "$" :: "cd" :: ".." :: _ -> (m, us)
          | "$" :: "cd" :: notranja_mapa :: _ ->
              let posodobljena_mapa, neobdelani_ukazi =
                aux (odpri_mapo notranja_mapa m) us
              in
              aux (zamenjaj_mapo posodobljena_mapa m) neobdelani_ukazi
          | "$" :: "ls" :: _ ->
              let elementi = vsebina_mape us
              and neobdelani_ukazi = preostanek_ukazov us in
              aux (dodaj_elemente elementi m) neobdelani_ukazi
          | _ -> failwith ("Ne poznam ukaza: " ^ u) )
      | d, [] -> (d, [])
    in
    fst (aux (prazna_mapa "/") ukazi)

  let naloga1 data =
    let drevo = preberi_drevo (List.lines data) in
    drevo |> velikosti_map
    |> List.filter (( >= ) 100000)
    |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let velikosti_map = preberi_drevo (List.lines data) |> velikosti_map in
    let prostor_na_disku = 70000000 - List.hd velikosti_map in
    let primanjkljaj_prostora = 30000000 - prostor_na_disku
    and najmanjsi lst = List.fold_left min (List.hd lst) (List.tl lst) in
    najmanjsi (List.filter (( <= ) primanjkljaj_prostora) velikosti_map)
    |> string_of_int
end
