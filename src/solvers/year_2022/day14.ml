open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type polnilo = Zrak | Skala | Pesek | Tla

  exception PadecVNeskoncnost

  exception ZapolnjenIzvir

  let niz_polnila = function
    | Zrak -> "."
    | Skala -> "#"
    | Pesek -> "o"
    | Tla -> "%"

  type koordinata = int * int

  let koordinata_iz_seznama = function
    | [ x; y ] -> (x, y)
    | _ -> failwith "Seznam ne predstavlja koordinate"

  type linija = koordinata * koordinata

  let min_max_koordinate (linije : linija list) : koordinata * koordinata =
    let max_min_koordinata primerjalna_funkcija zac linije =
      List.fold_left
        (fun acc l ->
          let x0 = fst acc and y0 = snd acc and k1 = fst l and k2 = snd l in
          let x1 = primerjalna_funkcija (fst k1) (fst k2)
          and y1 = primerjalna_funkcija (snd k1) (snd k2) in
          (primerjalna_funkcija x0 x1, primerjalna_funkcija y0 y1))
        zac linije
    in
    let xmin, ymin = max_min_koordinata min (max_int, max_int) linije
    and xmax, ymax = max_min_koordinata max (0, 0) linije in
    ((xmin, ymin), (xmax, ymax))

  let rec koordinate_linije (((x, y) as k1), ((x', y') as k2)) : koordinata list
      =
    if x = x' then
      let len = y' - y + 1 in
      if len <= 0 then koordinate_linije (k2, k1)
      else List.init len (fun i -> (x, y + i))
    else
      let len = x' - x + 1 in
      if len <= 0 then koordinate_linije (k2, k1)
      else List.init len (fun i -> (x + i, y))

  let preberi_linije niz : linija list =
    let koordinate =
      let r = Str.regexp {| -> |} in
      Str.split r niz
      |> List.map (fun k ->
             String.split_on_char ',' k |> List.map int_of_string
             |> koordinata_iz_seznama)
    in
    let k1 = koordinate |> List.rev |> List.tl |> List.rev
    and k2 = List.tl koordinate in
    List.combine k1 k2

  let ( +: ) (x, y) (x', y') = (x + x', y + y')

  let ( -: ) (x, y) (x', y') = (x - x', y - y')

  type poligon = {
    zgoraj_levo : int * int;
    polja : polnilo Array.t Array.t;
    mutable vrh_kupcka : koordinata option;
  }

  let indeks_iz_koordinate zgoraj_levo k =
    let j, i = k -: zgoraj_levo in
    (i, j)

  let pridobi poligon koordinata =
    let i, j = indeks_iz_koordinate poligon.zgoraj_levo koordinata in
    poligon.polja.(i).(j)

  let nastavi poligon koordinata vrednost =
    let i, j = indeks_iz_koordinate poligon.zgoraj_levo koordinata in
    poligon.polja.(i).(j) <- vrednost

  let preberi_poligon niz =
    let linije =
      String.split_on_char '\n' niz |> List.map preberi_linije |> List.concat
    in
    let (x0, y0), (x1, y1) = min_max_koordinate linije in
    let y0' = 0 and y1' = y1 + 2 in
    let x0' = min x0 (500 - y1' - 10) and x1' = max x1 (500 + y1' + 10) in
    let sirina = x1' - x0' + 1
    and visina = y1' + 1
    and koordinate_skal = linije |> List.map koordinate_linije |> List.concat
    and koordinate_tal = koordinate_linije ((x0', y1'), (x1', y1')) in
    let poligon =
      {
        zgoraj_levo = (x0', y0');
        polja = Array.make_matrix visina sirina Zrak;
        vrh_kupcka = None;
      }
    in
    List.iter
      (fun k ->
        nastavi poligon k Skala;
        if
          fst k = 500
          && ( poligon.vrh_kupcka = None
             || snd (Option.get poligon.vrh_kupcka) > snd k )
        then poligon.vrh_kupcka <- Some (500, snd k))
      koordinate_skal;
    List.iter (fun k -> nastavi poligon k Tla) koordinate_tal;
    poligon

  let niz_poligona poligon =
    let niz_vrstice arr =
      Array.map niz_polnila arr |> Array.to_list |> String.concat ""
    in
    Array.map niz_vrstice poligon.polja |> Array.to_list |> String.concat "\n"

  let rec simuliraj_padec poligon (polozaj : koordinata) : unit =
    if polozaj = (500, 0) then
      simuliraj_padec poligon (Option.get poligon.vrh_kupcka -: (0, 1))
    else
      let kl, k0, kd =
        (polozaj +: (-1, 1), polozaj +: (0, 1), polozaj +: (1, 1))
      in
      try
        match pridobi poligon k0 with
        | Zrak -> simuliraj_padec poligon k0
        | Tla -> raise (Invalid_argument "")
        | Skala | Pesek -> (
            match pridobi poligon kl with
            | Zrak -> simuliraj_padec poligon kl
            | Tla -> raise (Invalid_argument "")
            | Skala | Pesek -> (
                match pridobi poligon kd with
                | Zrak -> simuliraj_padec poligon kd
                | Tla -> raise (Invalid_argument "")
                | Skala | Pesek ->
                    nastavi poligon polozaj Pesek;
                    if
                      fst polozaj = 500
                      && snd polozaj < snd (Option.get poligon.vrh_kupcka)
                    then poligon.vrh_kupcka <- Some polozaj ) )
      with Invalid_argument _ -> raise PadecVNeskoncnost

  let rec simuliraj_padec_s_tlemi poligon (polozaj : koordinata) : unit =
    if polozaj = (500, 0) then
      if pridobi poligon polozaj = Pesek then raise ZapolnjenIzvir
      else
        simuliraj_padec_s_tlemi poligon (Option.get poligon.vrh_kupcka -: (0, 1))
    else
      let kl, k0, kd =
        (polozaj +: (-1, 1), polozaj +: (0, 1), polozaj +: (1, 1))
      in
      try
        match pridobi poligon k0 with
        | Zrak -> simuliraj_padec_s_tlemi poligon k0
        | Skala | Pesek | Tla -> (
            match pridobi poligon kl with
            | Zrak -> simuliraj_padec_s_tlemi poligon kl
            | Skala | Pesek | Tla -> (
                match pridobi poligon kd with
                | Zrak -> simuliraj_padec_s_tlemi poligon kd
                | Skala | Pesek | Tla ->
                    nastavi poligon polozaj Pesek;
                    if
                      fst polozaj = 500
                      && snd polozaj < snd (Option.get poligon.vrh_kupcka)
                    then poligon.vrh_kupcka <- Some polozaj ) )
      with Invalid_argument _ -> raise PadecVNeskoncnost

  let prestej_padce poligon : int =
    let nov_poligon = { poligon with polja = Array.(map copy) poligon.polja } in
    let rec aux st_padcev =
      try
        simuliraj_padec nov_poligon (500, 0);
        aux (st_padcev + 1)
      with PadecVNeskoncnost -> st_padcev
    in
    aux 0

  let prestej_padce2 poligon : int =
    let nov_poligon = { poligon with polja = Array.(map copy) poligon.polja } in
    let rec aux st_padcev =
      try
        simuliraj_padec_s_tlemi nov_poligon (500, 0);
        aux (st_padcev + 1)
      with
      | ZapolnjenIzvir -> st_padcev
      | PadecVNeskoncnost -> failwith "zmanjkalo mi je tal"
    in
    aux 0

  let naloga1 data = preberi_poligon data |> prestej_padce |> string_of_int

  let naloga2 data _part1 =
    preberi_poligon data |> prestej_padce2 |> string_of_int
end
