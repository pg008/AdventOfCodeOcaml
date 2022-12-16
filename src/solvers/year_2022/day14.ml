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
    izvir : koordinata;
  }

  let prazen_poligon zgoraj_levo sirina visina polnilo =
    {
      zgoraj_levo;
      polja = Array.make_matrix visina sirina polnilo;
      izvir = (500, 0);
    }

  let indeks_iz_koordinate zgoraj_levo k =
    let j, i = k -: zgoraj_levo in
    (i, j)

  let pridobi poligon koordinata =
    let i, j = indeks_iz_koordinate poligon.zgoraj_levo koordinata in
    poligon.polja.(i).(j)

  let pridobi_opt poligon koordinata =
    let i, j = indeks_iz_koordinate poligon.zgoraj_levo koordinata in
    try Some poligon.polja.(i).(j) with Invalid_argument _ -> None

  let nastavi poligon koordinata vrednost =
    let i, j = indeks_iz_koordinate poligon.zgoraj_levo koordinata in
    poligon.polja.(i).(j) <- vrednost

  let preberi_poligon niz =
    let linije =
      String.split_on_char '\n' niz |> List.map preberi_linije |> List.concat
    in
    let (x0, _), (x1, y1) = min_max_koordinate linije in
    let y0' = 0 and y1' = y1 + 2 in
    let x0' = min x0 (500 - y1') and x1' = max x1 (500 + y1') in
    let sirina = x1' - x0' + 1
    and visina = y1' + 1
    and koordinate_skal = linije |> List.map koordinate_linije |> List.concat
    and koordinate_tal = koordinate_linije ((x0', y1'), (x1', y1')) in
    let poligon = prazen_poligon (x0', y0') sirina visina Zrak in
    List.iter (fun k -> nastavi poligon k Skala) koordinate_skal;
    List.iter (fun k -> nastavi poligon k Tla) koordinate_tal;
    poligon

  let niz_poligona poligon =
    let niz_vrstice arr =
      Array.map niz_polnila arr |> Array.to_list |> String.concat ""
    in
    Array.map niz_vrstice poligon.polja |> Array.to_list |> String.concat "\n"

  let naslednji_polozaj dovoli_tla poligon (trenutni_polozaj : koordinata) :
      koordinata option =
    let l_k = trenutni_polozaj +: (-1, 1)
    and s_k = trenutni_polozaj +: (0, 1)
    and d_k = trenutni_polozaj +: (1, 1) in
    let l = pridobi_opt poligon l_k
    and s = pridobi_opt poligon s_k
    and d = pridobi_opt poligon d_k in
    match (l, s, d) with
    | _, Some Zrak, _ -> Some s_k
    | _, None, _ -> None
    | _, Some Tla, _ when not dovoli_tla -> None
    | Some Zrak, _, _ -> Some l_k
    | None, _, _ -> None
    | Some Tla, _, _ when not dovoli_tla -> None
    | _, _, Some Zrak -> Some d_k
    | _, _, None -> None
    | _, _, Some Tla when not dovoli_tla -> None
    | _ -> Some trenutni_polozaj

  let rec simuliraj_padec dovoli_tla poligon (polozaj : koordinata) : unit =
    if pridobi poligon poligon.izvir = Pesek then raise ZapolnjenIzvir;
    let polozaj' = naslednji_polozaj dovoli_tla poligon polozaj in
    match polozaj' with
    | Some p when p = polozaj -> nastavi poligon polozaj Pesek
    | Some p -> simuliraj_padec dovoli_tla poligon p
    | None -> raise PadecVNeskoncnost

  let prestej_padce dovoli_tla poligon : int =
    let nov_poligon = { poligon with polja = Array.(map copy) poligon.polja } in
    let rec aux st_padcev =
      try
        simuliraj_padec dovoli_tla nov_poligon nov_poligon.izvir;
        aux (st_padcev + 1)
      with PadecVNeskoncnost | ZapolnjenIzvir -> st_padcev
    in
    aux 0

  let naloga1 data =
    preberi_poligon data |> prestej_padce false |> string_of_int

  let naloga2 data _part1 =
    preberi_poligon data |> prestej_padce true |> string_of_int
end
