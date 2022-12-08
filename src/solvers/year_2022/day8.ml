open Solvers.Signature
open Utils.List_utils

(*

  |------------> y
  | 0 1 2 3
  | 1
  v 2
    3

  x
  
  *)

module Solver : Solver = struct
  type smer = Gor | Dol | Levo | Desno

  type indeks = { x : int; y : int }

  (*visina razdalja*)
  type razgled = (int * int) list

  let rec posodobi_razgled nova_visina (razgled : razgled) : razgled =
    match razgled with
    | d :: _ when nova_visina > fst d -> [ (nova_visina, 1) ]
    | [] -> [ (nova_visina, 1) ]
    | (v, r) :: ds -> (v, r + 1) :: posodobi_razgled nova_visina ds

  type drevo = {
    visina : int;
    razdalje_do_roba : (smer * int) list;
    max_sosednje_visine : (smer * int) list;
    razgledi : (smer * razgled) list;
  }

  type mreza = drevo Array.t Array.t

  let nov_indeks smer indeks =
    match smer with
    | Gor -> { indeks with x = indeks.x - 1 }
    | Dol -> { indeks with x = indeks.x + 1 }
    | Levo -> { indeks with y = indeks.y - 1 }
    | Desno -> { indeks with y = indeks.y + 1 }

  let pridobi_element indeks (mreza : mreza) =
    try Some mreza.(indeks.x).(indeks.y) with Invalid_argument _ -> None

  let nastavi_element indeks (mreza : mreza) element =
    try mreza.(indeks.x).(indeks.y) <- element
    with Invalid_argument _ -> failwith "Indeks izven meja"

  let nastavi_max_v_smeri smer vrednost element =
    let rec aux = function
      | (s, velikost) :: ss ->
          if s = smer then (s, max vrednost velikost) :: ss
          else (s, velikost) :: aux ss
      | [] -> failwith "Ne najdem smeri"
    in
    { element with max_sosednje_visine = aux element.max_sosednje_visine }

  let nastavi_razgled_v_smeri smer razgled element =
    let rec aux = function
      | ((s, _) as r) :: ss ->
          if s = smer then (s, razgled) :: ss else r :: aux ss
      | [] -> failwith "Ne najdem smeri"
    in
    { element with razgledi = aux element.razgledi }

  let razgled_v_smeri smer element =
    let rec aux = function
      | (s, r) :: ss -> if s = smer then r else aux ss
      | [] -> failwith "Ne najdem smeri"
    in
    aux element.razgledi

  let razdalja_do_roba smer element =
    let rec aux = function
      | (s, r) :: ss -> if s = smer then r else aux ss
      | [] -> failwith "Ne najdem smeri"
    in
    aux element.razdalje_do_roba

  let popravi_element s max razgled e =
    nastavi_max_v_smeri s max e |> nastavi_razgled_v_smeri s razgled

  let pridobi_max_v_smeri (smer : smer) element =
    let rec aux = function
      | (s, velikost) :: _ when s = smer -> max element.visina velikost
      | _ :: ss -> aux ss
      | [] -> failwith "Ne najdem smeri"
    in
    aux element.max_sosednje_visine

  let nastavi_v_smeri (indeks : indeks) (smer : smer) (mreza : mreza) =
    let sosednji_indeks = nov_indeks smer indeks in
    match
      (pridobi_element indeks mreza, pridobi_element sosednji_indeks mreza)
    with
    | None, _ -> failwith "Indeks izven meja"
    | _, None -> ()
    | Some trenutno_drevo, Some sosednje_drevo ->
        let max_vrednost = pridobi_max_v_smeri smer sosednje_drevo
        and nov_razgled =
          posodobi_razgled sosednje_drevo.visina
            (razgled_v_smeri smer sosednje_drevo)
        in
        nastavi_element indeks mreza
          (popravi_element smer max_vrednost nov_razgled trenutno_drevo)

  let nastavi_najvecje (indeks : indeks) (mreza : mreza) =
    nastavi_v_smeri indeks Gor mreza;
    nastavi_v_smeri indeks Dol mreza;
    nastavi_v_smeri indeks Levo mreza;
    nastavi_v_smeri indeks Desno mreza

  let iter_down_right f (mreza : mreza) =
    for x = 0 to Array.length mreza - 1 do
      for y = 0 to Array.length mreza.(0) - 1 do
        let i = { x; y } in
        f i mreza
      done
    done

  let iter_up_left f (mreza : mreza) =
    for x = Array.length mreza - 1 downto 0 do
      for y = Array.length mreza.(0) - 1 downto 0 do
        let i = { x; y } in
        f i mreza
      done
    done

  let nastavi_razdalje_do_robov indeks mreza =
    let sirina = Array.length mreza.(0) in
    let visina = Array.length mreza in
    let razdalje =
      [
        (Levo, indeks.y);
        (Desno, sirina - indeks.y - 1);
        (Gor, indeks.x);
        (Dol, visina - indeks.x - 1);
      ]
    in
    match pridobi_element indeks mreza with
    | None -> ()
    | Some d ->
        nastavi_element indeks mreza { d with razdalje_do_roba = razdalje }

  let drevo_of_niz znak =
    {
      visina = int_of_string (String.make 1 znak);
      max_sosednje_visine = [ (Levo, -1); (Desno, -1); (Gor, -1); (Dol, -1) ];
      razgledi = [ (Levo, []); (Desno, []); (Gor, []); (Dol, []) ];
      razdalje_do_roba = [];
    }

  let dolzina_razgleda (razgled : razgled) visina razdalja_do_roba =
    let rec aux = function
      | (v, r) :: _ when v >= visina -> r
      | [ (_, _) ] -> razdalja_do_roba
      | _ :: ss -> aux ss
      | [] -> 0
    in
    aux (List.rev razgled)

  let ocena_razgleda drevo =
    List.fold_left ( * ) 1
      (List.map
         (fun (s, r) ->
           dolzina_razgleda r drevo.visina (razdalja_do_roba s drevo))
         drevo.razgledi)

  let preberi_vrstico vrstica =
    vrstica |> List.list_of_string |> List.map drevo_of_niz |> Array.of_list

  let preberi_mrezo vrstice : mreza =
    let mreza = Array.of_list (List.map preberi_vrstico vrstice) in
    iter_down_right nastavi_najvecje mreza;
    iter_up_left nastavi_najvecje mreza;
    iter_down_right nastavi_razdalje_do_robov mreza;
    mreza

  let je_vidno drevo =
    let rec aux = function
      | [] -> false
      | (_, visina) :: smeri ->
          if visina < drevo.visina then true else aux smeri
    in
    aux drevo.max_sosednje_visine

  let count f (mreza : mreza) =
    Array.fold_left ( + ) 0
      (Array.map
         (Array.fold_left (fun s x -> if f x then s + 1 else s) 0)
         mreza)

  let ocene_razgledov (mreza : mreza) =
    Array.map (Array.map ocena_razgleda) mreza

  let max_in_array = Array.fold_left max 0

  let max_in_2d_array mreza = Array.map max_in_array mreza |> max_in_array

  let naloga1 data =
    let mreza = data |> List.lines |> preberi_mrezo in
    count je_vidno mreza |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> preberi_mrezo |> ocene_razgledov |> max_in_2d_array
    |> string_of_int
end
