open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type zabojnik = Zabojnik of char

  let niz_zabojnika (Zabojnik z) = String.make 1 z

  type premik = { kolicina : int; s_sklada : int; na_sklad : int }

  type sklad = zabojnik Stack.t

  type ladja = sklad Array.t

  let prazna_ladja stevilo_skladov =
    Array.init stevilo_skladov (fun _ -> Stack.create ())

  let deli_niza n = List.filter (( <> ) "") (String.split_on_char ' ' n)

  let enumerate lst =
    let indeksi = List.init (List.length lst) (fun i -> i + 1) in
    List.combine indeksi lst

  let zabojniki_v_nadstropju niz =
    let deli = List.chunkify 4 (List.list_of_string niz)
    and zabojnik_iz_seznama = function
      | ([ _; c2; _; _ ] | [ _; c2; _ ]) when c2 <> ' ' -> Some (Zabojnik c2)
      | _ -> None
    in
    deli
    |> List.map zabojnik_iz_seznama
    |> enumerate
    |> List.filter (fun (_, z) -> z <> None)
    |> List.map (fun (i, Some z) -> (i, z))

  let dodaj_zabojnik st_sklada zabojnik ladja =
    Stack.push zabojnik ladja.(st_sklada - 1)

  let odstrani_zabojnik st_sklada ladja = Stack.pop ladja.(st_sklada - 1)

  let preberi_ladjo vrstice : ladja =
    let nalozi_zabojnike ladja zabojniki =
      List.iter (fun (i, z) -> dodaj_zabojnik i z ladja) zabojniki
    in
    match List.rev vrstice with
    | [] -> failwith "Premalo vrstic"
    | indeksi :: nadstropja ->
        let stevilo_skladov = List.length (deli_niza indeksi) in
        let ladja = prazna_ladja stevilo_skladov in
        List.iter (nalozi_zabojnike ladja)
          (List.map zabojniki_v_nadstropju nadstropja);
        ladja

  let preberi_premik niz =
    match String.split_on_char ' ' niz with
    | [ _; k; _; z; _; na ] ->
        {
          kolicina = int_of_string k;
          s_sklada = int_of_string z;
          na_sklad = int_of_string na;
        }
    | _ -> failwith ("Napačen format: " ^ niz)

  let premakni_zabojnik z_mesta na_mesto (ladja : ladja) =
    let zabojnik = odstrani_zabojnik z_mesta ladja in
    dodaj_zabojnik na_mesto zabojnik ladja

  let izvedi_premik_obrni premik ladja =
    for _ = 1 to premik.kolicina do
      premakni_zabojnik premik.s_sklada premik.na_sklad ladja
    done

  let obrni_zgornje stevilo st_sklada (ladja : ladja) =
    let pomozna_vrsta = Queue.create () in
    for _ = 1 to stevilo do
      Queue.push (Stack.pop ladja.(st_sklada - 1)) pomozna_vrsta
    done;
    for _ = 1 to stevilo do
      Stack.push (Queue.take pomozna_vrsta) ladja.(st_sklada - 1)
    done

  let izvedi_premik_obdrzi premik ladja =
    for _ = 1 to premik.kolicina do
      premakni_zabojnik premik.s_sklada premik.na_sklad ladja
    done;
    obrni_zgornje premik.kolicina premik.na_sklad ladja

  let vrhnji_zabojniki (ladja : ladja) =
    Array.fold_left
      (fun z sklad -> z ^ niz_zabojnika (Stack.top sklad))
      "" ladja

  (* let premakni_zabojnik z_mesta na_mesto ladja =  *)
  let naloga1 data =
    let ladja, premiki =
      data |> List.lines |> List.split_on (fun l -> l = "")
    in
    let ladja = preberi_ladjo ladja in
    List.iter
      (fun p -> izvedi_premik_obrni p ladja)
      (List.map preberi_premik premiki);
    vrhnji_zabojniki ladja

  let naloga2 data _part1 =
    let ladja, premiki =
      data |> List.lines |> List.split_on (fun l -> l = "")
    in
    let ladja = preberi_ladjo ladja in
    List.iter
      (fun p -> izvedi_premik_obdrzi p ladja)
      (List.map preberi_premik premiki);
    vrhnji_zabojniki ladja
end
