open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type opica = {
    stvari : int Queue.t;
    operacija : int -> int -> int;
    test : int -> int;
    delitelj : int;
    mutable stevilo_pregledov : int;
  }

  type opice = opica Array.t * int

  let preberi_zacetne_stvari niz =
    let q = Queue.create () in
    ( match niz |> String.trim |> String.split_on_char ':' with
    | [ "Starting items"; stvari ] ->
        stvari |> String.split_on_char ',' |> List.map String.trim
        |> List.map int_of_string
        |> List.iter (fun e -> Queue.add e q)
    | _ -> failwith ("Ne razumem stvari: " ^ niz) );
    q

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  let lcm a b = a * b / gcd a b

  let rec list_lcm = function [] -> 1 | x :: xs -> lcm x (list_lcm xs)

  let preberi_operacijo niz =
    match niz |> String.trim |> String.split_on_char ' ' with
    | [ "Operation:"; "new"; "="; "old"; operator; stevilo ] -> (
        let operator =
          match operator with "*" -> ( * ) | "+" -> ( + ) | _ -> assert false
        in
        match stevilo with
        | "old" -> fun x del -> operator (x mod del) (x mod del)
        | st -> fun x del -> operator (int_of_string st mod del) (x mod del) )
    | _ -> failwith ("Ne razumem operacije: " ^ niz)

  let zadnje_stevilo niz =
    niz |> String.trim |> String.split_on_char ' ' |> List.rev |> List.hd
    |> int_of_string

  let preberi_pogoj = zadnje_stevilo

  let preberi_test pogoj resnicno neresnicno =
    let delitelj = preberi_pogoj pogoj
    and opica1 = zadnje_stevilo resnicno
    and opica2 = zadnje_stevilo neresnicno in
    fun i -> if i mod delitelj = 0 then opica1 else opica2

  let preberi_opico = function
    | [ _; z_stvari; operacija; testni_pogoj; resnicno; neresnicno ] ->
        {
          stvari = preberi_zacetne_stvari z_stvari;
          operacija = preberi_operacijo operacija;
          test = preberi_test testni_pogoj resnicno neresnicno;
          delitelj = preberi_pogoj testni_pogoj;
          stevilo_pregledov = 0;
        }
    | _ -> failwith "Ne razumem opice"

  let preberi_opice niz =
    let seznam =
      niz |> List.lines |> List.group_list |> List.map List.rev
      |> List.map preberi_opico
    in
    let skupni_delitelj =
      seznam |> List.map (fun o -> o.delitelj) |> list_lcm
    in
    (Array.of_list seznam, skupni_delitelj)

  let naredi_potezo ((opice, delitelj) : opice) (opica : opica) =
    opica.stvari
    |> Queue.iter (fun stvar ->
           let nova_vrednost =
             opica.operacija stvar delitelj / (3 mod delitelj)
           in
           Queue.add nova_vrednost opice.(opica.test nova_vrednost).stvari;
           opica.stevilo_pregledov <- succ opica.stevilo_pregledov);
    Queue.clear opica.stvari

  let naredi_potezo2 ((opice, delitelj) : opice) (opica : opica) =
    opica.stvari
    |> Queue.iter (fun stvar ->
           let nova_vrednost = opica.operacija stvar delitelj in
           Queue.add nova_vrednost opice.(opica.test nova_vrednost).stvari;
           opica.stevilo_pregledov <- succ opica.stevilo_pregledov);
    Queue.clear opica.stvari

  let niz_opice opica =
    opica.stvari |> Queue.to_seq |> List.of_seq |> List.map string_of_int
    |> String.concat ", "

  let izpisi_opice opice =
    Array.iteri
      (fun i o ->
        print_endline
          ( "opica " ^ string_of_int i ^ ": " ^ niz_opice o ^ ";; "
          ^ string_of_int o.stevilo_pregledov ))
      (fst opice)

  let naredi_rundo (opice : opice) poteza =
    Array.iter (poteza opice) (fst opice)

  let zmnozek_pregledov opice =
    let pregledi =
      Array.map (fun o -> o.stevilo_pregledov) (fst opice) |> Array.to_list
    in
    let urejeni = pregledi |> List.sort compare |> List.rev in
    List.nth urejeni 0 * List.nth urejeni 1

  let naloga1 data =
    let opice = preberi_opice data in
    for _ = 1 to 20 do
      naredi_rundo opice naredi_potezo
    done;
    zmnozek_pregledov opice |> string_of_int

  let naloga2 data _part1 =
    let opice = preberi_opice data in
    for _ = 1 to 10000 do
      naredi_rundo opice naredi_potezo2
    done;
    izpisi_opice opice;
    zmnozek_pregledov opice |> string_of_int
end
