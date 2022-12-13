open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type podatek = Stevilo of int | Seznam of podatek list

  type primerjalni_par = podatek * podatek

  let dodaj_stevko stevilo znak =
    string_of_int stevilo ^ String.make 1 znak |> int_of_string

  let preberi_paket niz =
    let odprti_podatki = Stack.create () in
    let () = Stack.push (Seznam []) odprti_podatki in
    let odpri_seznam s = Stack.push (Seznam []) s in
    let dodaj_podatek e s =
      match Stack.pop s with
      | Seznam sez -> Stack.push (Seznam (sez @ [ e ])) s
      | _ -> failwith "Ne morem dodati, če nimam seznama!"
    in
    let zapri_seznam s : unit =
      match Stack.pop s with
      | Stevilo _ -> failwith "Najprej moraš zapreti število"
      | Seznam _ as sez -> dodaj_podatek sez s
    in
    let zapri_stevilo s =
      match Stack.pop s with
      | Stevilo n -> dodaj_podatek (Stevilo n) s
      | Seznam xs -> Stack.push (Seznam xs) s
    in
    String.iter
      (function
        | '[' -> odpri_seznam odprti_podatki
        | ']' ->
            zapri_stevilo odprti_podatki;
            zapri_seznam odprti_podatki
        | '0' .. '9' as d -> (
            match Stack.pop odprti_podatki with
            | Stevilo n ->
                Stack.push (Stevilo (dodaj_stevko n d)) odprti_podatki
            | Seznam xs ->
                Stack.push (Seznam xs) odprti_podatki;
                Stack.push (Stevilo (dodaj_stevko 0 d)) odprti_podatki )
        | ',' -> zapri_stevilo odprti_podatki
        | _ -> failwith "Nepričakovan znak")
      niz;
    match Stack.top odprti_podatki with
    | Seznam s -> List.hd s
    | _ -> failwith "Nekaj je šlo narobe"

  let preberi_par = function
    | [ p1; p2 ] -> (preberi_paket p1, preberi_paket p2)
    | _ -> failwith "par mora vsebovati dve vrstici"

  let preberi_vhodne_podatke (niz : string) : primerjalni_par list =
    niz |> List.lines |> List.group_list |> List.map List.rev
    |> List.map preberi_par

  let rec primerjaj (p1, p2) =
    match (p1, p2) with
    | Stevilo s1, Stevilo s2 ->
        if s1 < s2 then Some true else if s1 > s2 then Some false else None
    | Seznam (h1 :: t1), Seznam (h2 :: t2) -> (
        match primerjaj (h1, h2) with
        | Some p -> Some p
        | None -> primerjaj (Seznam t1, Seznam t2) )
    | Seznam [], Seznam (_ :: _) -> Some true
    | Seznam (_ :: _), Seznam [] -> Some false
    | Seznam [], Seznam [] -> None
    | Stevilo s1, (Seznam _ as s) -> primerjaj (Seznam [ Stevilo s1 ], s)
    | (Seznam _ as s), Stevilo s1 -> primerjaj (s, Seznam [ Stevilo s1 ])

  let primerjaj' p1 p2 =
    match primerjaj (p1, p2) with None -> 0 | Some p -> if p then -1 else 1

  let naloga1 data =
    preberi_vhodne_podatke data
    |> List.mapi (fun i p -> if Option.get (primerjaj p) then i + 1 else 0)
    |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let m1 = Seznam [ Seznam [ Stevilo 2 ] ]
    and m2 = Seznam [ Seznam [ Stevilo 6 ] ] in
    let vrstice =
      data |> List.lines |> List.filter (( <> ) "") |> List.map preberi_paket
    in
    let vrstice' = m1 :: m2 :: vrstice in
    vrstice' |> List.sort primerjaj'
    |> List.mapi (fun i o -> if o = m1 || o = m2 then i + 1 else 1)
    |> List.fold_left ( * ) 1 |> string_of_int
end
