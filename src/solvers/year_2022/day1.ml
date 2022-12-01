open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let maksimalni locilo l =
    let rec aux najvecji zadnji = function
      | [] -> najvecji
      | x :: xs ->
          if x = locilo then aux (max zadnji najvecji) 0 xs
          else aux najvecji (int_of_string x + zadnji) xs
    in
    aux 0 0 l

  let maks3 locilo l =
    let vstavi_v_trojico (n1, n2, n3) x =
      if x > n1 then (x, n1, n2)
      else if x > n2 then (n1, x, n2)
      else if x > n3 then (n1, n2, x)
      else (n1, n2, n3)
    in
    let rec aux (n1, n2, n3) trenutni = function
      | [] -> vstavi_v_trojico (n1, n2, n3) trenutni
      | x :: xs ->
          if x = locilo then aux (vstavi_v_trojico (n1, n2, n3) trenutni) 0 xs
          else
            let x1 = int_of_string x in
            aux (n1, n2, n3) (x1 + trenutni) xs
    in
    let n1, n2, n3 = aux (0, 0, 0) 0 l in
    n1 + n2 + n3

  let naloga1 data =
    let lines = List.lines data in
    string_of_int (maksimalni "" lines)

  let naloga2 data _part1 =
    let lines = List.lines data in
    string_of_int (maks3 "" lines)
end
