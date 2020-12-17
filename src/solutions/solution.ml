open Solvers
open Solvers.Signature

let choose_solver : string -> (module Solver) = function
  | "0" -> (module Day0.Solver)
  | "1" -> (module Day1.Solver)
  | "2" -> (module Day2.Solver)
  | "3" -> (module Day3.Solver)
  | "4" -> (module Day4.Solver)
  | "5" -> (module Day5.Solver)
  | "6" -> (module Day6.Solver)
  | "7" -> (module Day7.Solver)
  | "8" -> (module Day8.Solver)
  | "9" -> (module Day9.Solver)
  | "10" -> (module Day10.Solver)
  | "11" -> (module Day11.Solver)
  | "12" -> (module Day12.Solver)
  | "13" -> (module Day13.Solver)
  | "14" -> (module Day14.Solver)
  | "15" -> (module Day15.Solver)
  | "16" -> (module Day16.Solver)
  | "17" -> (module Day17.Solver)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = Utils.Files.preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  Utils.Files.izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  Utils.Files.izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
