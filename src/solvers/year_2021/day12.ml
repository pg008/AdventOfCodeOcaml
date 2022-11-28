open Solvers.Signature
open Utils.List_utils
open Utils.Map_utils

module Solver : Solver = struct
  type vertex = Vertex of { name : string; reentrant : bool }

  let vertex name =
    Vertex { name; reentrant = name = String.capitalize_ascii name }

  let cmp (Vertex { name = name1; _ }) (Vertex { name = name2; _ }) =
    String.compare name1 name2

  let is_reentrant (Vertex v) = v.reentrant

  module Graph = Map.Make (struct
    type t = vertex

    let compare = cmp
  end)

  module VSet = Set.Make (struct
    type t = vertex

    let compare = cmp
  end)

  let start_v = vertex "start"

  let end_v = vertex "end"

  let with_default v1 v2 g =
    Graph.update v1
      (function
        | None -> Some (VSet.singleton v2) | Some s -> Some (s |> VSet.add v2))
      g

  let parse data =
    data |> List.lines
    |> List.fold_left
         (fun g line ->
           let[@warning "-8"] [ v1; v2 ] = String.split_on_char '-' line in
           g
           |> with_default (vertex v1) (vertex v2)
           |> with_default (vertex v2) (vertex v1))
         Graph.empty

  let solve graph start stop =
    let memo_dict = Hashtbl.create 200 in
    (* (vertex, [taken_non_reentrable]) *)
    let rec explore root taken =
      match Hashtbl.find_opt memo_dict (root, taken) with
      | Some r -> r
      | None ->
          let result =
            let children = Graph.find root graph in
            VSet.fold
              (fun child s ->
                s
                +
                if child = stop then 1
                else if is_reentrant child then explore child taken
                else if VSet.mem child taken then 0
                else explore child (VSet.add child taken))
              children 0
          in
          Hashtbl.replace memo_dict (root, taken) result;
          result
    in
    explore start (VSet.singleton start)

  let solve2 graph start stop =
    let memo_dict = Hashtbl.create 200 in
    (* (vertex, [taken_non_reentrable]) *)
    let rec explore root taken double =
      match Hashtbl.find_opt memo_dict (root, taken, double) with
      | Some r -> r
      | None ->
          let result =
            let children = Graph.find root graph in
            VSet.fold
              (fun child s ->
                s
                +
                if child = stop then 1
                else if is_reentrant child then explore child taken double
                else if VSet.mem child taken then
                  if double || child = start then 0
                  else explore child (VSet.add child taken) true
                else explore child (VSet.add child taken) double)
              children 0
          in
          Hashtbl.replace memo_dict (root, taken, double) result;
          result
    in
    explore start (VSet.singleton start) false

  let naloga1 (data : string) =
    let graph = data |> parse in
    solve graph start_v end_v |> string_of_int

  let naloga2 data _part1 =
    let graph = data |> parse in
    solve2 graph start_v end_v |> string_of_int
end
