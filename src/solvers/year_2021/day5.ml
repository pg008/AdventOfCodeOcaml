open Solvers.Signature
open Utils.List_utils
module Gu = Utils.General_utils
module IntSet = Set.Make (Int)

module Solver : Solver = struct
  type point = { x : int; y : int }

  type segment = { p1 : point; p2 : point }

  module PointMap = Map.Make (struct
    type t = point

    let compare { x; y } { x = x1; y = y1 } = compare (x, y) (x1, y1)
  end)

  let segment { p1; p2 } =
    if p1.x > p2.x || (p1.x = p2.x && p1.y > p2.y) then { p2 = p1; p1 = p2 }
    else { p1; p2 }

  let parse_line s =
    Scanf.sscanf s "%d,%d -> %d,%d" (fun x1 y1 x2 y2 ->
        segment { p1 = { x = x1; y = y1 }; p2 = { x = x2; y = y2 } })

  let is_full_straight seg = seg.p1.x == seg.p2.x || seg.p1.y == seg.p2.y

  let ( ++ ) p1 (x, y) = { x = p1.x + x; y = p1.y + y }

  let sign n = compare n 0

  let points { p1 = { x = x1; y = y1 } as p1; p2 = { x = x2; y = y2 } } =
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    let mmx = min x1 x2 in
    let mMx = max x1 x2 in
    let mmy = min y1 y2 in
    let mMy = max y1 y2 in
    assert (dx <> 0 || dy <> 0);
    (* Just split cases *)
    if dx = 0 then List.init (mMy - mmy + 1) (fun n -> { x = x1; y = n + mmy })
    else if dy = 0 then
      List.init (mMx - mmx + 1) (fun n -> { x = n + mmx; y = y1 })
    else
      let cx = sign dx in
      let cy = sign dy in
      List.init (dx + 1) (fun nn -> p1 ++ (nn * cx, nn * cy))

  let flat_and_count lines =
    lines |> List.map points |> List.flatten
    |> List.fold_left
         (fun s k ->
           PointMap.update k
             (fun x -> match x with None -> Some 1 | Some x -> Some (x + 1))
             s)
         PointMap.empty
    |> PointMap.bindings
    |> List.filter (fun (_, n) -> n > 1)
    |> List.length |> string_of_int

  let naloga1 (data : string) =
    let lines =
      data |> List.lines |> List.map parse_line |> List.filter is_full_straight
    in
    lines |> flat_and_count

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.map parse_line in
    lines |> flat_and_count
end
