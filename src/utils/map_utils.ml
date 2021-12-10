module MMap = Map

module Map = struct
  module type S = sig
    include MMap.S

    val of_bindings : (key * 'a) list -> 'a t

    val union_overwrite : 'a t -> 'a t -> 'a t

    val keys : 'a t -> key list

    val values : 'a t -> 'a list
  end

  module Make (Ord : MMap.OrderedType) : S with type key = Ord.t = struct
    include MMap.Make (Ord)

    let of_bindings list =
      List.fold_left (fun map (key, v) -> add key v map) empty list

    let union_overwrite m1 m2 = union (fun _ v1 _ -> Some v1) m1 m2

    let keys m = List.map fst (bindings m)

    let values m = List.map snd (bindings m)
  end
end

module Counter = struct
  module type S = sig
    include Map.S

    val plus : key -> int -> int t -> int t

    val most_common : int t -> (key * int) list
  end

  module Make (Ord : MMap.OrderedType) : S with type key = Ord.t = struct
    include Map.Make (Ord)

    let plus k v m =
      update k (function None -> Some v | Some v' -> Some (v + v')) m

    let most_common m =
      m |> bindings |> List.sort (fun (_, v1) (_, v2) -> Stdlib.compare v2 v1)
  end
end
