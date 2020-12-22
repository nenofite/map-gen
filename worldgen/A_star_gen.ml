open Core_kernel

module type Score = sig
  type t

  val ( + ) : t -> t -> t

  val ( <= ) : t -> t -> bool
end

module Make (Score : Score) (Coord : Comparable.S) = struct
  module Pq = Priority_queue.Make (Score)

  type node =
    {coord: Coord.t; came_from: Coord.t; g_score: Score.t; f_score: Score.t}

  let reconstruct_path ~goal_coord ~closed_set =
    let rec go coord coords =
      let node = Coord.Map.find_exn closed_set coord in
      if Coord.(node.coord = node.came_from) then coord :: coords
      else go node.came_from (coord :: coords)
    in
    go goal_coord []

  let pathfind ~neighbors ~heuristic ~start_set ~goal ~max_iters =
    let rec go ~closed_set ~open_set ~remaining_iters =
      if remaining_iters <= 0 then (
        Tale.logf "Exhausted all %d iterations" max_iters ;
        None )
      else
        match Pq.extract open_set with
        | None, _ ->
            Tale.log "Exhausted open set" ;
            None
        | Some open_node, _open_set when goal open_node.coord ->
            Tale.logf "Found path after %d iters"
              (max_iters - remaining_iters + 1) ;
            let closed_set =
              Coord.Map.add_exn ~key:open_node.coord ~data:open_node closed_set
            in
            Some (reconstruct_path ~goal_coord:open_node.coord ~closed_set)
        | Some open_node, open_set -> (
          match
            Coord.Map.add ~key:open_node.coord ~data:open_node closed_set
          with
          | `Duplicate ->
              go ~open_set ~closed_set ~remaining_iters:(remaining_iters - 1)
          | `Ok closed_set ->
              let open_set =
                List.fold (neighbors open_node.coord) ~init:open_set
                  ~f:(fun acc (neighbor_coord, neighbor_cost) ->
                    if Coord.Map.mem closed_set neighbor_coord then acc
                    else
                      let node =
                        { coord= neighbor_coord
                        ; came_from= open_node.coord
                        ; g_score= Score.(open_node.g_score + neighbor_cost)
                        ; f_score= heuristic neighbor_coord }
                      in
                      let priority = Score.(node.g_score + node.f_score) in
                      Pq.insert acc priority node)
              in
              go ~open_set ~closed_set ~remaining_iters:(remaining_iters - 1) )
    in
    let closed_set = Coord.Map.empty in
    let open_set =
      List.fold start_set ~init:Pq.empty ~f:(fun acc (start_coord, g_score) ->
          let f_score = heuristic start_coord in
          let node =
            {coord= start_coord; came_from= start_coord; g_score; f_score}
          in
          let priority = Score.(node.g_score + node.f_score) in
          Pq.insert acc priority node)
    in
    go ~closed_set ~open_set ~remaining_iters:max_iters
end
