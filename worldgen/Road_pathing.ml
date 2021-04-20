open! Core_kernel
module Pq = Priority_queue.Int
module Path_coord = Road_pathing_rules.Coord
(* module Path_coord = struct
  type structure = Road | Bridge_ns | Bridge_ew
  [@@deriving eq, ord, hash, sexp, bin_io]

  module T = struct
    type t = {x: int; y: int; z: int; structure: structure}
    [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end *)

type closest_path = {cost: int; parent: Path_coord.t option}

let root_path = {cost= 0; parent= None}

type pathing_state =
  {paths: Path_coord.Hash_set.t; closest_paths: closest_path Path_coord.Table.t}

let init_state () =
  { paths= Path_coord.Hash_set.create ()
  ; closest_paths= Path_coord.Table.create () }

let get_paths state = Path_coord.Set.of_hash_set state.paths

let update_closest_paths ~edges ~new_paths state =
  let allowed_iters = 10_000_000 in
  let rec go open_set ~elapsed_iters =
    if elapsed_iters > allowed_iters then (
      Tale.logf "Exhausted all %d iters" elapsed_iters ;
      () )
    else
      match Pq.extract_with_priority open_set with
      | Some (here_cost, here), open_set ->
          let open_set =
            List.fold (edges here) ~init:open_set
              ~f:(fun open_set (neighbor, edge_cost) ->
                let neighbor_cost = here_cost + edge_cost in
                let should_add =
                  match Path_coord.Table.find state.closest_paths neighbor with
                  | Some {cost= old_cost; parent= _} ->
                      neighbor_cost < old_cost
                  | None ->
                      true
                in
                if should_add then (
                  Path_coord.Table.set state.closest_paths ~key:neighbor
                    ~data:{cost= neighbor_cost; parent= Some here} ;
                  Pq.insert open_set neighbor_cost neighbor )
                else open_set )
          in
          go open_set ~elapsed_iters:(elapsed_iters + 1)
      | None, _ ->
          Tale.logf "Finished after %d of %d iters" elapsed_iters allowed_iters ;
          ()
  in
  List.iter new_paths ~f:(fun p ->
      Hash_set.add state.paths p ;
      Path_coord.Table.set state.closest_paths ~key:p ~data:root_path ) ;
  let open_set =
    List.fold new_paths ~init:Pq.empty ~f:(fun pq coord ->
        Pq.insert pq 0 coord )
  in
  go open_set ~elapsed_iters:0 ;
  ()

let reconstruct_path start ~state =
  let rec go ls point =
    let ls = point :: ls in
    let cp = Path_coord.Table.find_exn state.closest_paths point in
    match cp.parent with Some parent -> go ls parent | None -> List.rev ls
  in
  go [] start

let get_closest_path ~from_paths state =
  List.filter_map from_paths ~f:(fun p ->
      Path_coord.Table.find state.closest_paths p )
  |> List.max_elt ~compare:(fun a b -> Int.compare a.cost b.cost)
  |> Option.map ~f:(fun cp ->
         reconstruct_path (Option.value_exn cp.parent) ~state )

let enroad ~town_roads state =
  let canon = Overlay.Canon.require () in
  let town_paths =
    List.map town_roads ~f:(fun (x, z) ->
        let y = Grid.get x z canon.elevation in
        Path_coord.{x; y; z; bridge= No_bridge} )
  in
  let new_path =
    match get_closest_path state ~from_paths:town_paths with
    | Some path ->
        Tale.log "Found path" ; path
    | None ->
        Tale.log "No path" ; []
  in
  let get_obstacle ~x ~z = Grid.get x z canon.obstacles in
  let get_elevation ~x ~z = Grid.get x z canon.elevation in
  let edges = Road_pathing_rules.neighbors ~get_obstacle ~get_elevation in
  update_closest_paths ~edges ~new_paths:(new_path @ town_paths) state ;
  ()
