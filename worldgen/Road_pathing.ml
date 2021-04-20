open! Core_kernel
module Pq = Priority_queue.Int

module Path_coord = struct
  type structure = Road | Bridge_ns | Bridge_ew
  [@@deriving eq, ord, hash, sexp, bin_io]

  module T = struct
    type t = {x: int; y: int; z: int; structure: structure}
    [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

type closest_path = {cost: int; parent: Path_coord.t option}

let root_path = {cost= 0; parent= None}

type pathing_state =
  {paths: Path_coord.Hash_set.t; closest_paths: closest_path Path_coord.Table.t}

let init_state () =
  { paths= Path_coord.Hash_set.create ()
  ; closest_paths= Path_coord.Table.create () }

let update_closest_paths ~edges ~new_paths state =
  let rec go open_set ~remaining_iters =
    if remaining_iters <= 0 then ()
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
          go open_set ~remaining_iters:(remaining_iters - 1)
      | None, _ ->
          ()
  in
  List.iter new_paths ~f:(fun p ->
      Hash_set.strict_add_exn state.paths p ;
      Path_coord.Table.set state.closest_paths ~key:p ~data:root_path ) ;
  let open_set =
    List.fold new_paths ~init:Pq.empty ~f:(fun pq coord ->
        Pq.insert pq 0 coord )
  in
  go open_set ~remaining_iters:10_000_000 ;
  ()
