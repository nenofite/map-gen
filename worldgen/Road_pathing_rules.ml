open Core_kernel

module Coord = struct
  module T = struct
    type direction = Ns | Ew [@@deriving eq, ord, hash, sexp, bin_io]

    type structure = Road | Bridge of direction
    [@@deriving eq, ord, hash, sexp, bin_io]

    type t = {x: int; y: int; z: int; structure: structure}
    [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let make_road ~x ~y ~z = {x; y; z; structure= Road}
end

include Coord.T

(* Costs *)
let flat_ground_cost = 10

let slope_ground_cost = 20

let bridge_cost = 100

let get_obstacle_in_margin ~x ~z =
  let margin = 3 in
  let obstacles = (Overlay.Canon.require ()).obstacles in
  Range.fold (z - margin) (z + margin) Overlay.Canon.Clear (fun obs z ->
      Range.fold (x - margin) (x + margin) obs (fun obs x ->
          let here_obs = Grid.get x z obstacles in
          Overlay.Canon.Obstacle.max obs here_obs ) )

let get_elevation ~x ~z =
  let elevation = (Overlay.Canon.require ()).elevation in
  Grid.get x z elevation

let two_neighbors_of_direction = function
  | Ns ->
      [(0, -1); (0, 1)]
  | Ew ->
      [(-1, 0); (1, 0)]

let make_direction_exn x1 z1 x2 z2 =
  if x1 = x2 && z1 <> z2 then Ns
  else if x1 <> x2 && z1 = z2 then Ew
  else invalid_argf "cannot make direction: (%d, %d) (%d, %d)" x1 z1 x2 z2 ()

let neighbors {x; y; z; structure} =
  let add_ground_neighbor ~y ~nx ~ny ~nz list =
    if abs (y - ny) <= 1 then
      ({x= nx; y= ny; z= nz; structure= Road}, flat_ground_cost) :: list
    else list
  in
  let add_bridge_start_neighbor ~x ~y ~z ~nx ~ny ~nz list =
    if ny <= y then
      ( {x= nx; y= ny; z= nz; structure= Bridge (make_direction_exn x z nx nz)}
      , bridge_cost )
      :: list
    else list
  in
  let ground_neighbors ~x ~y ~z =
    List.fold Grid.Griddable.Helpers.four_directions ~init:[]
      ~f:(fun acc (dx, dz) ->
        let nx = x + dx in
        let nz = z + dz in
        match get_obstacle_in_margin ~x:nx ~z:nz with
        | Overlay.Canon.Impassable ->
            acc
        | Bridgeable ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_start_neighbor ~y ~ny ~x ~z ~nx ~nz acc
        | Clear ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_ground_neighbor ~y ~ny ~nx ~nz acc )
  in
  let add_bridge_continue_neighbor bridge_dir ~y ~ny ~nx ~nz list =
    if ny <= y then
      ({x= nx; y; z= nz; structure= Bridge bridge_dir}, bridge_cost) :: list
    else list
  in
  let add_bridge_end_neighbor _bridge ~y ~ny ~nx ~nz list =
    if ny = y then
      ({x= nx; y; z= nz; structure= Road}, flat_ground_cost) :: list
    else list
  in
  let bridge_neighbors ~x ~y ~z bridge_dir =
    List.fold (two_neighbors_of_direction bridge_dir) ~init:[]
      ~f:(fun acc (dx, dz) ->
        let nx = x + dx in
        let nz = z + dz in
        match get_obstacle_in_margin ~x:nx ~z:nz with
        | Overlay.Canon.Impassable ->
            acc
        | Bridgeable ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_continue_neighbor bridge_dir ~y ~ny ~nx ~nz acc
        | Clear ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_continue_neighbor bridge_dir ~y ~ny ~nx ~nz
            @@ add_bridge_end_neighbor bridge_dir ~y ~ny ~nx ~nz acc )
  in
  match structure with
  | Road ->
      ground_neighbors ~x ~y ~z
  | Bridge dir ->
      bridge_neighbors ~x ~y ~z dir
