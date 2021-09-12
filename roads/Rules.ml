open! Core_kernel

module Coord = struct
  module T = struct
    type dir2 = Ns | Ew [@@deriving eq, ord, hash, sexp, bin_io]

    (** the direction you must walk to go up the stairs *)
    type dir4 = N | E | S | W [@@deriving eq, ord, hash, sexp, bin_io]

    type structure = Road | Stair of dir4 | Bridge of dir4
    [@@deriving eq, ord, hash, sexp, bin_io]

    type t = {x: int; y: int; z: int; structure: structure}
    [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make_binable (T)
  include Hashable.Make (T)

  let make_road ~x ~y ~z = {x; y; z; structure= Road}

  let translate ~dx ~dz {x; y; z; structure} =
    {x= x + dx; y; z= z + dz; structure}
end

include Coord.T

type bridge = {x: int; y: int; z: int; direction: dir4; length: int}
[@@deriving eq, bin_io]

(* Costs *)
let flat_ground_cost = 10

let start_stair_cost = 100

let continue_stair_cost = 10

let bridge_cost = 100

let add_margin get_obstacle ~x ~z =
  let margin = 2 in
  Mg_util.Range.fold (z - margin) (z + margin) Overlay.Canon.Clear (fun obs z ->
      Mg_util.Range.fold (x - margin) (x + margin) obs (fun obs x ->
          let here_obs = get_obstacle ~x ~z in
          Overlay.Canon.Obstacle.max obs here_obs ) )

let get_canon_obstacle ~x ~z =
  let canon = Overlay.Canon.require () in
  Grid.get x z canon.obstacles

let get_canon_elevation ~x ~z =
  let elevation = (Overlay.Canon.require ()).elevation in
  Grid.get x z elevation

let two_neighbors_of_dir2 = function
  | Ns ->
      [(0, -1); (0, 1)]
  | Ew ->
      [(-1, 0); (1, 0)]

let neighbor_of_dir4 = function
  | N ->
      (0, -1)
  | E ->
      (1, 0)
  | S ->
      (0, 1)
  | W ->
      (-1, 0)

let two_neighbors_of_dir4 = function
  | N ->
      [(0, 1, -1); (0, 0, 1)]
  | E ->
      [(-1, 0, 0); (1, 1, 0)]
  | S ->
      [(0, 0, -1); (0, 1, 1)]
  | W ->
      [(-1, 1, 0); (1, 0, 0)]

let opposite_of_dir4 = function N -> S | E -> W | S -> N | W -> E

let make_dir2_exn x1 z1 x2 z2 =
  if x1 = x2 && z1 <> z2 then Ns
  else if x1 <> x2 && z1 = z2 then Ew
  else invalid_argf "cannot make dir2: (%d, %d) (%d, %d)" x1 z1 x2 z2 ()

let make_dir4_exn x1 z1 x2 z2 =
  match (x2 - x1, z2 - z1) with
  | 0, -1 ->
      N
  | 1, 0 ->
      E
  | 0, 1 ->
      S
  | -1, 0 ->
      W
  | _ ->
      invalid_argf "cannot make dir4: (%d, %d) (%d, %d)" x1 z1 x2 z2 ()

let neighbors ~get_elevation ~get_obstacle {x; y; z; structure} =
  let get_obstacle_in_margin = add_margin get_obstacle in
  let add_ground_neighbor ~from_stair ~y ~nx ~ny ~nz list =
    let stair_cost =
      if from_stair then continue_stair_cost else start_stair_cost
    in
    let dy = ny - y in
    if dy = 0 then
      ({x= nx; y= ny; z= nz; structure= Road}, flat_ground_cost) :: list
    else if dy = 1 then
      (* stairs should only generate on the lower block of a slope, so
         forcefully lower the y *)
      ( {x= nx; y= ny - 1; z= nz; structure= Stair (make_dir4_exn x z nx nz)}
      , stair_cost )
      :: list
    else if dy = -1 then
      ( { x= nx
        ; y= ny
        ; z= nz
        ; structure= Stair (opposite_of_dir4 (make_dir4_exn x z nx nz)) }
      , stair_cost )
      :: list
    else list
  in
  let add_bridge_start_neighbor ~x ~y ~z ~nx ~ny ~nz list =
    if ny <= y then
      ( {x= nx; y= ny; z= nz; structure= Bridge (make_dir4_exn x z nx nz)}
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
            add_ground_neighbor ~from_stair:false ~y ~ny ~nx ~nz acc )
  in
  let stair_neighbors ~x ~y ~z stair_dir =
    List.fold (two_neighbors_of_dir4 stair_dir) ~init:[]
      ~f:(fun acc (dx, dy, dz) ->
        let y = y + dy in
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
            add_ground_neighbor ~from_stair:true ~y ~ny ~nx ~nz acc )
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
    let dx, dz = neighbor_of_dir4 bridge_dir in
    let nx = x + dx in
    let nz = z + dz in
    match get_obstacle_in_margin ~x:nx ~z:nz with
    | Overlay.Canon.Impassable ->
        []
    | Bridgeable ->
        let ny = get_elevation ~x:nx ~z:nz in
        add_bridge_continue_neighbor bridge_dir ~y ~ny ~nx ~nz []
    | Clear ->
        let ny = get_elevation ~x:nx ~z:nz in
        add_bridge_continue_neighbor bridge_dir ~y ~ny ~nx ~nz
        @@ add_bridge_end_neighbor bridge_dir ~y ~ny ~nx ~nz []
  in
  match structure with
  | Road ->
      ground_neighbors ~x ~y ~z
  | Stair dir ->
      stair_neighbors ~x ~y ~z dir
  | Bridge dir ->
      bridge_neighbors ~x ~y ~z dir

let put_roads_onto_sparse_grid (roads : t list) ~grid =
  List.fold roads ~init:grid ~f:(fun grid road ->
      Sparse_grid.put grid road.x road.z road )

(**
  widen_path makes Paved roads three blocks wide. Each block is the highest
  elevation and nicest niceness of any road it touches.
 *)
let widen_roads (roads : t Sparse_grid.t) =
  let roads_lo_to_hi =
    Sparse_grid.fold roads (fun coord road ls -> (coord, road) :: ls) []
    |> List.stable_sort ~compare:(fun (_, a) (_, b) ->
           match compare_structure a.structure b.structure with
           | 0 ->
               Int.compare a.y b.y
           | i ->
               i )
  in
  List.fold roads_lo_to_hi
    ~init:(Sparse_grid.make (Sparse_grid.side roads))
    ~f:(fun g ((x, z), coord) ->
      match coord.structure with
      | Road ->
          Mg_util.Range.fold (z - 1) (z + 1) g (fun g z ->
              Mg_util.Range.fold (x - 1) (x + 1) g (fun g x ->
                  Sparse_grid.put g x z coord ) )
      | Stair (N | S) ->
          Mg_util.Range.fold (x - 1) (x + 1) g (fun g x ->
              Sparse_grid.put g x z coord )
      | Stair (E | W) ->
          Mg_util.Range.fold (z - 1) (z + 1) g (fun g z ->
              Sparse_grid.put g x z coord )
      | Bridge _ ->
          Sparse_grid.put g x z coord )

let extract_bridges paths =
  (* direction always goes from min to max *)
  let normalize_dir = function N | S -> S | E | W -> E in
  let rec go paths cur_bridge bridges =
    match paths with
    | {x; y; z; structure= Bridge direction} :: paths -> (
        let direction = normalize_dir direction in
        match cur_bridge with
        | Some {x= cx; y= _; z= cz; length; direction= _} ->
            (* TODO check same direction? *)
            go paths
              (Some {x= min x cx; y; z= min z cz; length= length + 1; direction})
              bridges
        | None ->
            go paths (Some {x; y; z; direction; length= 1}) bridges )
    | _ :: paths -> (
      match cur_bridge with
      | Some cb ->
          go paths None (cb :: bridges)
      | None ->
          go paths None bridges )
    | [] -> (
      match cur_bridge with Some cb -> cb :: bridges | None -> bridges )
  in
  go paths None []
