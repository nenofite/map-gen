open Core_kernel

module Coord = struct
  module T = struct
    type direction = Ns | Ew
    [@@deriving eq, ord, sexp, bin_io]

    type bridge = 
      | No_bridge
      | Ns_bridge
      | Ew_bridge
    [@@deriving eq, ord, sexp, bin_io]

    type t = {
      x: int;
      y: int;
      z: int;
      bridge: bridge;
    }
    [@@deriving eq, ord, sexp, bin_io]
  end
  include T
  include Comparable.Make(T)
end
include Coord.T

module A_star = A_star_gen.Make(Int)(Coord)

(* Costs *)
let flat_ground_cost = 10
let slope_ground_cost = 20
let bridge_cost = 100
let existing_road_cost = 1

let direction_of_bridge_exn = function
  | Ns_bridge -> Ns
  | Ew_bridge -> Ew
  | No_bridge -> invalid_arg "No_bridge"

let bridge_of_direction = function
  | Ns -> Ns_bridge 
  | Ew -> Ew_bridge 

let two_neighbors_of_direction = function
  | Ns -> [0, -1; 0, 1]
  | Ew -> [-1, 0; 1, 0]

let make_direction_exn x1 z1 x2 z2 =
  if x1 = x2 && z1 <> z2
  then Ns
  else if x1 <> x2 && z1 = z2
  then Ew
  else invalid_argf "cannot make direction: (%d, %d) (%d, %d)" x1 z1 x2 z2 ()

let pathfind_road ~get_elevation ~get_obstacle ~has_existing_road ~start_coords ~goal_pred ~goal_coord = (

  let (goal_x, goal_z) = goal_coord in
  let goal_y = get_elevation ~x:goal_x ~z:goal_z in
  let wrapped_goal_pred { x; y = _; z; bridge } = match bridge with
    | No_bridge -> goal_pred ~x ~z
    | _ -> false
  in

  let rec reuse_existing_roads_opt neighbors = match neighbors with
    | (coord, _cost) as old_here :: rest ->
      if has_existing_road coord then (
        let n = (coord, existing_road_cost) :: Option.value (reuse_existing_roads_opt rest) ~default:rest in
        Some n
      ) else (
        match reuse_existing_roads_opt rest with
        | None -> None
        | Some new_rest -> Some (old_here :: new_rest)
      )
    | [] -> None
  in
  let reuse_existing_roads neighbors = Option.value (reuse_existing_roads_opt neighbors) ~default:neighbors in

  let add_ground_neighbor ~y ~nx ~ny ~nz list =
    if abs (y - ny) <= 1
    then ({ x = nx; y = ny; z = nz; bridge = No_bridge }, flat_ground_cost) :: list
    else list
  in
  let add_bridge_start_neighbor ~x ~y ~z ~nx ~ny ~nz list =
    if ny <= y
    then ({ x = nx; y = ny; z = nz;
            bridge = bridge_of_direction (make_direction_exn x z nx nz) }
         , bridge_cost) :: list
    else list
  in
  let ground_neighbors ~x ~y ~z = (
    List.fold Grid.Griddable.Helpers.four_directions
      ~init:[]
      ~f:(fun acc (dx, dz) ->
          let nx = x + dx in
          let nz = z + dz in
          match get_obstacle ~x:nx ~z:nz with
          | Canonical_overlay.Impassable -> acc
          | Bridgeable -> 
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_start_neighbor ~y ~ny ~x ~z ~nx ~nz acc
          | Clear -> 
            let ny = get_elevation ~x:nx ~z:nz in
            add_ground_neighbor ~y ~ny ~nx ~nz acc
        )
  )
  in

  let add_bridge_continue_neighbor bridge ~y ~ny ~nx ~nz list = (
    if ny <= y
    then ({ x = nx; y; z = nz; bridge }, bridge_cost) :: list
    else list
  )
  in
  let add_bridge_end_neighbor _bridge ~y ~ny ~nx ~nz list = (
    if ny = y
    then ({ x = nx; y; z = nz; bridge = No_bridge }, flat_ground_cost) :: list
    else list
  )
  in
  let bridge_neighbors ~x ~y ~z bridge = (
    List.fold (two_neighbors_of_direction (direction_of_bridge_exn bridge))
      ~init:[]
      ~f:(fun acc (dx, dz) ->
          let nx = x + dx in
          let nz = z + dz in
          match get_obstacle ~x:nx ~z:nz with
          | Canonical_overlay.Impassable -> acc
          | Bridgeable ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_continue_neighbor bridge ~y ~ny ~nx ~nz acc
          | Clear ->
            let ny = get_elevation ~x:nx ~z:nz in
            add_bridge_continue_neighbor bridge ~y ~ny ~nx ~nz @@
            add_bridge_end_neighbor bridge ~y ~ny ~nx ~nz acc
        )
  )
  in

  let neighbors { x; y; z; bridge } =
    let n = match bridge with
      | No_bridge -> ground_neighbors ~x ~y ~z
      | b -> bridge_neighbors ~x ~y ~z b
    in
    reuse_existing_roads n
  in

  let heuristic { x; y; z; bridge = _ } = existing_road_cost * (abs (goal_x - x) + abs (goal_z - z) + abs (goal_y - y)) in

  let start_set = List.map start_coords ~f:(fun (x, z, g) -> ({ x; y = get_elevation ~x ~z; z; bridge = No_bridge }, g)) in

  A_star.pathfind ~neighbors ~heuristic ~start_set ~goal:wrapped_goal_pred ~max_iters:10_000_000
)

let%test_module "tests" = (module struct
  let palette = [
    ".", Canonical_overlay.Clear;
    "X", Impassable;
    "O", Bridgeable;
  ]

  let print_path grid road = (
    let side = Grid.Mut.side grid in
    let road_at ~x ~z = List.find road ~f:(fun coord -> coord.x = x && coord.z = z) in
    for z = 0 to side - 1 do
      for x = 0 to side - 1 do
        let p = match road_at ~x ~z with
          | Some { bridge; _ } -> (match bridge with
              | No_bridge -> "="
              | Ns_bridge -> "v"
              | Ew_bridge -> ">"
            )
          | None -> (match Grid.Mut.get ~x ~z grid with
              | Canonical_overlay.Clear -> "."
              | Impassable -> "X"
              | Bridgeable -> "O"
            )
        in
        print_string p;
        print_string " ";
      done;
      print_endline "";
    done;
  )

  let%expect_test "basic pathing" = (
    let grid = Test_helpers.grid_of_txt ~palette {|
      X X X X X X X X X X X X X X X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . . . . . . X 
      X . . . . . X . . X . . . . X 
      X . . . . . X . . X . . . . X 
      X . . . . . X . . X . . . . X 
      X . . . . . . . . X . . . . X 
      X . . . . . . . . X . . . . X 
      X . . . X X X X X X . X X X X 
      X . . . X . . . . . . . . . X 
      X X X X X X X X X X X X X X X 
    |} in
    let get_elevation ~x:_ ~z:_ = 0 in
    let get_obstacle ~x ~z = Grid.Mut.get ~x ~z grid in
    let has_existing_road _ = false in
    let start = (2, 1, 0) in
    let goal = (13, 13) in
    let road = Option.value_exn
        (pathfind_road ~get_elevation ~get_obstacle ~has_existing_road
           ~start_coords:[start]
           ~goal_pred:(fun ~x ~z -> x = 13 && z = 13)
           ~goal_coord:goal)
    in
    print_path grid road;
    [%expect {|
      X X X X X X X X X X X X X X X
      X . = . . . X . . . . . . . X
      X . = . . . X . . . . . . . X
      X . = . . . X . . . . . . . X
      X . = . . . X . . . . . . . X
      X . = . . . X . . . . . . . X
      X . = . . . X = = = = . . . X
      X . = . . . X = . X = . . . X
      X . = . . . X = . X = . . . X
      X . = = = . X = . X = . . . X
      X . . . = = = = . X = . . . X
      X . . . . . . . . X = . . . X
      X . . . X X X X X X = X X X X
      X . . . X . . . . . = = = = X
      X X X X X X X X X X X X X X X
    |}];
  )

  let%expect_test "bridge pathing" = (
    let grid = Test_helpers.grid_of_txt ~palette {|
      X X X X X X X X X X X X X X X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . . . . . . X 
      X . . . . O O O . X O O . . X 
      X . . . . O O O . X O O . . X 
      X . . . . O O . . X O O . . X 
      X . . . . O O . . X O O . . X 
      X . . . . O O O . X O O . . X 
      X . . . X X X X X X . X X X X 
      X . . . X . . . . . . . . . X 
      X X X X X X X X X X X X X X X 
    |} in
    let get_elevation ~x:_ ~z:_ = 0 in
    let get_obstacle ~x ~z = Grid.Mut.get ~x ~z grid in
    let has_existing_road _ = false in
    let start = (2, 1, 0) in
    let goal = (13, 13) in
    let road = Option.value_exn
        (pathfind_road ~get_elevation ~get_obstacle ~has_existing_road
           ~start_coords:[start]
           ~goal_pred:(fun ~x ~z -> x = 13 && z = 13)
           ~goal_coord:goal)
    in
    print_path grid road;
    [%expect {|
      X X X X X X X X X X X X X X X
      X . = . . O O O . . . . . . X
      X . = . . O O O . . . . . . X
      X . = . . O O O . . . . . . X
      X . = . . O O O . . . . . . X
      X . = . . O O O . . . . . . X
      X . = . . O O O = = = . . . X
      X . = . . O O O = X v O . . X
      X . = . . O O O = X v O . . X
      X . = = = > > = = X v O . . X
      X . . . . O O . . X v O . . X
      X . . . . O O O . X v O . . X
      X . . . X X X X X X = X X X X
      X . . . X . . . . . = = = = X
      X X X X X X X X X X X X X X X
    |}];
  )

  let%expect_test "reuse existing roads" = (
    let grid = Test_helpers.grid_of_txt ~palette {|
      X X X X X X X X X X X X X X X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X . . . . . . . . . . . . . X 
      X X X X X X X X X X X X X X X 
    |} in
    let get_elevation ~x:_ ~z:_ = 0 in
    let get_obstacle ~x ~z = Grid.Mut.get ~x ~z grid in
    let has_existing_road coord = coord.z = 2 in
    let start = (2, 1, 0) in
    let goal = (13, 1) in
    let road = Option.value_exn
        (pathfind_road ~get_elevation ~get_obstacle ~has_existing_road
           ~start_coords:[start]
           ~goal_pred:(fun ~x ~z -> x = 13 && z = 1)
           ~goal_coord:goal)
    in
    print_path grid road;
    [%expect {|
      X X X X X X X X X X X X X X X
      X . = . . . . . . . . . . = X
      X . = = = = = = = = = = = = X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X . . . . . . . . . . . . . X
      X X X X X X X X X X X X X X X
    |}];
  )
end)