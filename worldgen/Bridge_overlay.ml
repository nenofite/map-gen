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

module A_star = A_star_gen.Make(Coord)

type obstacle = Clear | River | Impassable

(* Costs *)
let flat_ground_cost = 1.
let slope_ground_cost = 2.
let bridge_cost = 10.

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

let pathfind_road ~get_elevation ~get_obstacle ~start_coords ~goal_pred ~goal_coord = (

  let (goal_x, goal_y, goal_z) = goal_coord in
  let wrapped_goal_pred { x; y = _; z; bridge } = match bridge with
    | No_bridge -> goal_pred ~x ~z
    | _ -> false
  in

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
          | Impassable -> acc
          | River -> 
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
          | Impassable -> acc
          | River ->
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
    match bridge with
    | No_bridge -> ground_neighbors ~x ~y ~z
    | b -> bridge_neighbors ~x ~y ~z b
  in

  let heuristic { x; y; z; bridge = _ } = Float.of_int @@ abs (goal_x - x) + abs (goal_z - z) + abs (goal_y - y) in
  (* let heuristic { x; y; z; bridge = _ } = Float.of_int @@ Int.((goal_x - x) ** 2 + (goal_z - z) ** 2 + (goal_y - y) ** 2) in *)

  let start_set = List.map start_coords ~f:(fun (x, y, z) -> { x; y; z; bridge = No_bridge }) in

  A_star.pathfind ~neighbors ~heuristic ~start_set ~goal:wrapped_goal_pred
)

let%test_module "tests" = (module struct
  let palette = [
    ".", Clear;
    "X", Impassable;
    "O", River;
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
              | Clear -> "."
              | Impassable -> "X"
              | River -> "O"
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
    let start = (2, 0, 1) in
    let goal = (13, 0, 13) in
    let road = Option.value_exn
        (pathfind_road ~get_elevation ~get_obstacle
           ~start_coords:[start]
           ~goal_pred:(fun ~x ~z -> x = 13 && z = 13)
           ~goal_coord:goal)
    in
    print_path grid road;
    [%expect {|
      X X X X X X X X X X X X X X X
      X . = = = = X . . . . . . . X
      X . . . . = X . . . . . . . X
      X . . . . = X . . . . . . . X
      X . . . . = X . . . . . . . X
      X . . . . = X . . . . . . . X
      X . . . . = X = = = = . . . X
      X . . . . = X = . X = . . . X
      X . . . . = X = . X = . . . X
      X . . . . = X = . X = . . . X
      X . . . . = = = . X = . . . X
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
    let start = (2, 0, 1) in
    let goal = (13, 0, 13) in
    let road = Option.value_exn
        (pathfind_road ~get_elevation ~get_obstacle
           ~start_coords:[start]
           ~goal_pred:(fun ~x ~z -> x = 13 && z = 13)
           ~goal_coord:goal)
    in
    print_path grid road;
    [%expect {|
      X X X X X X X X X X X X X X X
      X . = = = O O O . . . . . . X
      X . . . = O O O . . . . . . X
      X . . . = O O O . . . . . . X
      X . . . = O O O . . . . . . X
      X . . . = O O O . . . . . . X
      X . . . = O O O = = = . . . X
      X . . . = O O O = X v O . . X
      X . . . = O O O = X v O . . X
      X . . . = > > = = X v O . . X
      X . . . . O O . . X v O . . X
      X . . . . O O O . X v O . . X
      X . . . X X X X X X = X X X X
      X . . . X . . . . . = = = = X
      X X X X X X X X X X X X X X X
    |}];
  )
end)