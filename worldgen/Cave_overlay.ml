open Core_kernel

type metaball = {
  center: Geometry.Vec3i.t;
  radii: Geometry.Vec3i.t;
}
[@@deriving bin_io]

type x = metaball list list
[@@deriving bin_io]

type t = x * Canonical_overlay.t
[@@deriving bin_io]

(** Average blocks between cave entrances, before probability is applied *)
let cave_spacing = 100

(** Percent chance that a cave entrance has a cave *)
let cave_prob = 20

(** Average block length of a cave segment *)
let joint_spacing = 10

let min_radius, max_radius = 1, 4
let min_joints, max_joints = 5, 40

let down_moves = [ 0, -1, -1;
                   1, -1, 0;
                   0, -1, 1;
                   -1, -1, 0 ]

let flat_moves = [ 0, 0, -1;
                   1, 0, 0;
                   0, 0, 1;
                   -1, 0, 0 ]

let random_joints () =
  let module V = Geometry.Vec3i in
  let rec go position prev_move points max_moves =
    if max_moves <= 0 then
      points
    else
      let next_move = List.random_element_exn (if Random.int 100 < 10 then down_moves else flat_moves) in
      if V.(next_move = prev_move *. -1) then
        (* We backtracked, so try again *)
        go position prev_move points max_moves
      else
        let next_position = V.(position + next_move) in
        go next_position next_move (next_position :: points) (max_moves - 1)
  in
  let first_move = List.random_element_exn down_moves in
  let start = (0, 0, 0) in 
  let max_moves = Random.int_incl min_joints max_joints in
  let snd_joint = V.(start + first_move) in
  go snd_joint first_move [snd_joint; start] max_moves |>
  List.rev
;;

let points_of_joints joints =
  let rec go joints points =
    match joints with
    | [] -> points
    | [final_point] -> final_point :: points
    | fst_point :: (snd_point :: _ as rest_points) ->
      let segment = List.tl_exn (Geometry.points_on_line snd_point fst_point) in
      go rest_points (segment @ points)
  in 
  go joints [] |> List.rev
;;

let add_radii points =
  let random_radii () =
    (Random.int_incl min_radius max_radius,
     Random.int_incl min_radius max_radius,
     Random.int_incl min_radius max_radius)
  in
  List.map points ~f: (fun point -> { center = point; radii = random_radii () })
;;

let random_wiggle () =
  (Random.float_range (-0.5) 0.5,
   Random.float_range (-0.5) 0.5,
   Random.float_range (-0.5) 0.5)
;;

let within_ball balls x y z =
  let rec go balls x y z sum =
    match balls with
    | [] -> sum
    | {center; radii} :: rest -> 
      let (dx, dy, dz) = Geometry.Vec3i.((x, y, z) - center) |> Geometry.Vec3f.of_int in
      let (rx, ry, rz) = Geometry.Vec3f.of_int radii in
      let dist4 = Float.(((dx / rx) ** 2. + (dy / ry) ** 2. + (dz / rz) ** 2.) ** 2.) in
      let next_sum = Float.(sum + 1. / dist4) in
      go rest x y z next_sum
  in
  let value_here = go balls x y z 0. in
  Float.(value_here >= 1.)
;;

let remove_overlaps balls =
  let rec go prev_ball balls result =
    match balls with
    | [] -> result (* Shouldn't happen *)
    | [last] -> last :: result
    | {center = (x,y,z); _} :: rest when within_ball [prev_ball] x y z -> go prev_ball rest result
    | ball :: rest -> go ball rest (ball :: result)
  in
  match balls with
  | [] -> []
  | fst :: rest -> go fst rest [fst] |> List.rev
;;

let ball_bounds ball =
  let {center = (cx, cy, cz); radii} = ball in
  let (rx, ry, rz) = radii in
  let extra = 2 in
  let open Mg_util.Range in
  fold (cz - rz - extra) (cz + rz + extra) [] @@ fun acc z ->
  fold (cx - rx - extra) (cx + rx + extra) acc @@ fun acc x ->
  fold (cy - ry - extra) (cy + ry + extra) acc @@ fun acc y ->
  (x, y, z) :: acc
;;

let obstacles_of_balls balls (canon : Canonical_overlay.t) =
  List.fold balls ~init: (Sparse_grid.make canon.side) ~f: (fun obs ball ->
      ball_bounds ball |>
      List.filter ~f: (fun (x, y, z) ->
          Grid.is_within canon.elevation x z && y = Grid_compat.at canon.elevation x z
        ) |>
      List.fold ~init: obs ~f: (fun obs (x, _y, z) ->
          Sparse_grid.put_opt obs x z ()
        )
    )
;;

let update_canon caves (canon : Canonical_overlay.t) =
  let obstacles =
    List.fold caves ~init: canon.obstacles ~f: (fun obs cave ->
        Sparse_grid.add_all (obstacles_of_balls cave canon) ~onto: obs
      )
  in
  { canon with obstacles }
;;

let transform_and_wiggle start joints =
  let module Vi = Geometry.Vec3i in
  let module Vf = Geometry.Vec3f in
  match joints with
  | [] -> []
  | _fst :: rest ->
    start :: (List.map rest ~f: (fun point ->
        let wiggled = Vf.(of_int point + random_wiggle ()) in
        let spaced = Vf.(wiggled *. Float.of_int joint_spacing) |> Vi.of_float in
        Vi.(spaced + start)
      ))
;;

let has_no_collisions cave canon =
  Sparse_grid.for_all (obstacles_of_balls cave canon) (fun (x, z) () ->
      not (Sparse_grid.has canon.obstacles x z)
    )

let rec try_make_points ~tries canon start =
  if tries <= 0 then
    None
  else
    let points =
      random_joints () |>
      transform_and_wiggle start |>
      points_of_joints |>
      add_radii |>
      remove_overlaps
    in
    if has_no_collisions points canon then
      Some points
    else
      try_make_points ~tries: (tries - 1) canon start
;;

let prepare (canon : Canonical_overlay.t) () =
  let prepare_cave (start_x, start_z) =
    if Random.int 100 >= cave_prob then
      None
    else
      let start_y = Grid_compat.at canon.elevation start_x start_z in
      let start = (start_x, start_y, start_z) in
      try_make_points ~tries: 10 canon start
  in
  let caves =
    Point_cloud.make_int_list ~spacing: cave_spacing ~width: canon.side ~height: canon.side () |>
    List.filter_map ~f: prepare_cave
  in
  (caves, update_canon caves canon)
;;

let before_and_after ~half ls =
  let rec go behind remaining result =
    match remaining with
    | [] -> result
    | here :: ahead ->
      let ba = List.rev_append (here :: Mg_util.take half behind) (Mg_util.take half ahead) in
      go (here :: behind) ahead ((here, ba) :: result)
  in
  go [] ls [] |> List.rev
;;

let apply_region (caves, _) (args : Minecraft_converter.region_args) =
  let module R = Minecraft.Region in
  List.iter caves ~f: (fun all_balls ->
      before_and_after ~half: 5 all_balls |>
      List.iter ~f: (fun (ball, nearby_balls) ->
          List.iter (ball_bounds ball) ~f: (fun (x, y, z) ->
              if R.is_within ~x ~y ~z args.region && within_ball nearby_balls x y z then
                match R.get_block_opt ~x ~y ~z args.region with
                (* Do not overwrite water in case we hit the ocean from above *)
                | None | Some (Air | Water | Flowing_water _) -> () 
                | Some _ -> R.set_block_opt Air ~x ~y ~z args.region
            )
        )
    )
;;

let overlay (canon : Canonical_overlay.t) : t Overlay.monad =
  Overlay.make "cave" (prepare canon) apply_region bin_reader_t bin_writer_t