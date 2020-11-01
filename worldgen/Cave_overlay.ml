open Core_kernel

type metaball = {
  center: Geometry.Vec3i.t;
  radius: int;
}
[@@deriving bin_io]

type t = metaball list (* TODO *)
[@@deriving bin_io]

(** Average blocks between cave entrances, before probability is applied *)
let cave_spacing = 100

(** Percent chance that a cave entrance has a cave *)
let cave_prob = 10

(** Average block length of a cave segment *)
let joint_spacing = 10

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
  let max_moves = Random.int_incl 10 20 in
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
  go joints []
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
    | {center; radius} :: rest -> 
      let open Int in
      let (cx, cy, cz) = center in
      let r4 = radius ** 4 in
      let dist4 = ((x - cx) ** 2 + (y - cy) ** 2 + (z - cz) ** 2) ** 2 in
      let next_sum = Float.(sum + of_int r4 / of_int dist4) in
      go rest x y z next_sum
  in
  let value_here = go balls x y z 0. in
  Float.(value_here >= 1.)

let ball_bounds ball =
  let {center = (cx, cy, cz); radius} = ball in
  let radius = radius + 2 in
  let open Mg_util.Range in
  fold (cz - radius) (cz + radius) [] @@ fun acc z ->
  fold (cx - radius) (cx + radius) acc @@ fun acc x ->
  fold (cy - radius) (cy + radius) acc @@ fun acc y ->
  (x, y, z) :: acc
;;

let before_after balls =
  let rec go balls result =
    match balls with
    | [] | [_] -> result
    | [a; b] -> (b, [a; b]) :: result
    | a :: ((b :: c :: _) as rest) ->
      go rest ((b, [a; b; c]) :: result)
  in
  match balls with
  | [] -> []
  | [a] -> [(a, [a])]
  | (a :: b :: _) as balls -> go balls [(a, [a; b])]
;;

let prepare (canon : Canonical_overlay.t) () =
  let module Vi = Geometry.Vec3i in
  let module Vf = Geometry.Vec3f in
  let transform_and_wiggle start joints =
    match joints with
    | [] -> []
    | _fst :: rest ->
      start :: (List.map rest ~f: (fun point ->
          let wiggled = Vf.(of_int point + random_wiggle ()) in
          let spaced = Vf.(wiggled *. Float.of_int joint_spacing) |> Vi.of_float in
          Vi.(spaced + start)
        ))
  in
  let prepare_cave (start_x, start_z) =
    if Random.int 100 >= cave_prob then
      None
    else
      let start_y = Grid.at canon.elevation start_x start_z in
      let start = (start_x, start_y, start_z) in
      let points =
        random_joints () |>
        transform_and_wiggle start |>
        points_of_joints |>
        List.map ~f: (fun point -> { center = point; radius = 2 }) (* TODO *)
      in
      Some points
  in

  let cave_starts =
    Point_cloud.make_int_list ~spacing: cave_spacing ~width: canon.side ~height: canon.side () |>
    List.filter_map ~f: prepare_cave
  in
  List.concat cave_starts
;;

let apply_region t (args : Minecraft_converter.region_args) =
  (* TODO *)
  let with_ba = before_after t in
  List.iter with_ba ~f: (fun (ball, ba) ->
      List.iter (ball_bounds ball) ~f: (fun (x, y, z) ->
          if Minecraft.Region.is_within ~x ~y ~z args.region && within_ball ba x y z then
            Minecraft.Region.set_block_opt Minecraft.Block.Air ~x ~y ~z args.region
        )
    )
;;

let overlay (canon : Canonical_overlay.t) : t Overlay.monad =
  Overlay.make "cave" (prepare canon) apply_region bin_reader_t bin_writer_t