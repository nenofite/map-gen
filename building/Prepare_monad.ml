open! Core

type state =
  {obstacles: Overlay.Canon.Obstacle.t Grid.t; region: (int * int) option}

let put_obstacle_into_state state ~x ~z =
  let {obstacles; _} = state in
  let obstacles =
    Grid.Mut.copy_set ~x ~z Overlay.Canon.Obstacle.Impassable obstacles
  in
  {state with obstacles}

type 'a result = Ok of 'a * state | Failed of string Lazy.t

module T = struct
  type 'a t = state -> Pos.t -> 'a result

  let bind (t : 'a t) ~f state pos =
    match t state pos with
    | Ok (a, a_state) ->
        f a a_state pos
    | Failed _ as f ->
        f

  let return a state _pos = Ok (a, state)

  (* let map (t : 'a t) ~f state pos =
     match t state pos with
     | Ok (a, a_state) ->
         Ok (f a, a_state)
     | Collision ->
         Collision *)

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let nop = return ()

(* let get_pos : Shared.pos t = fun state pos -> Ok (pos, state) *)

let apply_pos ~x ~y ~z : (int * int * int) t =
 fun state pos ->
  let ((x, _y, z) as xyz) = Pos.apply pos ~x ~y ~z in
  let rx, rz = Minecraft.Region.region_containing ~x ~z in
  match state.region with
  | Some (srx, srz) ->
      if srx = rx && srz = rz then Ok (xyz, state)
      else
        Failed
          ( lazy
            (Printf.sprintf "should be in region %d,%d but touched region %d,%d"
               srx srz rx rz ) )
  | None ->
      Ok (xyz, {state with region= Some (rx, rz)})

let run (t : 'a t) ~pos =
  let canon = Overlay.Canon.require () in
  let obstacles = Grid.make ~side:canon.side Overlay.Canon.Obstacle.Clear in
  let state = {obstacles; region= None} in
  match t state pos with
  | Ok (a, state) ->
      let {obstacles; region= _} = state in
      let canond = Overlay.Canon.make_delta ~obstacles:(`Add obstacles) in
      Result.return (a, canond)
  | Failed s ->
      Result.fail s

let fail_if cond ~msg : unit t =
 fun state _pos -> if cond then Failed msg else Ok ((), state)

let after_checking_region state ~x ~z ~f =
  let rx, rz = Minecraft.Region.region_containing ~x ~z in
  match state.region with
  | Some (srx, srz) ->
      if srx = rx && srz = rz then f state
      else
        Failed
          ( lazy
            (Printf.sprintf "should be in region %d,%d but touched region %d,%d"
               srx srz rx rz ) )
  | None ->
      f {state with region= Some (rx, rz)}

let collide_obstacle ~x ~z =
  let open Let_syntax in
  let%bind x, _, z = apply_pos ~x ~y:0 ~z in
  fun state _pos ->
    let canon = Overlay.Canon.require () in
    if
      Grid.is_within ~x ~z canon.obstacles
      && Overlay.Canon.can_build_on (Grid.get ~x ~z canon.obstacles)
    then
      let state = put_obstacle_into_state state ~x ~z in
      Ok ((), state)
    else Failed (lazy (Printf.sprintf "hit obstacle at %d, %d" x z))

(** apply position to (x, y, z), run f, and return the result *)
let with_pos_applied ~x ~y ~z f : 'a t =
  let open Let_syntax in
  let%map x, y, z = apply_pos ~x ~y ~z in
  f ~x ~y ~z

let place_template (t : _ Minecraft_template.t) ~x ~y:_ ~z : unit t =
  let open Let_syntax in
  List.fold t.footprint ~init:nop ~f:(fun m (fx, fz) ->
      let%bind () = m in
      collide_obstacle ~x:(x + fx) ~z:(z + fz) )
