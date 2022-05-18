open! Core

type region_args = Minecraft.Region.t

module T = struct
  type 'a t = Pos.t -> region_args -> 'a

  let bind (t : 'a t) ~f : 'b t = fun pos region -> f (t pos region) pos region

  let return a : 'a t = fun _pos _args -> a

  let map (t : 'a t) ~f : 'b t = fun pos region -> f (t pos region)

  let map = `Custom map
end

include T
include Monad.Make (T)

let nop = return ()

let with_pos_applied ~x ~y ~z f : 'a t =
 fun pos _region ->
  let x, y, z = Pos.apply pos ~x ~y ~z in
  f ~x ~y ~z

let run (t : 'a t) ~pos ~region = t pos region

let set_block mat ~x ~y ~z : unit t =
 fun pos region ->
  let x, y, z = Pos.apply pos ~x ~y ~z in
  let mat = Pos.apply_rotation_to_block mat ~pos in
  Minecraft.Region.set_block mat ~x ~y ~z region

let get_block ~x ~y ~z : Minecraft.Block.material t =
 fun pos region ->
  let x, y, z = Pos.apply pos ~x ~y ~z in
  Minecraft.Region.get_block ~x ~y ~z region
  |> Pos.negate_rotation_of_block ~pos

let height_at ~x ~z : int t =
 fun pos region ->
  let x, _, z = Pos.apply pos ~x ~y:0 ~z in
  Minecraft.Region.height_at ~x ~z region

let place_template (t : _ Minecraft_template.t) ~x ~y ~z : unit t =
  let open Let_syntax in
  List.fold t.blocks ~init:nop ~f:(fun m (dx, dy, dz, mat) ->
      let%bind () = m in
      set_block mat ~x:(x + dx) ~y:(y + dy) ~z:(z + dz) )
