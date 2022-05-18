open! Core

type t = {origin: int * int * int; rotation: int}

let at ~x ~y ~z ~rotation_cw = {origin= (x, y, z); rotation= rotation_cw mod 4}

let get_cw_rotations pos = pos.rotation mod 4

let apply pos ~x ~y ~z =
  let {origin= ox, oy, oz; rotation} = pos in
  let rec rotate times ~x ~y ~z =
    if times <= 0 then (x, y, z) else rotate (times - 1) ~x:(-z) ~y ~z:x
  in
  let x, y, z = rotate rotation ~x ~y ~z in
  (x + ox, y + oy, z + oz)

let apply_rotation_to_block mat ~pos =
  Minecraft.Block.rotate_cw mat ~times:pos.rotation

let negate_rotation_of_block mat ~pos =
  Minecraft.Block.rotate_cw mat ~times:(4 - pos.rotation)
