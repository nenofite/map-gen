open! Core_kernel

type pos = {origin: int * int * int; rotation: int}

let at ~x ~y ~z ~rotation_cw = {origin= (x, y, z); rotation= rotation_cw mod 4}

let get_cw_rotations pos = pos.rotation mod 4

let apply_pos pos ~x ~y ~z =
  let {origin= ox, oy, oz; rotation} = pos in
  let rec rotate times ~x ~y ~z =
    if times <= 0 then (x, y, z) else rotate (times - 1) ~x:(-z) ~y ~z:x
  in
  let x, y, z = rotate rotation ~x ~y ~z in
  (x + ox, y + oy, z + oz)

let apply_rotation_to_block mat ~pos =
  (* TODO *)
  ignore pos ; mat

let negate_rotation_of_block mat ~pos =
  (* TODO *)
  ignore pos ; mat

type 'a t = pos -> 'a

let get_elevation ~x ~z pos =
  let canon = Overlay.Canon.require () in
  let x, _, z = apply_pos pos ~x ~y:0 ~z in
  Grid.get x z canon.elevation
