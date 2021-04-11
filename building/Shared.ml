open! Core_kernel

type pos = {origin: int * int * int; rotation: int}

let apply_pos pos ~x ~y ~z =
  let {origin= ox, oy, oz; rotation} = pos in
  (* TODO also handle block rotation (eg. stairs) *)
  ignore rotation ;
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
