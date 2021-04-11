open! Core_kernel

let lateral_and_up = [(-1, 0, 0); (1, 0, 0); (0, 0, -1); (0, 0, 1); (0, 1, 0)]

(** removes blocks randomly from the surface; in other words, only blocks
touching None spaces will be eaten *)
let eat (t : Core.t) ~(blocks : int) =
  let non_air_at ~x ~y ~z =
    List.exists t.blocks ~f:(fun (bx, by, bz, mat) ->
        bx = x && by = y && bz = z && not (Minecraft.Block.is_air mat) )
  in
  let touches_air ~x ~y ~z =
    List.exists lateral_and_up ~f:(fun (dx, dy, dz) ->
        not (non_air_at ~x:(x + dx) ~y:(y + dy) ~z:(z + dz)) )
  in
  let surface = ref [] in
  List.iter t.blocks ~f:(fun ((x, y, z, mat) as b) ->
      if (not (Minecraft.Block.is_air mat)) && touches_air ~x ~y ~z then
        surface := b :: !surface ) ;
  let surface = Mg_util.shuffle !surface in
  let to_remove = List.take surface blocks in
  let remaining_blocks =
    List.filter t.blocks ~f:(fun b ->
        not (List.mem to_remove b ~equal:Poly.equal) )
  in
  Core.of_blocks remaining_blocks

let eat_frac t ~(frac : float) =
  let blocks =
    Int.of_float
      (Float.round_up (Float.of_int (List.length t.Core.blocks) *. frac))
  in
  eat t ~blocks
