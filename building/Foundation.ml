open! Core_kernel

(** Foundations and stairs *)

let column_down mat ~x ~y ~z =
  let prepare = Prepare_monad.collide_obstacle ~x ~z in
  let rec go y =
    let open Apply_monad.Let_syntax in
    let%bind here = Apply_monad.get_block ~x ~y ~z in
    if not (Minecraft.Block.is_solid here) then
      let%bind () = Apply_monad.set_block mat ~x ~y ~z in
      go (y - 1)
    else Apply_monad.nop
  in
  let apply = go y in
  Building_monad.parallel ~prepare ~apply

let rectangle_foundation mat ~minx ~maxx ~y ~minz ~maxz =
  let for_m = Mg_util.Range.iter_m ~bind:Building_monad.bind in
  for_m minz maxz (fun z -> for_m minx maxx (fun x -> column_down mat ~x ~y ~z))

let rec lay_stairs_no_max stair ~under ~x ~y ~z ~dx ~dz =
  let open Building_monad.Let_syntax in
  let%bind here_height = Building_monad.height_at ~x ~z in
  if here_height < y then
    let%bind () = Building_monad.set_block stair ~x ~y ~z in
    let%bind () = column_down under ~x ~y:(y - 1) ~z in
    lay_stairs_no_max stair ~under ~x:(x + dx) ~y:(y - 1) ~z:(z + dz) ~dx ~dz
  else return ()

let rec lay_stairs stair ~under ~x ~y ~z ~dx ~dz ~max =
  let open Building_monad.Let_syntax in
  let%bind here_height = Building_monad.height_at ~x ~z in
  if here_height < y then
    let%bind () =
      Building_monad.only_prepare (Prepare_monad.fail_if (max <= 0))
    in
    let%bind () = Building_monad.set_block stair ~x ~y ~z in
    let%bind () = column_down under ~x ~y:(y - 1) ~z in
    lay_stairs stair ~under ~x:(x + dx) ~y:(y - 1) ~z:(z + dz) ~dx ~dz
      ~max:(max - 1)
  else return ()
