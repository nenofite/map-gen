open! Core_kernel

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
