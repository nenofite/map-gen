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

let lay_foundation mat ~minx ~maxx ~y ~minz ~maxz =
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
      Building_monad.only_prepare
        (Prepare_monad.fail_if (max <= 0)
           ~msg:(lazy "exceeded max stair distance") )
    in
    let%bind () = Building_monad.set_block stair ~x ~y ~z in
    let%bind () = column_down under ~x ~y:(y - 1) ~z in
    lay_stairs stair ~under ~x:(x + dx) ~y:(y - 1) ~z:(z + dz) ~dx ~dz
      ~max:(max - 1)
  else return ()

let lay_stair_foundation ?(foundation = Minecraft.Block.Cobblestone)
    ?(stair = Minecraft.Block.Cobblestone_stairs) ?(n = true) ?(e = true)
    ?(s = true) ?(w = true) ~minx ~maxx ~y ~minz ~maxz ~max_stair () =
  let open Building_monad.Let_syntax in
  let%bind () = lay_foundation foundation ~minx ~maxx ~y ~minz ~maxz in
  let for_m = Mg_util.Range.iter_m ~bind:Building_monad.bind in
  let stair d = Minecraft.Block.Stairs (stair, d) in
  let%bind () =
    for_m minx maxx (fun x ->
        let%bind () =
          (* N side *)
          if n then
            lay_stairs (stair S) ~under:foundation ~x ~y ~z:(minz - 1) ~dx:0
              ~dz:(-1) ~max:max_stair
          else Building_monad.nop
        in
        (* S side *)
        if s then
          lay_stairs (stair N) ~under:foundation ~x ~y ~z:(maxz + 1) ~dx:0 ~dz:1
            ~max:max_stair
        else Building_monad.nop )
  in
  for_m minz maxz (fun z ->
      let%bind () =
        (* E side *)
        if e then
          lay_stairs (stair W) ~under:foundation ~x:(maxx + 1) ~y ~z ~dx:1 ~dz:0
            ~max:max_stair
        else Building_monad.nop
      in
      (* W side *)
      if w then
        lay_stairs (stair E) ~under:foundation ~x:(minx - 1) ~y ~z ~dx:(-1)
          ~dz:0 ~max:max_stair
      else Building_monad.nop )
