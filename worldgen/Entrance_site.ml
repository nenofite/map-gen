open! Core_kernel

type t = {floor_elev: int} [@@deriving bin_io]

let cavern_entrance_fits_within_region ~canon_side ~x ~z =
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  Minecraft_converter.within_region_boundaries ~canon_side ~min_x:(minx + x)
    ~max_x:(maxx + x) ~min_z:(minz + z) ~max_z:(maxz + z)

let can_build_cavern_entrance canon ~x ~z =
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  (* TODO add padding for stairs *)
  Range.for_all (z + minz) (z + maxz) (fun z ->
      Range.for_all (x + minx) (x + maxx) (fun x ->
          Canonical_overlay.can_build_on
            (Grid.get x z canon.Canonical_overlay.obstacles) ) )

let add_cavern_entrance_obstacles obs ~x ~z =
  (* TODO use this function *)
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  (* TODO add padding for stairs *)
  Range.fold (z + minz) (z + maxz) obs (fun obs z ->
      Range.fold (x + minx) (x + maxx) obs (fun obs x ->
          Canonical_overlay.Obstacles.set x z
            Canonical_overlay.Obstacle.Impassable obs ) )

let prepare ~x ~z =
  let canon = Canonical_overlay.require () in
  let cavern = Cavern_overlay.require () in
  if
    cavern_entrance_fits_within_region ~canon_side:canon.side ~x ~z
    && can_build_cavern_entrance canon ~x ~z
  then
    match Grid_compat.at cavern x z with
    | {floor_elev; ceiling_elev}
      when ceiling_elev > floor_elev
           && floor_elev > Cavern_overlay.magma_sea_elev ->
        Tale.logf "cavern entrance at %d, %d" x z ;
        Some ({floor_elev}, x, z)
    | _ ->
        None
  else None

let put_obstacles t ~x ~z ~put =
  ignore (t, x, z, put) ;
  ()

let max_height_within (args : Minecraft_converter.region_args) ~minx ~maxx ?y
    ~minz ~maxz () : int =
  let m = ref 0 in
  for z = minz to maxz do
    for x = minx to maxx do
      let here = Minecraft.Region.height_at args.region ~x ?y ~z in
      m := max !m here
    done
  done ;
  !m

let apply t ~x ~z ~args : unit =
  let {floor_elev= tube_depth} = t in
  let top = Site_templates.cavern_entrance in
  let tube = Site_templates.cavern_entrance_tube in
  let base = Site_templates.cavern_entrance_base in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  let y =
    max_height_within args ~minx:(minx + x) ~maxx:(maxx + x) ~minz:(minz + z)
      ~maxz:(maxz + z) ()
    + 3
  in
  Building.stair_foundation args ~minx:(minx + x) ~maxx:(maxx + x) ~y
    ~minz:(minz + z) ~maxz:(maxz + z) ;
  Minecraft_template.place_overwrite top args.region ~x ~y ~z ;
  let tube_height = Minecraft_template.height_of tube in
  let base_height = Minecraft_template.height_of base in
  let tube_sections = (y - tube_depth - base_height) / tube_height in
  for i = 1 to tube_sections do
    let y = y - (i * tube_height) in
    Minecraft_template.place_overwrite tube args.region ~x ~y ~z
  done ;
  let y = y - (tube_sections * tube_height) - base_height in
  Minecraft_template.place_overwrite base args.region ~x ~y ~z ;
  let minx, maxx = base.bounds_x in
  let minz, maxz = base.bounds_z in
  Building.stair_foundation args ~minx:(minx + x) ~maxx:(maxx + x) ~y
    ~minz:(minz + z) ~maxz:(maxz + z)
