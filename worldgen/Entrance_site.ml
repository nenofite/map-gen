open! Core_kernel

type t = {floor_elev: int} [@@deriving bin_io]

let max_stair_distance = 10

let max_height_within height_at ~minx ~maxx ~minz ~maxz : int =
  let m = ref 0 in
  for z = minz to maxz do
    for x = minx to maxx do
      let here = height_at ~x ~z in
      m := max !m here
    done
  done ;
  !m

let y_of_top_piece_at height_at ~minx ~maxx ~minz ~maxz =
  max_height_within height_at ~minx ~maxx ~minz ~maxz + 3

let cavern_entrance_fits_within_region ~canon_side ~x ~z =
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  Minecraft_converter.within_region_boundaries ~canon_side ~min_x:(minx + x)
    ~max_x:(maxx + x) ~min_z:(minz + z) ~max_z:(maxz + z)

let can_build_cavern_entrance (canon : Canonical_overlay.t) ~x ~z =
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  let y =
    y_of_top_piece_at
      (fun ~x ~z -> Grid.get x z canon.elevation)
      ~minx:(minx + x) ~maxx:(maxx + x) ~minz:(minz + z) ~maxz:(maxz + z)
  in
  Building.would_stair_foundation_fit canon.elevation ~minx:(minx + x)
    ~maxx:(maxx + x) ~y ~minz:(minz + z) ~maxz:(maxz + z)
    ~max_distance:max_stair_distance
  && Range.for_all
       (z + minz - max_stair_distance)
       (z + maxz + max_stair_distance)
       (fun z ->
         Range.for_all
           (x + minx - max_stair_distance)
           (x + maxx + max_stair_distance)
           (fun x ->
             Canonical_overlay.can_build_on (Grid.get x z canon.obstacles) ) )

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
  let {floor_elev= _} = t in
  let top = Site_templates.cavern_entrance in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  for z = z + minz - max_stair_distance to z + maxz + max_stair_distance do
    for x = x + minx - max_stair_distance to x + maxx + max_stair_distance do
      put Canonical_overlay.Obstacle.Impassable ~x ~z
    done
  done

let apply t ~x ~z ~(args : Minecraft_converter.region_args) : unit =
  let {floor_elev= tube_depth} = t in
  let top = Site_templates.cavern_entrance in
  let tube = Site_templates.cavern_entrance_tube in
  let base = Site_templates.cavern_entrance_base in
  let minx, maxx = top.bounds_x in
  let minz, maxz = top.bounds_z in
  let y =
    y_of_top_piece_at
      (fun ~x ~z -> Minecraft.Region.height_at ~x ~z args.region)
      ~minx:(minx + x) ~maxx:(maxx + x) ~minz:(minz + z) ~maxz:(maxz + z)
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
