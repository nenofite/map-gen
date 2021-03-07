open Core_kernel

type site = Cavern_entrance of int [@@deriving bin_io]

type t = site option Point_cloud.t [@@deriving bin_io]

let prepare () =
  let canon = Canonical_overlay.require () in
  let cavern = Cavern_overlay.require () in
  Point_cloud.init_f ~side:canon.side ~spacing:128 (fun ~xf ~yf ~xi:_ ~yi:_ ->
      let x = int_of_float xf in
      let y = int_of_float yf in
      if
        not
          ( Grid.is_within x y canon.obstacles
          && not (Canonical_overlay.can_build_on (Grid.get x y canon.obstacles))
          )
      then
        match Grid_compat.at cavern x y with
        | {floor_elev; ceiling_elev}
          when ceiling_elev > floor_elev
               && floor_elev > Cavern_overlay.magma_sea_elev ->
            Tale.logf "cavern entrance at %d, %d" x y ;
            Some (Cavern_entrance floor_elev [@explicit_arity])
            [@explicit_arity]
        | _ ->
            None
      else None)

let apply_standard args ~x ~z template =
  Building.apply_template args ~x ~z template

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

let apply_cavern_entrance args ~tube_depth ~x ~z : unit =
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
  let tube_height = Minecraft_template.height tube in
  let base_height = Minecraft_template.height base in
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

let apply_region sites (args : Minecraft_converter.region_args) : unit =
  Sparse_grid.iter sites.Point_cloud.points
    (fun _ Point_cloud.{px= x; py= z; value= site} ->
      let x = int_of_float x in
      let z = int_of_float z in
      if Minecraft.Region.is_within ~x ~y:0 ~z args.region then
        match site with
        | ((Some ((Cavern_entrance tube_depth)[@explicit_arity]))[@explicit_arity
                                                                   ]) ->
            apply_cavern_entrance args ~tube_depth ~x ~z
        | None ->
            ())

let require, prepare, apply =
  Overlay.make_no_canon "site" prepare apply_region bin_reader_t bin_writer_t
