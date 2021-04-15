open Core_kernel

type flower = {block: Minecraft.Block.material; percentage: int}
[@@deriving eq, bin_io]

type cactus = {percentage: int} [@@deriving eq, bin_io]

type mid_biome = Plain of flower | Forest of flower | Desert of cactus
[@@deriving eq, bin_io]

type shore_biome = Sand | Gravel | Clay [@@deriving eq, bin_io]

type high_biome = Pine_forest | Barren | Snow [@@deriving eq, bin_io]

type biome = Mid of mid_biome | Shore of shore_biome | High of high_biome
[@@deriving eq, bin_io]

type t = biome Grid.t * Overlay.Canon.delta [@@deriving bin_io]

module Biome_grid = Grid.Make0 (struct
  type t = biome

  let ( = ) = equal_biome
end)

type low_mid_high = {shore: shore_biome; mid: mid_biome; high: high_biome}
[@@deriving eq, bin_io]

module Low_mid_high_grid = Grid.Make0 (struct
  type t = low_mid_high

  let ( = ) = equal_low_mid_high
end)

let colorize_biome = function
  | Mid (Plain _) ->
      0x86A34D
  | Mid (Forest _) ->
      0x388824
  | Mid (Desert _) ->
      0xD9D0A1
  | High Pine_forest ->
      0x286519
  | High Barren ->
      0x727272
  | High Snow ->
      0xFDFDFD
  | Shore Sand ->
      0xF5EAB7
  | Shore Gravel ->
      0x828282
  | Shore Clay ->
      0x8E7256

let random_flower () =
  let open Minecraft.Block in
  let block =
    match Random.int 12 with
    | 0 ->
        Dandelion
    | 1 ->
        Poppy
    | 2 ->
        Blue_orchid
    | 3 ->
        Allium
    | 4 ->
        Azure_bluet
    | 5 ->
        Red_tulip
    | 6 ->
        Orange_tulip
    | 7 ->
        White_tulip
    | 8 ->
        Pink_tulip
    | 9 ->
        Oxeye_daisy
    | 10 ->
        Cornflower
    | 11 | _ ->
        Lily_of_the_valley
    (* TODO tall flowers *)
  in
  {block; percentage= Random.int_incl 10 100}

let random_cactus () = {percentage= Random.int_incl 10 100}

let random_mid () =
  match Random.int 10 with
  | 0 | 1 | 2 | 3 | 4 ->
      Plain (random_flower ())
  | 5 | 6 | 7 ->
      Forest (random_flower ())
  | 8 | _ ->
      Desert (random_cactus ())

let random_shore () =
  match Random.int 3 with 0 -> Sand | 1 -> Gravel | _ -> Clay

let random_high () =
  match Random.int 3 with 0 -> Pine_forest | 1 -> Barren | _ -> Snow

let select_elevation ~(base : Base_overlay.tile Grid.t)
    (lmh : low_mid_high option Grid.Mut.t) =
  Biome_grid.init ~side:base.Grid.side (fun (x, z) ->
      let here_base = Grid.get x z base in
      let elevation = here_base.elevation in
      let {shore; mid; high} =
        Option.value_exn ~message:"grid coordinate has no biome"
          (Grid.Mut.get ~x ~z lmh)
      in
      if River.has_water here_base || elevation <= Heightmap.sea_level + 2 then
        Shore shore
      else if elevation >= Heightmap.mountain_level - 20 then
        (* TODO use some sort of interp for mid-high cutoff, instead of flat line *)
        High high
      else Mid mid )

let prepare_voronoi () =
  let voronoi_frac = 4 in
  let high_cost = 10 in
  let canon = Overlay.Canon.require () in
  let base, _ = Base_overlay.require () in
  let voronoi_side = canon.side / voronoi_frac in
  let high_cost_spots =
    Grid.Poly.init ~side:voronoi_side (fun (x, z) ->
        Range.exists z
          (z + voronoi_frac - 1)
          (fun z ->
            Range.exists x
              (x + voronoi_frac - 1)
              (fun x ->
                let t = Grid.get x z base in
                t.elevation >= Heightmap.mountain_level - 20
                || River.has_ocean t ) ) )
  in
  let random_lmh () =
    {shore= random_shore (); mid= random_mid (); high= random_high ()}
  in
  let points =
    Point_cloud.init ~side:voronoi_side ~spacing:(512 / voronoi_frac)
      (fun _x _z -> random_lmh ())
  in
  let lmh = Grid.Mut.create ~side:voronoi_side ~alloc_side:canon.side None in
  let initial_live_set =
    Sparse_grid.fold points.points
      (fun _ point ls ->
        let value = point.value in
        let x = Int.of_float point.px in
        let z = Int.of_float point.py in
        (x, z, value) :: ls )
      []
  in
  let spread_into ~x ~z ~level lmh_val live_set =
    if Grid.is_within x z high_cost_spots then
      let cost = if Grid.get x z high_cost_spots then high_cost else 1 in
      let occupied = Option.is_some (Grid.Mut.get ~x ~z lmh) in
      if occupied then live_set else (level + cost, (x, z, lmh_val)) :: live_set
    else live_set
  in
  Grid_flood.flood_gen ~init:() ~live:[(0, initial_live_set)]
    ~spread:(fun () ~level (x, z, lmh_val) ->
      let occupied = Option.is_some (Grid.Mut.get ~x ~z lmh) in
      if occupied then ((), [])
      else (
        Grid.Mut.set ~x ~z (Some lmh_val) lmh ;
        let next_live_set =
          []
          |> spread_into ~x ~z:(z - 1) ~level lmh_val
          |> spread_into ~x:(x + 1) ~z ~level lmh_val
          |> spread_into ~x ~z:(z + 1) ~level lmh_val
          |> spread_into ~x:(x - 1) ~z ~level lmh_val
        in
        ((), next_live_set) ) ) ;
  (* TODO base on voronoi_frac *)
  Subdivide_mut.subdivide lmh ;
  Subdivide_mut.subdivide lmh ;
  let biomes = select_elevation ~base lmh in
  biomes

let get_obstacle dirt biome =
  match biome with
  | Mid (Desert _) -> (
    match dirt = 0 with true -> Overlay.Canon.Impassable | false -> Clear )
  | _ ->
      Clear

let prepare () =
  let dirt = Dirt_overlay.require () in
  let biomes = prepare_voronoi () in
  let biome_obstacles =
    Overlay.Canon.Obstacles.zip_map dirt biomes ~f:get_obstacle
  in
  let canond = Overlay.Canon.make_delta ~obstacles:(`Add biome_obstacles) () in
  (biomes, canond)

let colorize (biome, base) =
  if River.has_water base then River.colorize base
  else
    let elev = base.River.Tile.elevation in
    let frac = float_of_int elev /. 200. in
    let frac =
      let open Float in
      max (min frac 1.) 0.
    in
    let black = 0 in
    let biome_col = colorize_biome biome in
    Color.blend black biome_col frac

let apply_progress_view (state : t) =
  let base, _ = Base_overlay.require () in
  let biome, _canon = state in
  let side = base.Grid.side in
  let layer = Progress_view.push_layer () in
  let g = Grid_compat.zip biome base in
  Progress_view.update ~fit:(0, side, 0, side)
    ~draw_dense:(Progress_view_helper.dense colorize)
    ~state:g layer ;
  Progress_view.save ~side ~format:Images.Png "biome" ;
  ()

(** overwrite_stone_air only sets the block if it is Stone or Air, to avoid overwriting rivers etc. *)
let overwrite_stone_air region x y z block =
  match Minecraft.Region.get_block_opt region ~x ~y ~z with
  | Some Air | Some Stone ->
      Minecraft.Region.set_block ~x ~y ~z block region
  | Some _ | None ->
      ()
  [@@ocaml.doc
    " overwrite_stone_air only sets the block if it is Stone or Air, to avoid \
     overwriting rivers etc. "]

let apply_dirt (dirt : int Grid_compat.t) (state, _canon)
    (region : Minecraft.Region.t) =
  let region = region in
  Minecraft_converter.iter_blocks region (fun ~x ~z ->
      let open Minecraft.Region in
      let elev = height_at ~x ~z region in
      let dirt_depth = Grid_compat.at dirt x z in
      match Grid_compat.at state x z with
      | Mid (Plain _ | Forest _) | High Pine_forest ->
          (* Dirt (will become grass in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Mid (Desert _) ->
          (* Sand with rocks sticking out *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Sand
          done ;
          if dirt_depth = 0 then
            (* Add some rock sticking out *)
            for y = elev + 1 to elev + 2 do
              overwrite_stone_air region x y z Stone
            done
      | High Barren ->
          (* Gravel *)
          let gravel_depth = max 0 (dirt_depth - Dirt_overlay.max_depth + 2) in
          for y = elev - gravel_depth + 1 to elev do
            overwrite_stone_air region x y z Gravel
          done
      | High Snow ->
          (* Dirt (will add snow in Plant_overlay) *)
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z Dirt
          done
      | Shore m ->
          let material =
            match m with
            | Sand ->
                Minecraft.Block.Sand
            | Gravel ->
                Minecraft.Block.Gravel
            | Clay ->
                Minecraft.Block.Clay
          in
          for y = elev - dirt_depth + 1 to elev do
            overwrite_stone_air region x y z material
          done )

let apply_region state (region : Minecraft.Region.t) =
  let dirt = Dirt_overlay.require () in
  apply_dirt dirt state region

let require, prepare, apply =
  Overlay.make_no_canon "biome" prepare ~apply_progress_view apply_region
    bin_reader_t bin_writer_t
