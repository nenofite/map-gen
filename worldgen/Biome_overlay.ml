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

type t = biome Grid.t * Canonical_overlay.delta [@@deriving bin_io]

module Biome_grid = Grid.Make0 (struct
  type t = biome

  let ( = ) = equal_biome
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

let prepare_mid side =
  Tale.block "Prepare mid biomes" ~f:(fun () ->
      (* Start with a point cloud then subdivide a couple times *)
      let r = 16 in
      let cloud =
        Point_cloud.init ~side:(side / r) ~spacing:32 (fun _x _y ->
            match Random.int 10 with
            | 0 | 1 | 2 | 3 | 4 ->
                Plain (random_flower ())
            | 5 | 6 | 7 ->
                Forest (random_flower ())
            | 8 | _ ->
                Desert (random_cactus ()))
      in
      Tale.log "Init" ;
      let empty_val = Plain {block= Minecraft.Block.Air; percentage= 0} in
      let grid =
        Grid.Mut.init ~side:(side / r) ~alloc_side:side empty_val
          ~f:(fun ~x ~z ->
            Point_cloud.nearest cloud (float_of_int x) (float_of_int z))
      in
      let fill =
        let open Fill in
        line () **> random
      in
      for i = 1 to 4 do
        Tale.logf "Subdivide %d" i ;
        Subdivide_mut.subdivide_with_fill grid ~fill
      done ;
      grid)

let prepare_shore side =
  Tale.block "Prepare shore biomes" ~f:(fun () ->
      (* Start with a point cloud then subdivide a couple times *)
      let r = 16 in
      let cloud =
        Point_cloud.init ~side:(side / r) ~spacing:32 (fun _x _y ->
            match Random.int 3 with 0 -> Sand | 1 -> Gravel | _ -> Clay)
      in
      Tale.log "Init" ;
      let grid =
        Grid.Mut.init ~side:(side / r) ~alloc_side:side Sand ~f:(fun ~x ~z ->
            Point_cloud.nearest cloud (float_of_int x) (float_of_int z))
      in
      let fill =
        let open Fill in
        line () **> random
      in
      for i = 1 to 4 do
        Tale.logf "Subdivide %d" i ;
        Subdivide_mut.subdivide_with_fill grid ~fill
      done ;
      assert (Grid.Mut.side grid = side) ;
      grid)

let prepare_high side =
  Tale.block "Init high biomes" ~f:(fun () ->
      (* Start with a point cloud then subdivide a couple times *)
      let r = 16 in
      let cloud =
        Point_cloud.init ~side:(side / r) ~spacing:32 (fun _x _y ->
            match Random.int 3 with 0 -> Pine_forest | 1 -> Barren | _ -> Snow)
      in
      let grid =
        Grid.Mut.init ~side:(side / r) ~alloc_side:side Pine_forest
          ~f:(fun ~x ~z ->
            Point_cloud.nearest cloud (float_of_int x) (float_of_int z))
      in
      let fill =
        let open Fill in
        line () **> random
      in
      for i = 1 to 4 do
        Tale.logf "Subdivide %d" i ;
        Subdivide_mut.subdivide_with_fill grid ~fill
      done ;
      grid)

let zip_biomes (base : Base_overlay.tile Grid.t) ~mid ~shore ~high =
  Biome_grid.init ~side:base.Grid.side (fun (x, z) ->
      let here_base = Grid.get x z base in
      let elevation = here_base.elevation in
      if
        here_base.river || here_base.ocean
        || elevation <= Heightmap.sea_level + 2
      then Shore (Grid.Mut.get ~x ~z shore)
      else if elevation >= Heightmap.mountain_level - 20 then
        (* TODO use some sort of interp for mid-high cutoff, instead of flat line *)
        High (Grid.Mut.get ~x ~z high)
      else Mid (Grid.Mut.get ~x ~z mid))

let get_obstacle dirt biome =
  match biome with
  | Mid (Desert _) -> (
    match dirt = 0 with true -> Canonical_overlay.Impassable | false -> Clear )
  | _ ->
      Clear

let prepare () =
  let base, _ = Base_overlay.require () in
  let dirt = Dirt_overlay.require () in
  let mid = prepare_mid base.side in
  let shore = prepare_shore base.side in
  let high = prepare_high base.side in
  let biomes =
    let open Phase_chain in
    run_all (phase "Zip biomes" (fun () -> zip_biomes base ~mid ~shore ~high))
  in
  let biome_obstacles =
    Canonical_overlay.Obstacles.zip_map dirt biomes ~f:get_obstacle
  in
  let canond =
    Canonical_overlay.make_delta ~obstacles:(`Add biome_obstacles) ()
  in
  (biomes, canond)

let colorize (biome, base) =
  if base.River.Tile.river || base.River.Tile.ocean then River.colorize base
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
  Progress_view.save ~side "biome" ;
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
    (args : Minecraft_converter.region_args) =
  let region = args.region in
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
          done)

let apply_region state (args : Minecraft_converter.region_args) =
  let dirt = Dirt_overlay.require () in
  apply_dirt dirt state args

let require, prepare, apply =
  Overlay.make_no_canon "biome" prepare ~apply_progress_view apply_region
    bin_reader_t bin_writer_t
