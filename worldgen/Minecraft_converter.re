let scale_elevation = elevation => {
  /* Take -30 to 100 and map to 1 to 100 */
  (elevation + 30) * 99 / 130 + 1;
};
let sea_level = scale_elevation(0);

/**
  xz_of_gxy is for use within callbacks passed to segment_grid_by_region. It
  converts the grid coordinates to region-local block coordinates.
 */
let xz_of_gxy = (~block_per_tile, gx, gy) => {
  /* Convert to global block coordinates */
  let x = gx * block_per_tile;
  let z = gy * block_per_tile;
  /* Truncate to region-local coordinates. We know the coords are >= 0 so we can use mod */
  let x = x mod Minecraft.Block_tree.block_per_region;
  let z = z mod Minecraft.Block_tree.block_per_region;
  (x, z);
};

/**
  segment_grid_by_region divides the grid into squares such that each square
  becomes one Minecraft Anvil region. Then it calls f with the region
  coordinates, grid offset, and grid size.

  block_per_tile specifies the ratio from grid tiles to Minecraft blocks. For
  example, if it is 2, then each grid tile will become 2x2 blocks. A region
  contains 512x512 blocks horizontally, so in this case the region would
  contain 256x256 grid tiles. Because it must divide 512 evenly, it must be a
  power of 2.

  If the grid does not divide evenly into regions, then this raises
  Invalid_argument. To avoid this, the each grid dimension times
  block_per_tile must divide evenly by 512.

  sub is an optional range of regions to limit the export, eg. ((2, 3), (1,
  1)) to export a 1x1 slice of regions starting with region r.2.3
 */
let segment_grid_by_region =
    (block_per_tile: int, world: Grid.t('a), ~sub=?, f): unit => {
  let block_per_region = Minecraft.Block_tree.block_per_region;
  /* block_per_tile must divide 512 evenly */
  assert(block_per_region mod block_per_tile == 0);
  /* The grid must split evenly into regions */
  if (world.side * block_per_tile mod block_per_region != 0) {
    let msg =
      Printf.sprintf(
        "grid does not divide evenly into regions with block_per_tile=%d and grid %dx%d",
        block_per_tile,
        world.side,
        world.side,
      );
    raise(Invalid_argument(msg));
  };
  /* Calculate how many tiles form the side of a region */
  let tile_per_region = block_per_region / block_per_tile;
  let regions_side = world.side / tile_per_region;
  Printf.printf(
    "Grid divided into %dx%d regions\n",
    regions_side,
    regions_side,
  );
  let ((min_rx, min_rz), (len_rx, len_rz)) =
    Option.value(sub, ~default=((0, 0), (regions_side, regions_side)));
  /* Iterate over the grid */
  for (rx in 0 to pred(regions_side)) {
    for (rz in 0 to pred(regions_side)) {
      let in_sub =
        min_rx <= rx
        && rx < min_rx
        + len_rx
        && min_rz <= rz
        && rz < min_rz
        + len_rz;
      if (in_sub) {
        f(
          ~world,
          ~rx,
          ~rz,
          ~block_per_tile,
          ~gx_offset=rx * tile_per_region,
          ~gy_offset=rz * tile_per_region,
          ~gsize=tile_per_region,
        );
      };
    };
  };
};

/** save_region creates a region, fills it with blocks from the grid, and saves */
let save_region =
    (
      ~region_path: string,
      ~region,
      ~dirt,
      ~world: Grid.t(River.tile),
      ~rx,
      ~rz,
      ~block_per_tile,
      ~gx_offset,
      ~gy_offset,
      ~gsize,
    ) => {
  let set_block = Minecraft.Block_tree.set_block;

  Printf.printf("Creating region (%d, %d)\n", rx, rz);
  let start_time = Minecraft.Utils.time_ms();
  Printf.printf("Resetting region\n");
  Minecraft.Block_tree.reset(region);
  Printf.printf("Filling blocks\n");
  for (gx in gx_offset to pred(gx_offset + gsize)) {
    for (gy in gy_offset to pred(gy_offset + gsize)) {
      let here = Grid.at(world, gx, gy);
      let elevation = scale_elevation(here.elevation);
      let dirt_height = Grid.at(dirt, gx, gy);
      let (x, z) = xz_of_gxy(~block_per_tile, gx, gy);
      for (x in x to pred(x + block_per_tile)) {
        for (z in z to pred(z + block_per_tile)) {
          set_block(region, x, 0, z, Minecraft.Block.Bedrock);

          for (y in 1 to elevation) {
            let material =
              y <= elevation - dirt_height
                ? Minecraft.Block.Stone : Minecraft.Block.Dirt;
            set_block(region, x, y, z, material);
          };
          if (here.ocean) {
            for (y in elevation to sea_level) {
              set_block(region, x, y, z, Minecraft.Block.Water);
            };
          } else if (here.river) {
            set_block(
              region,
              x,
              elevation,
              z,
              Minecraft.Block.Flowing_water(0),
            );
          } else if (dirt_height > 0) {
            set_block(region, x, elevation, z, Minecraft.Block.Grass);
          };
        };
      };
    };
  };
  Printf.printf("Flowing water\n");
  Minecraft.Water.flow_water(region);
  Printf.printf("Saving region\n");
  Minecraft.Block_tree.save_region(region_path, region, rx, rz);
  let elapsed_time =
    Int64.sub(Minecraft.Utils.time_ms(), start_time) |> Int64.to_float;
  Printf.printf("Finished (%d, %d) in %fs\n", rx, rz, elapsed_time /. 1000.);
};

let make_dirt_cloud = side => {
  Point_cloud.init(~width=side, ~height=side, ~spacing=20, (_x, _y) =>
    Random.int(8) |> float_of_int
  );
};

/** save creates a Minecraft world with the given heightmap */
let save = (world: Grid.t(Sites.tile), ~dirt: Grid.t(int)): unit => {
  let world_config =
    Minecraft.World.{
      name: "heightmap",
      spawn: (0, 0, 0),
      generator: Minecraft.Generator.Flat,
    };

  let region_path = Minecraft.World.save(world_config);
  let region = Minecraft.Block_tree.create();
  segment_grid_by_region(
    1,
    world,
    ~sub=((3, 4), (2, 2)),
    save_region(~region_path, ~region, ~dirt),
  );
};