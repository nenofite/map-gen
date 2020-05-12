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
 */
let segment_grid_by_region =
    (block_per_tile: int, world: Grid.t('a), f): unit => {
  let block_per_region = Minecraft.Block_tree.block_per_region;
  /* block_per_tile must divide 512 evenly */
  assert(block_per_region mod block_per_tile == 0);
  /* The grid must split evenly into regions */
  if (world.width
      * block_per_tile
      mod block_per_region != 0
      || world.height
      * block_per_tile
      mod block_per_region != 0) {
    let msg =
      Printf.sprintf(
        "grid does not divide evenly into regions with block_per_tile=%d and grid %dx%d",
        block_per_tile,
        world.width,
        world.height,
      );
    raise(Invalid_argument(msg));
  };
  /* Calculate how many tiles form the side of a region */
  let tile_per_region = block_per_region / block_per_tile;
  let regions_width = world.width / tile_per_region;
  let regions_height = world.height / tile_per_region;
  Printf.printf(
    "Grid divided into %dx%d regions\n",
    regions_width,
    regions_height,
  );
  /* Iterate over the grid */
  for (rx in 0 to pred(regions_width)) {
    for (rz in 0 to pred(regions_height)) {
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

/** save_region creates a region, fills it with blocks from the grid, and saves */
let save_region =
    (
      ~region_path: string,
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
  let tree = Minecraft.Block_tree.create();
  for (gx in gx_offset to pred(gx_offset + gsize)) {
    for (gy in gy_offset to pred(gy_offset + gsize)) {
      let here = Grid.at(world, gx, gy);
      let elevation = scale_elevation(here.elevation);
      /* TODO apply scale */
      let (x, z) = xz_of_gxy(~block_per_tile, gx, gy);
      set_block(tree, x, 0, z, Minecraft.Block.Bedrock);

      for (y in 1 to elevation) {
        set_block(tree, x, y, z, Minecraft.Block.Dirt);
      };
      if (here.ocean) {
        for (y in elevation + 1 to sea_level) {
          set_block(tree, x, y, z, Minecraft.Block.Water);
        };
      } else if (Option.is_some(here.river)) {
        set_block(tree, x, elevation, z, Minecraft.Block.Water);
      };
    };
  };
  Minecraft.Block_tree.save_region(region_path, tree, rx, rz);
};

/** save creates a Minecraft world with the given heightmap */
let save = (world: Grid.t(River.tile)): unit => {
  let world_config =
    Minecraft.World.{
      name: "heightmap",
      spawn: (0, 0, 0),
      generator: Minecraft.Generator.Flat,
    };

  let region_path = Minecraft.World.save(world_config);

  segment_grid_by_region(1, world, save_region(~region_path));
};