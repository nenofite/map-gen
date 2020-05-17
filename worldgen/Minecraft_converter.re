let scale_elevation = elevation => {
  /* Take -30 to 100 and map to 1 to 100 */
  (elevation + 30) * 99 / 130 + 1;
};
let sea_level = scale_elevation(0);

/**
  xz_of_gxy is for use within callbacks passed to segment_grid_by_region. It
  converts the grid coordinates to region-local block coordinates.
 */
let xz_of_gxy = (gx, gy) => {
  /* Truncate to region-local coordinates. We know the coords are >= 0 so we can use mod */
  let x = gx mod Minecraft.Block_tree.block_per_region;
  let z = gy mod Minecraft.Block_tree.block_per_region;
  (x, z);
};

/**
  segment_grid_by_region divides the grid into squares such that each square
  becomes one Minecraft Anvil region. Then it calls f with the region
  coordinates, grid offset, and grid size.

  If the grid does not divide evenly into regions, then this raises
  Invalid_argument. To avoid this, the each grid dimension must divide evenly
  by 512.

  sub is an optional range of regions to limit the export, eg. ((2, 3), (1,
  1)) to export a 1x1 slice of regions starting with region r.2.3
 */
let segment_grid_by_region = (world: Grid.t('a), ~sub=?, f): unit => {
  let block_per_region = Minecraft.Block_tree.block_per_region;
  /* The grid must split evenly into regions */
  if (world.side mod block_per_region != 0) {
    let msg =
      Printf.sprintf(
        "grid does not divide evenly into regions with grid %dx%d",
        world.side,
        world.side,
      );
    raise(Invalid_argument(msg));
  };
  /* Calculate how many tiles form the side of a region */
  let regions_side = world.side / block_per_region;
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
          ~gx_offset=rx * block_per_region,
          ~gy_offset=rz * block_per_region,
          ~gsize=block_per_region,
        );
      };
    };
  };
};

let iter_blocks = (~rx, ~rz, ~gx_offset, ~gy_offset, ~gsize, f) => {
  for (gx in gx_offset to pred(gx_offset + gsize)) {
    for (gy in gy_offset to pred(gy_offset + gsize)) {
      let (x, z) = xz_of_gxy(gx, gy);
      f(~gx, ~gy, ~x, ~z);
    };
  };
};

/** save_region creates a region, fills it with blocks from the grid, and saves */
let save_region =
    (
      ~region_path: string,
      ~region,
      ~dirt,
      ~overlays,
      ~world: Grid.t(River.tile),
      ~rx,
      ~rz,
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
  iter_blocks(
    ~rx,
    ~rz,
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      let here = Grid.at(world, gx, gy);
      let elevation = scale_elevation(here.elevation);
      let dirt_height = Grid.at(dirt, gx, gy);

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
        set_block(region, x, elevation, z, Minecraft.Block.Flowing_water(0));
      } else if (dirt_height > 0) {
        set_block(region, x, elevation, z, Minecraft.Block.Grass);
      };
    },
  );

  List.iter(
    ov =>
      Overlay.apply_overlay(
        ov,
        ~region,
        ~rx,
        ~rz,
        ~gx_offset,
        ~gy_offset,
        ~gsize,
      ),
    overlays,
  );

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
let save =
    (
      world: Grid.t(Sites.tile),
      ~dirt: Grid.t(int),
      ~overlays: list(Overlay.overlay_state),
    )
    : unit => {
  let world_config =
    Minecraft.World.{
      name: "heightmap",
      spawn: (0, 0, 0),
      generator: Minecraft.Generator.Flat,
    };

  let region_path = Minecraft.World.save(world_config);
  let region = Minecraft.Block_tree.create();
  segment_grid_by_region(
    world,
    ~sub=((3, 4), (2, 2)),
    save_region(~region_path, ~region, ~dirt, ~overlays),
  );
};