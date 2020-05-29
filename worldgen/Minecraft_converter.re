/**
  region_params are the numerous arguments provided when generating a region
 */
type region_args = {
  region: Minecraft.Block_tree.t,
  rx: int,
  rz: int,
  gx_offset: int,
  gy_offset: int,
  gsize: int,
};

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
let segment_grid_by_region = (~side: int, ~sub=?, f): unit => {
  let block_per_region = Minecraft.Block_tree.block_per_region;
  /* The grid must split evenly into regions */
  if (side mod block_per_region != 0) {
    let msg =
      Printf.sprintf(
        "grid does not divide evenly into regions with grid %dx%d",
        side,
        side,
      );
    raise(Invalid_argument(msg));
  };
  /* Calculate how many tiles form the side of a region */
  let regions_side = side / block_per_region;
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

let iter_blocks = (~gx_offset, ~gy_offset, ~gsize, f) => {
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
      ~apply_overlays,
      ~rx,
      ~rz,
      ~gx_offset,
      ~gy_offset,
      ~gsize,
    ) => {
  Printf.printf("Creating region (%d, %d)\n", rx, rz);
  let start_time = Minecraft.Utils.time_ms();
  Printf.printf("Resetting region\n");
  Minecraft.Block_tree.reset(region, ~rx, ~rz);

  let args = {region, rx, rz, gx_offset, gy_offset, gsize};
  apply_overlays(args);

  Printf.printf("Flowing water\n");
  Minecraft.Water.flow_water(region);
  Printf.printf("Saving region\n");
  Minecraft.Block_tree.save_region(region_path, region);
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
let save = (~side: int, ~apply_overlays: region_args => unit): unit => {
  let world_config =
    Minecraft.World.{
      name: "heightmap",
      spawn: (2600, 120, 600),
      generator: Minecraft.Generator.Flat,
    };

  let region_path = Minecraft.World.save(world_config);
  /*
   * the rx and rz will be given when we reset the region, so no need to set
   * them to anything meaningful
   */
  let region = Minecraft.Block_tree.create(~rx=0, ~rz=0);
  segment_grid_by_region(
    ~side,
    ~sub=((4, 1), (2, 2)),
    save_region(~region_path, ~region, ~apply_overlays),
  );
};