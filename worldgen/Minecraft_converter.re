/**
  region_params are the numerous arguments provided when generating a region
 */
type region_args = {
  region: Minecraft.Region.t,
  rx: int,
  rz: int,
  gx_offset: int,
  gy_offset: int,
  gsize: int,
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
  let block_per_region = Minecraft.Region.block_per_region_side;
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
  Tale.logf("Grid divided into %dx%d regions", regions_side, regions_side);
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
          /* TODO can probably get rid of these, since it's always 1:1 */
          ~gx_offset=rx * block_per_region,
          ~gy_offset=rz * block_per_region,
          ~gsize=block_per_region,
        );
      };
    };
  };
};

/** fills the region with blocks from the grid */
let convert_region =
    (~region, ~apply_overlays, ~rx, ~rz, ~gx_offset, ~gy_offset, ~gsize) => {
  let args = {region, rx, rz, gx_offset, gy_offset, gsize};
  apply_overlays(args);

  Tale.logf("Flowing water");
  flush(stdout);
  Minecraft.Water.flow_water(region);
};

/** save creates a Minecraft world with the given heightmap */
let save = (~side: int, ~apply_overlays: region_args => unit): unit => {
  Minecraft.World.make("heightmap", ~spawn=(2600, 120, 600), builder => {
    segment_grid_by_region(
      ~side,
      ~sub=((4, 1), (2, 2)),
      (~rx, ~rz, ~gx_offset, ~gy_offset, ~gsize) => {
      Minecraft.World.make_region(~rx, ~rz, builder, region => {
        convert_region(
          ~region,
          ~apply_overlays,
          ~rx,
          ~rz,
          ~gx_offset,
          ~gy_offset,
          ~gsize,
        )
      })
    })
  });
  ();
};

let iter_blocks = (r: Minecraft.Region.t, fn): unit => {
  open Minecraft.Region;
  let (x_off, z_off) = chunk_offset(~cx=0, ~cz=0, r);
  for (z in 0 to pred(block_per_region_side)) {
    for (x in 0 to pred(block_per_region_side)) {
      fn(~x=x + x_off, ~z=z + z_off);
    };
  };
};