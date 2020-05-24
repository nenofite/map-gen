type tile = River.tile;

type t = Grid.t(tile);

let prepare = () =>
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      /* @> Draw.phase("grid-height.ppm", Heightmap.colorize) */
      @> River.phase
      @> Draw.phase("grid-river.ppm", River.colorize)
      @> Sites.phase,
      /* @> Draw.phase("grid-sites.ppm", River.colorize), */
    )
  );

let apply_region = (world: Grid.t(tile), args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      open Minecraft.Block_tree;

      let here = Grid.at(world, gx, gy);
      let elevation = here.elevation;

      set_block(region, x, 0, z, Minecraft.Block.Bedrock);

      for (y in 1 to elevation) {
        set_block(region, x, y, z, Stone);
      };
      if (here.ocean) {
        for (y in elevation to Heightmap.sea_level) {
          set_block(region, x, y, z, Minecraft.Block.Water);
        };
      } else if (here.river) {
        set_block(region, x, elevation, z, Minecraft.Block.Flowing_water(0));
      };
    },
  );
};

let overlay = Overlay.make("base", prepare, apply_region);