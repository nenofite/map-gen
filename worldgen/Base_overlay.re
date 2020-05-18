type tile = River.tile;

/* TODO make this obselete */
let scale_elevation = elevation => {
  /* Take -30 to 100 and map to 1 to 100 */
  (elevation + 30) * 99 / 130 + 1;
};
let sea_level = scale_elevation(0);

let prepare = () =>
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> Draw.phase("grid-height.ppm", Heightmap.colorize)
      /* @@> Erosion.phase
         @@> draw_phase("grid-erosion.ppm", Erosion.colorize), */
      @> River.phase
      @> Draw.phase("grid-river.ppm", River.colorize)
      @> Sites.phase
      @> Draw.phase("grid-sites.ppm", River.colorize),
    )
  );

let apply_region = (dirt: Grid.t(int), world: Grid.t(tile), args) => {
  let Minecraft_converter.{region, rx, rz, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~rx,
    ~rz,
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      open Minecraft.Block_tree;

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
};

let overlay = dirt => Overlay.make("base", prepare, apply_region(dirt));