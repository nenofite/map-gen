type tile = River.tile;

type t = Grid.t(tile);

let prepare = () =>
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> Draw.phase("grid-height.png", Heightmap.colorize)
      @> River.phase
      @> Draw.phase("grid-river.png", River.colorize)
      @> Sites.phase
      @> Draw.phase("grid-sites.png", River.colorize),
    )
  );

let apply_region =
    (world: Grid.t(tile), args: Minecraft_converter.region_args) => {
  let region = args.region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;

      let here = Grid.at(world, x, z);
      let elevation = here.elevation;

      set_block(~x, ~y=0, ~z, Minecraft.Block.Bedrock, region);

      for (y in 1 to elevation) {
        set_block(~x, ~y, ~z, Stone, region);
      };
      if (here.ocean) {
        for (y in elevation to Heightmap.sea_level) {
          set_block(~x, ~y, ~z, Minecraft.Block.Water, region);
        };
      } else if (here.river) {
        set_block(
          ~x,
          ~y=elevation,
          ~z,
          Minecraft.Block.Flowing_water(0),
          region,
        );
      };
    },
  );
};

let overlay = Overlay.make("base", prepare, apply_region);