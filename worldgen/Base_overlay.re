type tile = River.tile;

type t = Grid.t(tile);

let apply_progress_view = world => {
  let layer = Progress_view.push_layer();
  Progress_view.update(
    ~draw_dense=Progress_view_helper.dense(River.colorize),
    ~state=world,
    layer,
  );
  ();
};

let prepare = () => {
  module Pvh = Progress_view_helper;
  let layer = Progress_view.push_layer();
  let s =
    Phase_chain.(
      run_all(
        Tectonic.phase
        @> Heightmap.phase
        @> Pvh.phase(~title="height", layer, Heightmap.colorize)
        @> Draw.phase("grid-height.png", Heightmap.colorize)
        @> River.phase
        @> Pvh.phase(~title="river", layer, River.colorize)
        @> Draw.phase("grid-river.png", River.colorize)
        @> Sites.phase
        @> Pvh.phase(~title="sites", layer, River.colorize)
        @> Draw.phase("grid-sites.png", River.colorize),
      )
    );
  Progress_view.remove_layer(layer);
  s;
};

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

let overlay =
  Overlay.make("base", ~apply_progress_view, prepare, apply_region);