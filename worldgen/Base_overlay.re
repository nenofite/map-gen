type tile = River.tile;

type t = Grid.t(tile);

type preview_pair =
  | No_pair: preview_pair
  | Preview_pair(Grid.t('a), 'a => int): preview_pair;

let preview_init = () => {
  Progress_view.push_layer(
    ~draw_dense=
      (s, x, y) =>
        switch (s) {
        | Preview_pair(grid, colorizer) =>
          if (Grid.is_within(grid, x, y)) {
            let rgb = colorizer(Grid.at(grid, x, y));
            let r = (rgb land 0xFF0000) lsr 16;
            let g = (rgb land 0x00FF00) lsr 8;
            let b = rgb land 0x0000FF;
            Some((r, g, b));
          } else {
            None;
          }
        | No_pair => None
        },
    No_pair,
  );
};

let preview_phase = (~title=?, layer, colorizer) => {
  Phase_chain.phase("preview", grid => {
    Progress_view.update(~title?, Preview_pair(grid, colorizer), layer);
    grid;
  });
};

let prepare = () => {
  let layer = preview_init();
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> preview_phase(~title="height", layer, Heightmap.colorize)
      @> Draw.phase("grid-height.png", Heightmap.colorize)
      @> River.phase
      @> preview_phase(~title="river", layer, River.colorize)
      @> Draw.phase("grid-river.png", River.colorize)
      @> Sites.phase
      @> preview_phase(~title="sites", layer, River.colorize)
      @> Draw.phase("grid-sites.png", River.colorize),
    )
  );
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

let overlay = Overlay.make("base", prepare, apply_region);