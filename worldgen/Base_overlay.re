open Core_kernel;

[@deriving bin_io]
type tile = River.tile;

[@deriving bin_io]
type t = (Grid.t(tile), Canonical_overlay.t);

type x = Grid.t(tile);

let apply_progress_view = (state: t) => {
  let (world, _canon) = state;
  let layer = Progress_view.push_layer();
  Progress_view.update(
    ~fit=(0, world.Grid.side, 0, world.Grid.side),
    ~draw_dense=Progress_view_helper.dense(River.colorize),
    ~state=world,
    layer,
  );
  ();
};

let obstacle_of_tile = tile => {
  Canonical_overlay.(
    if (tile.River.Tile.ocean) {
      Impassable;
    } else if (tile.river) {
      Bridgeable;
    } else {
      Clear;
    }
  );
};

let extract_canonical = (grid: Grid.t(tile)) =>
  Canonical_overlay.{
    side: grid.side,
    elevation: Grid_compat.map(grid, (_x, _y, tile) => tile.elevation),
    obstacles: Obstacles.map(grid, ~f=obstacle_of_tile),
    spawn_points: [],
  };

let prepare = (side, ()) => {
  module Pvh = Progress_view_helper.Make(Grid.Mut.Intf);
  let layer = Progress_view.push_layer();

  let grid = Tectonic.phase(side);
  let grid = Heightmap.phase(grid);
  Pvh.update_with_colorize(
    ~title="height",
    ~colorize=Heightmap.colorize,
    grid,
    layer,
  );
  let grid = River.phase(grid, ~alloc_side=Grid.Mut.side(grid) * 4);
  Pvh.update_with_colorize(
    ~title="river",
    ~colorize=River.colorize,
    grid,
    layer,
  );
  let grid = Sites.phase(grid);
  assert(Grid.Mut.side(grid) == side);
  Pvh.update_with_colorize(
    ~title="sites",
    ~colorize=River.colorize,
    grid,
    layer,
  );
  Progress_view.save(~side=Grid.Mut.side(grid), "grid-river");
  Progress_view.remove_layer(layer);
  let grid = River.Tile.Grid.of_mut(grid);
  (grid, extract_canonical(grid));
};

let apply_region = (state: t, args: Minecraft_converter.region_args) => {
  let (world, _canon) = state;
  let region = args.region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;

      let here = Grid_compat.at(world, x, z);
      let elevation = here.elevation;

      set_block(~x, ~y=0, ~z, Minecraft.Block.Bedrock, region);

      for (y in 1 to elevation) {
        set_block(~x, ~y, ~z, Stone, region);
      };
      if (here.ocean) {
        for (y in elevation + 1 to Heightmap.sea_level) {
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

let overlay = (side): Overlay.monad(t) =>
  Overlay.make(
    "base",
    ~apply_progress_view,
    prepare(side),
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );