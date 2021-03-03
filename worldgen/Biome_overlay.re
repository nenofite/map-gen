open Core_kernel;

[@deriving (eq, bin_io)]
type flower = {
  block: Minecraft.Block.material,
  percentage: int,
};

[@deriving (eq, bin_io)]
type cactus = {percentage: int};

[@deriving (eq, bin_io)]
type mid_biome =
  | Plain(flower)
  | Forest(flower)
  | Desert(cactus);

[@deriving (eq, bin_io)]
type shore_biome =
  | Sand
  | Gravel
  | Clay;

[@deriving (eq, bin_io)]
type high_biome =
  | Pine_forest
  | Barren
  | Snow;

[@deriving (eq, bin_io)]
type biome =
  | Mid(mid_biome)
  | Shore(shore_biome)
  | High(high_biome);

[@deriving bin_io]
type t = (Grid.t(biome), Canonical_overlay.delta);

module Biome_grid =
  Grid.Make0({
    type t = biome;
    let (==) = equal_biome;
  });

let colorize_biome =
  fun
  | Mid(Plain(_)) => 0x86A34D
  | Mid(Forest(_)) => 0x388824
  | Mid(Desert(_)) => 0xD9D0A1
  | High(Pine_forest) => 0x286519
  | High(Barren) => 0x727272
  | High(Snow) => 0xFDFDFD
  | Shore(Sand) => 0xF5EAB7
  | Shore(Gravel) => 0x828282
  | Shore(Clay) => 0x8E7256;

let random_flower = () => {
  open Minecraft.Block;
  let block =
    switch (Random.int(12)) {
    | 0 => Dandelion
    | 1 => Poppy
    | 2 => Blue_orchid
    | 3 => Allium
    | 4 => Azure_bluet
    | 5 => Red_tulip
    | 6 => Orange_tulip
    | 7 => White_tulip
    | 8 => Pink_tulip
    | 9 => Oxeye_daisy
    | 10 => Cornflower
    | 11
    | _ => Lily_of_the_valley
    /* TODO tall flowers */
    };
  {block, percentage: Random.int_incl(10, 100)};
};

let random_cactus = () => {percentage: Random.int_incl(10, 100)};

let prepare_mid = side =>
  Tale.block("Prepare mid biomes", ~f=() => {
    /* Start with a point cloud then subdivide a couple times */
    let r = 16;
    let cloud =
      Point_cloud.init(~side=side / r, ~spacing=32, (_x, _y) =>
        switch (Random.int(10)) {
        | 0
        | 1
        | 2
        | 3
        | 4 => Plain(random_flower())
        | 5
        | 6
        | 7 => Forest(random_flower())
        | 8
        | _ => Desert(random_cactus())
        }
      );

    Tale.log("Init");
    let empty_val = Plain({block: Minecraft.Block.Air, percentage: 0});
    let grid =
      Grid.Mut.init(~side=side / r, ~alloc_side=side, empty_val, ~f=(~x, ~z) => {
        Point_cloud.nearest(cloud, float_of_int(x), float_of_int(z))
      });
    let fill = Fill.(line() **> random);
    for (i in 1 to 4) {
      Tale.logf("Subdivide %d", i);
      Subdivide_mut.subdivide_with_fill(grid, ~fill);
    };
    grid;
  });

let prepare_shore = side =>
  Tale.block("Prepare shore biomes", ~f=() => {
    /* Start with a point cloud then subdivide a couple times */
    let r = 16;
    let cloud =
      Point_cloud.init(~side=side / r, ~spacing=32, (_x, _y) =>
        switch (Random.int(3)) {
        | 0 => Sand
        | 1 => Gravel
        | _ => Clay
        }
      );

    Tale.log("Init");
    let grid =
      Grid.Mut.init(~side=side / r, ~alloc_side=side, Sand, ~f=(~x, ~z) =>
        Point_cloud.nearest(cloud, float_of_int(x), float_of_int(z))
      );
    let fill = Fill.(line() **> random);
    for (i in 1 to 4) {
      Tale.logf("Subdivide %d", i);
      Subdivide_mut.subdivide_with_fill(grid, ~fill);
    };
    assert(Grid.Mut.side(grid) == side);
    grid;
  });

let prepare_high = side =>
  Tale.block("Init high biomes", ~f=() => {
    /* Start with a point cloud then subdivide a couple times */
    let r = 16;
    let cloud =
      Point_cloud.init(~side=side / r, ~spacing=32, (_x, _y) =>
        switch (Random.int(3)) {
        | 0 => Pine_forest
        | 1 => Barren
        | _ => Snow
        }
      );

    let grid =
      Grid.Mut.init(
        ~side=side / r, ~alloc_side=side, Pine_forest, ~f=(~x, ~z) => {
        Point_cloud.nearest(cloud, float_of_int(x), float_of_int(z))
      });
    let fill = Fill.(line() **> random);
    for (i in 1 to 4) {
      Tale.logf("Subdivide %d", i);
      Subdivide_mut.subdivide_with_fill(grid, ~fill);
    };
    grid;
  });

let zip_biomes = (base: Grid.t(Base_overlay.tile), ~mid, ~shore, ~high) => {
  Biome_grid.init(
    ~side=base.Grid.side,
    ((x, z)) => {
      let here_base = Grid.get(x, z, base);
      let elevation = here_base.elevation;
      if (here_base.river
          || here_base.ocean
          || elevation <= Heightmap.sea_level
          + 2) {
        Shore(Grid.Mut.get(~x, ~z, shore));
      } else if (elevation >= Heightmap.mountain_level - 20) {
        /* TODO use some sort of interp for mid-high cutoff, instead of flat line */
        High(
          Grid.Mut.get(~x, ~z, high),
        );
      } else {
        Mid(Grid.Mut.get(~x, ~z, mid));
      };
    },
  );
};

let get_obstacle = (dirt, biome) => {
  switch (biome) {
  | Mid(Desert(_)) => dirt == 0 ? Canonical_overlay.Impassable : Clear
  | _ => Clear
  };
};

let prepare = () => {
  let (base, _) = Base_overlay.require();
  let dirt = Dirt_overlay.require();

  let mid = prepare_mid(base.side);
  let shore = prepare_shore(base.side);
  let high = prepare_high(base.side);
  let biomes =
    Phase_chain.(
      run_all(
        phase("Zip biomes", () => zip_biomes(base, ~mid, ~shore, ~high)),
      )
    );
  let biome_obstacles =
    Canonical_overlay.Obstacles.zip_map(dirt, biomes, ~f=get_obstacle);
  let canond =
    Canonical_overlay.make_delta(~obstacles=`Add(biome_obstacles), ());
  (biomes, canond);
};

let colorize = ((biome, base)) =>
  if (base.River.Tile.river || base.River.Tile.ocean) {
    River.colorize(base);
  } else {
    let elev = base.River.Tile.elevation;
    let frac = float_of_int(elev) /. 200.;
    let frac = Float.(max(min(frac, 1.), 0.));
    let black = 0;
    let biome_col = colorize_biome(biome);
    Color.blend(black, biome_col, frac);
  };

let apply_progress_view = (state: t) => {
  let (base, _) = Base_overlay.require();
  let (biome, _canon) = state;
  let side = base.Grid.side;
  let layer = Progress_view.push_layer();
  let g = Grid_compat.zip(biome, base);
  Progress_view.update(
    ~fit=(0, side, 0, side),
    ~draw_dense=Progress_view_helper.dense(colorize),
    ~state=g,
    layer,
  );
  Progress_view.save(~side, "biome");
  ();
};

/** overwrite_stone_air only sets the block if it is Stone or Air, to avoid overwriting rivers etc. */
let overwrite_stone_air = (region, x, y, z, block) =>
  switch (Minecraft.Region.get_block_opt(region, ~x, ~y, ~z)) {
  | Some(Air)
  | Some(Stone) => Minecraft.Region.set_block(~x, ~y, ~z, block, region)
  | Some(_)
  | None => ()
  };

let apply_dirt =
    (
      dirt: Grid_compat.t(int),
      (state, _canon),
      args: Minecraft_converter.region_args,
    ) => {
  let region = args.region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let elev = height_at(~x, ~z, region);
      let dirt_depth = Grid_compat.at(dirt, x, z);
      switch (Grid_compat.at(state, x, z)) {
      | Mid(Plain(_) | Forest(_))
      | High(Pine_forest) =>
        /* Dirt (will become grass in Plant_overlay) */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Dirt);
        }
      | Mid(Desert(_)) =>
        /* Sand with rocks sticking out */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Sand);
        };
        if (dirt_depth == 0) {
          /* Add some rock sticking out */
          for (y in elev + 1 to elev + 2) {
            overwrite_stone_air(region, x, y, z, Stone);
          };
        };
      | High(Barren) =>
        /* Gravel */
        let gravel_depth = max(0, dirt_depth - Dirt_overlay.max_depth + 2);
        for (y in elev - gravel_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Gravel);
        };
      | High(Snow) =>
        /* Dirt (will add snow in Plant_overlay) */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Dirt);
        }
      | Shore(m) =>
        let material =
          switch (m) {
          | Sand => Minecraft.Block.Sand
          | Gravel => Minecraft.Block.Gravel
          | Clay => Minecraft.Block.Clay
          };
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, material);
        };
      };
    },
  );
};

let apply_region = (state, args: Minecraft_converter.region_args) => {
  let dirt = Dirt_overlay.require();
  apply_dirt(dirt, state, args);
};

let (require, prepare, apply) =
  Overlay.make(
    "biome",
    prepare,
    ~apply_progress_view,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );
