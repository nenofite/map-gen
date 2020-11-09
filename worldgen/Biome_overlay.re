open Core_kernel;

[@deriving (eq, bin_io)]
type flower = {
  block: Minecraft.Block.material,
  percentage: int,
};

[@deriving (eq, bin_io)]
type mid_biome =
  | Plain(flower)
  | Forest(flower)
  | Desert;

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
type t = (Grid.t(biome), Canonical_overlay.t);

let colorize =
  fun
  | Mid(Plain(_)) => 0x86A34D
  | Mid(Forest(_)) => 0x388824
  | Mid(Desert) => 0xD9D0A1
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

let prepare_mid = side => {
  /* Start with a point cloud then subdivide a couple times */
  let r = 16;
  let cloud =
    Point_cloud.init(~width=side / r, ~height=side / r, ~spacing=32, (_x, _y) =>
      switch (/* TODO */ Caml.Random.int(10)) {
      | 0
      | 1
      | 2
      | 3
      | 4 => Plain(random_flower())
      | 5
      | 6
      | 7 => Forest(random_flower())
      | 8
      | _ => Desert
      }
    );

  Phase_chain.(
    run_all(
      phase("Init mid biomes", () =>
        Grid_compat.init(side / r, (x, y) =>
          Point_cloud.nearest(cloud, float_of_int(x), float_of_int(y))
        )
      )
      @> phase_repeat(
           4,
           "Subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
         ),
    )
  );
};

let prepare_shore = side => {
  /* Start with a point cloud then subdivide a couple times */
  let r = 16;
  let cloud =
    Point_cloud.init(~width=side / r, ~height=side / r, ~spacing=32, (_x, _y) =>
      switch (/* TODO */ Caml.Random.int(3)) {
      | 0 => Sand
      | 1 => Gravel
      | _ => Clay
      }
    );

  Phase_chain.(
    run_all(
      phase("Init shore biomes", () =>
        Grid_compat.init(side / r, (x, y) =>
          Point_cloud.nearest(cloud, float_of_int(x), float_of_int(y))
        )
      )
      @> phase_repeat(
           4,
           "Subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
         ),
    )
  );
};

let prepare_high = side => {
  /* Start with a point cloud then subdivide a couple times */
  let r = 16;
  let cloud =
    Point_cloud.init(~width=side / r, ~height=side / r, ~spacing=32, (_x, _y) =>
      switch (/* TODO */ Caml.Random.int(3)) {
      | 0 => Pine_forest
      | 1 => Barren
      | _ => Snow
      }
    );

  Phase_chain.(
    run_all(
      phase("Init high biomes", () =>
        Grid_compat.init(side / r, (x, y) =>
          Point_cloud.nearest(cloud, float_of_int(x), float_of_int(y))
        )
      )
      @> phase_repeat(
           4,
           "Subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
         ),
    )
  );
};

let zip_biomes =
    (base: Grid_compat.t(Base_overlay.tile), ~mid, ~shore, ~high) => {
  let all_biomes = Grid_compat.zip(mid, Grid_compat.zip(shore, high));
  Grid_compat.zip_map(
    base,
    all_biomes,
    (base, (mid, (shore, high))) => {
      let elevation = base.elevation;
      if (base.river || base.ocean || elevation <= Heightmap.sea_level + 2) {
        Shore(shore);
      } else if (elevation >= Heightmap.mountain_level - 20) {
        /* TODO use some sort of interp for mid-high cutoff, instead of flat line */
        High(
          high,
        );
      } else {
        Mid(mid);
      };
    },
  );
};

let has_obstacle = (_base, dirt, biomes, x, y) => {
  switch (Grid_compat.at(biomes, x, y)) {
  | Mid(Desert) => Grid_compat.at(dirt, x, y) == 0
  | _ => false
  };
};

let prepare =
    (
      canon: Canonical_overlay.t,
      base: Grid_compat.t(Base_overlay.tile),
      dirt,
      (),
    ) => {
  let mid = prepare_mid(base.side);
  let shore = prepare_shore(base.side);
  let high = prepare_high(base.side);
  let biomes =
    Phase_chain.(
      run_all(
        phase("Zip biomes", () => zip_biomes(base, ~mid, ~shore, ~high))
        @> Draw.phase("biome.png", colorize),
      )
    );
  let canon = {
    ...canon,
    obstacles:
      Grid_compat.fold(biomes, canon.obstacles, (obstacles, x, y, _biome) =>
        if (has_obstacle(base, dirt, biomes, x, y)) {
          Sparse_grid.put(obstacles, x, y, ());
        } else {
          obstacles;
        }
      ),
  };
  (biomes, canon);
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
      | Mid(Desert) =>
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

let apply_region =
    (
      _base: Grid_compat.t(River.tile),
      dirt,
      state,
      args: Minecraft_converter.region_args,
    ) => {
  apply_dirt(dirt, state, args);
};

let overlay =
    (
      canon: Canonical_overlay.t,
      base: Grid_compat.t(River.tile),
      dirt: Grid_compat.t(int),
    )
    : Overlay.monad(t) =>
  Overlay.make(
    "biome",
    prepare(canon, base, dirt),
    apply_region(base, dirt),
    bin_reader_t,
    bin_writer_t,
  );