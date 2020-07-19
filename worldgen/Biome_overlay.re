type mid_biome =
  | Plain
  | Forest
  | Desert;

type shore_biome =
  | Sand
  | Gravel
  | Clay;

type high_biome =
  | Pine_forest
  | Barren
  | Snow;

type biome =
  | Mid(mid_biome)
  | Shore(shore_biome)
  | High(high_biome);

type t = Grid.t(biome);

let colorize =
  fun
  | Mid(Plain) => 0x86A34D
  | Mid(Forest) => 0x388824
  | Mid(Desert) => 0xD9D0A1
  | High(Pine_forest) => 0x286519
  | High(Barren) => 0x727272
  | High(Snow) => 0xFDFDFD
  | Shore(Sand) => 0xF5EAB7
  | Shore(Gravel) => 0x828282
  | Shore(Clay) => 0x8E7256;

let prepare_mid = side => {
  /* Start with a point cloud then subdivide a couple times */
  let r = 16;
  let cloud =
    Point_cloud.init(~width=side / r, ~height=side / r, ~spacing=32, (_x, _y) =>
      switch (Random.int(10)) {
      | 0
      | 1
      | 2
      | 3
      | 4 => Plain
      | 5
      | 6
      | 7 => Forest
      | 8
      | _ => Desert
      }
    );

  Phase_chain.(
    run_all(
      phase("Init mid biomes", () =>
        Grid.init(side / r, (x, y) =>
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
      switch (Random.int(3)) {
      | 0 => Sand
      | 1 => Gravel
      | _ => Clay
      }
    );

  Phase_chain.(
    run_all(
      phase("Init shore biomes", () =>
        Grid.init(side / r, (x, y) =>
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
      switch (Random.int(3)) {
      | 0 => Pine_forest
      | 1 => Barren
      | _ => Snow
      }
    );

  Phase_chain.(
    run_all(
      phase("Init high biomes", () =>
        Grid.init(side / r, (x, y) =>
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

let zip_biomes = (base: Grid.t(Base_overlay.tile), ~mid, ~shore, ~high) => {
  let all_biomes = Grid.zip(mid, Grid.zip(shore, high));
  Grid.zip_map(
    base,
    all_biomes,
    (_x, _y, base, (mid, (shore, high))) => {
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

let prepare = (base: Grid.t(Base_overlay.tile), ()) => {
  let mid = prepare_mid(base.side);
  let shore = prepare_shore(base.side);
  let high = prepare_high(base.side);
  Phase_chain.(
    run_all(
      phase("Zip biomes", () => zip_biomes(base, ~mid, ~shore, ~high))
      @> Draw.phase("biome.png", colorize),
    )
  );
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
    (dirt: Grid.t(int), state, args: Minecraft_converter.region_args) => {
  let region = args.region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let elev = height_at(~x, ~z, region);
      let dirt_depth = Grid.at(dirt, x, z);
      switch (Grid.at(state, x, z)) {
      | Mid(Plain)
      | Mid(Forest)
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
      _base: Grid.t(River.tile),
      dirt,
      state,
      args: Minecraft_converter.region_args,
    ) => {
  apply_dirt(dirt, state, args);
};

let overlay = (base: Grid.t(River.tile), dirt: Grid.t(int)) =>
  Overlay.make("biome", prepare(base), apply_region(base, dirt));