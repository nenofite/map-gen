type mid_biome =
  | Plain
  | Forest
  | Desert;

type shore_biome =
  | Sand
  | Gravel
  | Clay
  | No_shore;

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
  | Shore(Clay) => 0x8E7256
  | Shore(No_shore) => 0; /* TODO */

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
      switch (Random.int(4)) {
      | 0 => Sand
      | 1 => Gravel
      | 2 => Clay
      | _ => No_shore
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
      if (elevation <= Heightmap.sea_level + 2) {
        /* TODO check for distance to any water instead */
        Shore(shore);
      } else if (elevation >= Heightmap.mountain_level - 40) {
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
      @> Draw.phase("biome.ppm", colorize),
    )
  );
};

/** overwrite_stone_air only sets the block if it is Stone or Air, to avoid overwriting rivers etc. */
let overwrite_stone_air = (region, x, y, z, block) =>
  switch (Minecraft.Block_tree.get_block_opt(region, x, y, z)) {
  | Some(Air)
  | Some(Stone) => Minecraft.Block_tree.set_block(region, x, y, z, block)
  | Some(_)
  | None => ()
  };

let apply_dirt =
    (dirt: Grid.t(int), state, args: Minecraft_converter.region_args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~gx_offset,
    ~gy_offset,
    ~gsize,
    (~gx, ~gy, ~x, ~z) => {
      open Minecraft.Block_tree;
      let elev = height_at(region, x, z);
      let dirt_depth = Grid.at(dirt, gx, gy);
      switch (Grid.at(state, gx, gy)) {
      | Mid(Plain)
      | Mid(Forest)
      | High(Pine_forest) =>
        /* Dirt and grass */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, y == elev ? Grass : Dirt);
        }
      | Mid(Desert) =>
        /* Sand with rocks sticking out */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, y <= elev - 2 ? Sand : Air);
        }
      | High(Barren) =>
        /* Gravel */
        let gravel_depth = max(0, dirt_depth - Dirt_overlay.max_depth + 2);
        for (y in elev - gravel_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Gravel);
        };
      | High(Snow) =>
        /* Dirt and snow */
        for (y in elev - dirt_depth + 1 to elev) {
          overwrite_stone_air(region, x, y, z, Dirt);
        };
        overwrite_stone_air(region, x, elev + 1, z, Snow_layer);
      | Shore(_) => () /* TODO shores */
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