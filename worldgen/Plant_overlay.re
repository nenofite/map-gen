type t = unit;

let prepare = () => ();

let apply_trees =
    (biomes: Biome_overlay.t, args: Minecraft_converter.region_args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  let trees =
    Point_cloud.init(~width=gsize, ~height=gsize, ~spacing=8, (_, _) =>
      Random.int(100) < 67
    );
  /* Try placing them where it's forest && solid ground */
  List.iter(
    (Point_cloud.{x, y: z, value}) =>
      if (value) {
        let x = int_of_float(x) + gx_offset;
        let z = int_of_float(z) + gy_offset;
        switch (Grid.at(biomes, x, z)) {
        | Mid(Forest) =>
          let y = Minecraft.Region.height_at(region, ~x, ~z);
          let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
          switch (block) {
          | Grass_block =>
            Minecraft.Template.place(
              Minecraft.Template_data.tree(),
              region,
              ~x,
              ~y=y + 1,
              ~z,
            )
            |> ignore
          | _ => ()
          };
        | _ => ()
        };
      },
    trees.points,
  );
};

let apply_ground_cover =
    (biomes: Biome_overlay.t, args: Minecraft_converter.region_args) => {
  let region = args.region;
  /* Change dirt => grass and add snow */
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let y = height_at(region, ~x, ~z);
      let top = get_block(region, ~x, ~y, ~z);
      let biome = Grid.at(biomes, x, z);
      switch (biome, top) {
      | (Mid(Plain | Forest) | High(Pine_forest), Dirt) =>
        set_block(~x, ~y, ~z, Grass_block, region)
      | (High(Snow), _) => set_block(~x, ~y=y + 1, ~z, Snow, region)
      | (_, _) => ()
      };
    },
  );
};

let apply_tallgrass =
    (biomes: Biome_overlay.t, args: Minecraft_converter.region_args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  let tallgrass =
    Point_cloud.init(~width=gsize, ~height=gsize, ~spacing=2, (_, _) =>
      Random.int(100) < 67
    );
  List.iter(
    (Point_cloud.{x, y: z, value}) =>
      if (value) {
        let x = int_of_float(x) + gx_offset;
        let z = int_of_float(z) + gy_offset;
        switch (Grid.at(biomes, x, z)) {
        | Mid(Forest | Plain) =>
          /* TODO should pine forests have tallgrass? */
          let y = Minecraft.Region.height_at(region, ~x, ~z);
          let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
          switch (block) {
          | Grass_block =>
            Minecraft.Region.set_block(
              ~x,
              ~y=y + 1,
              ~z,
              Minecraft.Block.Grass,
              region,
            )
          | _ => ()
          };
        | _ => ()
        };
      },
    tallgrass.points,
  );
};

let apply_region = (biomes, (), args: Minecraft_converter.region_args) => {
  apply_ground_cover(biomes, args);
  apply_trees(biomes, args);
  apply_tallgrass(biomes, args);
};

let overlay = (biomes: Biome_overlay.t) =>
  Overlay.make("plant", prepare, apply_region(biomes));