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
        let x = int_of_float(x);
        let z = int_of_float(z);
        let gx = x + gx_offset;
        let gy = z + gy_offset;
        switch (Grid.at(biomes, gx, gy)) {
        | Mid(Forest) =>
          let y = Minecraft.Block_tree.height_at(region, ~x, ~z, ());
          let block = Minecraft.Block_tree.get_block(region, x, y, z);
          switch (block) {
          | Grass =>
            Minecraft.Template.place(
              Minecraft.Template_data.tree(),
              region,
              x,
              y + 1,
              z,
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
        let x = int_of_float(x);
        let z = int_of_float(z);
        let gx = x + gx_offset;
        let gy = z + gy_offset;
        switch (Grid.at(biomes, gx, gy)) {
        | Mid(Forest | Plain) =>
          /* TODO should pine forests have tallgrass? */
          let y = Minecraft.Block_tree.height_at(region, ~x, ~z, ());
          let block = Minecraft.Block_tree.get_block(region, x, y, z);
          switch (block) {
          | Grass =>
            Minecraft.Block_tree.set_block(
              region,
              x,
              y + 1,
              z,
              Minecraft.Block.Tallgrass,
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
  apply_trees(biomes, args);
  apply_tallgrass(biomes, args);
};

let overlay = (biomes: Biome_overlay.t) =>
  Overlay.make("plant", prepare, apply_region(biomes));