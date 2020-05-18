type biome =
  | Plain
  | Forest;

type t = {biomes: Grid.t(biome)};

let prepare = (side, ()) => {
  /* Start with a point cloud then subdivide a couple times */
  let r = 16;
  let cloud =
    Point_cloud.init(~width=side / r, ~height=side / r, ~spacing=32, (_x, _y) =>
      Random.int(100) < 50 ? Forest : Plain
    );

  let biomes =
    Phase_chain.(
      run_all(
        phase("Init", () =>
          Grid.init(side / r, (x, y) =>
            Point_cloud.nearest(cloud, float_of_int(x), float_of_int(y))
          )
        )
        @> phase_repeat(
             4,
             "Subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> random)),
           ),
        /* @> Draw.phase(
             "biome.ppm",
             fun
             | Forest => 0x4F8D26
             | Plain => 0x77543D,
           ), */
      )
    );
  {biomes: biomes};
};

let apply_region =
    (base: Grid.t(River.tile), state, args: Minecraft_converter.region_args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  /* Create a point cloud of trees */
  let trees =
    Point_cloud.init(~width=gsize, ~height=gsize, ~spacing=8, (_, _) =>
      Random.int(100) < 67
    );
  let tallgrass =
    Point_cloud.init(~width=gsize, ~height=gsize, ~spacing=2, (_, _) =>
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
        let here_biome = Grid.at(state.biomes, gx, gy);
        let here_base = Grid.at(base, gx, gy);
        if (here_biome == Forest && !here_base.ocean && !here_base.river) {
          let y = Minecraft.Block_tree.height_at(region, x, z);
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
        };
      },
    trees.points,
  );
  List.iter(
    (Point_cloud.{x, y: z, value}) =>
      if (value) {
        let x = int_of_float(x);
        let z = int_of_float(z);
        let gx = x + gx_offset;
        let gy = z + gy_offset;
        let here_base = Grid.at(base, gx, gy);
        if (!here_base.ocean && !here_base.river) {
          let y = Minecraft.Block_tree.height_at(region, x, z);
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
        };
      },
    tallgrass.points,
  );
};

let overlay = (base: Grid.t(River.tile)) =>
  Overlay.make("biome", prepare(base.side), apply_region(base));