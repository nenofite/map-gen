open Bin_prot.Std;

[@deriving bin_io]
type t = unit;

let prepare = () => ();

let apply_trees =
    (
      biomes: Grid.t(Biome_overlay.biome),
      args: Minecraft_converter.region_args,
    ) => {
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
        switch (Grid_compat.at(biomes, x, z)) {
        | Mid(Forest(_)) =>
          let y = Minecraft.Region.height_at(region, ~x, ~z);
          let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
          switch (block) {
          | Grass_block =>
            Minecraft_template.place(
              Tree_template.tree(),
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
    (
      biomes: Grid.t(Biome_overlay.biome),
      args: Minecraft_converter.region_args,
    ) => {
  let region = args.region;
  /* Change dirt => grass and add snow */
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let y = height_at(region, ~x, ~z);
      let top = get_block(region, ~x, ~y, ~z);
      let biome = Grid_compat.at(biomes, x, z);
      switch (biome, top) {
      | (Mid(Plain(_) | Forest(_)) | High(Pine_forest), Dirt) =>
        set_block(~x, ~y, ~z, Grass_block, region)
      | (High(Snow), _) => set_block(~x, ~y=y + 1, ~z, Snow, region)
      | (_, _) => ()
      };
    },
  );
};

let apply_flowers =
    (
      biomes: Grid.t(Biome_overlay.biome),
      args: Minecraft_converter.region_args,
    ) => {
  let region = args.region;
  let flower_spots =
    Point_cloud.make_int_list(
      ~width=Minecraft.Region.block_per_region_side,
      ~height=Minecraft.Region.block_per_region_side,
      ~spacing=10,
      (),
    );
  let (x_off, z_off) = Minecraft.Region.region_offset(region);
  List.iter(
    ((lx, lz)) => {
      let (x, z) = (lx + x_off, lz + z_off);
      switch (Grid_compat.at(biomes, x, z)) {
      | Mid(Plain(flower) | Forest(flower))
          when Random.int(100) < flower.percentage =>
        let y = Minecraft.Region.height_at(region, ~x, ~z);
        let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
        switch (block) {
        | Grass_block =>
          Minecraft.Region.set_block(~x, ~y=y + 1, ~z, flower.block, region)
        | _ => ()
        };
      | _ => ()
      };
    },
    flower_spots,
  );
};

let apply_tallgrass =
    (
      biomes: Grid.t(Biome_overlay.biome),
      args: Minecraft_converter.region_args,
    ) => {
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
        switch (Grid_compat.at(biomes, x, z)) {
        | Mid(Forest(_) | Plain(_)) =>
          /* TODO should pine forests have tallgrass? */
          let y = Minecraft.Region.height_at(region, ~x, ~z);
          let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
          let block_above =
            Minecraft.Region.get_block(region, ~x, ~y=y + 1, ~z);
          switch (block, block_above) {
          | (Grass_block, Air) =>
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
  apply_flowers(biomes, args);
  apply_tallgrass(biomes, args);
};

let overlay = (biomes: Grid_compat.t(Biome_overlay.biome)): Overlay.monad(t) =>
  Overlay.make(
    "plant",
    prepare,
    apply_region(biomes),
    bin_reader_t,
    bin_writer_t,
  );