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
  Sparse_grid.iter(trees.points, (_, Point_cloud.{px: x, py: z, value}) =>
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
    }
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

let place_flower = (~x, ~z, flower_block, region) => {
  let y = Minecraft.Region.height_at(region, ~x, ~z);
  let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
  switch (block) {
  | Grass_block =>
    Minecraft.Region.set_block(~x, ~y=y + 1, ~z, flower_block, region)
  | _ => ()
  };
};

let apply_flowers =
    (
      biomes: Grid.t(Biome_overlay.biome),
      args: Minecraft_converter.region_args,
    ) => {
  open Core_kernel;
  let region = args.region;
  let (x_off, z_off) = Minecraft.Region.region_offset(region);
  let cluster_centers =
    Point_cloud.init(
      ~width=Minecraft.Region.block_per_region_side,
      ~height=Minecraft.Region.block_per_region_side,
      ~spacing=15,
      (lx, lz) => {
        let (x, z) = (lx + x_off, lz + z_off);
        switch (Grid_compat.at(biomes, x, z)) {
        | Mid(Plain(flower) | Forest(flower))
            when Random.int(100) < flower.percentage =>
          Some(flower)
        | _ => None
        };
      },
    );
  let flower_coords =
    Point_cloud.make_int_list(
      ~width=Minecraft.Region.block_per_region_side,
      ~height=Minecraft.Region.block_per_region_side,
      ~spacing=3,
      (),
    );
  List.iter(
    flower_coords,
    ~f=((lx, lz)) => {
      let (x, z) = (lx + x_off, lz + z_off);
      switch (Grid_compat.at(biomes, x, z)) {
      | Mid(Plain(biome_flower) | Forest(biome_flower)) =>
        switch (
          Point_cloud.closest_point(
            cluster_centers,
            Float.of_int(lx),
            Float.of_int(lz),
          )
        ) {
        | (Point_cloud.{value: Some(cluster), _}, cluster_dist2) =>
          /* TODO vary cluster size */
          let cluster_size = 5;
          let perc =
            Float.(100. * cluster_dist2 |> to_int) / Int.(cluster_size ** 2);
          if (Minecraft.Block.equal_material(
                biome_flower.block,
                cluster.block,
              )
              && Random.int(100) < perc) {
            place_flower(~x, ~z, cluster.block, region);
          };
        | _ => ()
        }
      | _ => ()
      };
    },
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
  Sparse_grid.iter(tallgrass.points, (_, Point_cloud.{px: x, py: z, value}) =>
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
    }
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