open Bin_prot.Std;

[@deriving bin_io]
type t = unit;

let prepare = () => ();

let apply_trees = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  let trees =
    Point_cloud.init(
      ~side=Minecraft.Region.block_per_region_side, ~spacing=8, (_, _) =>
      Random.int(100) < 67
    );
  let (rx, rz) = Minecraft.Region.region_offset(region);
  /* Try placing them where it's forest && solid ground */
  Point_cloud.iter(
    trees,
    ~f=(~x, ~y as z, value) => {
      let x = int_of_float(x) + rx;
      let z = int_of_float(z) + rz;
      if (value && Minecraft.Region.is_within(~x, ~y=0, ~z, region)) {
        let y = Minecraft.Region.height_at(region, ~x, ~z);
        let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
        switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
        | Forest(_) =>
          switch (block) {
          | Dirt =>
            Minecraft_template.place(
              Oak_tree.random_tree(),
              region,
              ~x,
              ~y=y + 1,
              ~z,
            )
            |> ignore
          | _ => ()
          }
        | Pine_forest =>
          switch (block) {
          | Dirt =>
            Minecraft_template.place(
              Spruce_tree.random_tree(),
              region,
              ~x,
              ~y=y + 1,
              ~z,
            )
            |> ignore
          | _ => ()
          }
        | Ocean
        | Barren_mountain
        | Snow_mountain
        | Desert(_)
        | Plain(_)
        | Savanna
        | Shore
        | Stone_shore
        | River => ()
        };
      };
    },
  );
};

let is_opaque_or_water = b => Minecraft.Block.(is_wet(b) || is_opaque(b));

let should_add_snow_despite_obstacle: Minecraft.Block.material => bool =
  fun
  | Dirt
  | Grass => true
  | _ => false;

let apply_grass = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let y =
        Option.value(
          highest_such_block(region, ~x, ~z, is_opaque_or_water),
          ~default=0,
        );
      let top = get_block(region, ~x, ~y, ~z);
      let biome = Biome_overlay.biome_at(~x, ~z, biomes);
      switch (biome) {
      | Plain(_)
      | Forest(_)
      | Savanna
      | River
      | Pine_forest
      | Snow_mountain =>
        switch (top) {
        | Dirt => set_block(~x, ~y, ~z, Grass_block, region)
        | _ => ()
        }
      | Ocean
      | Barren_mountain
      | Desert(_)
      | Shore
      | Stone_shore => ()
      };
    },
  );
};

let apply_snow = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let y = height_at(region, ~x, ~z);
      let top = get_block(region, ~x, ~y, ~z);
      let biome = Biome_overlay.biome_at(~x, ~z, biomes);
      switch (biome) {
      | Snow_mountain =>
        if (Minecraft.Block.should_receive_snow(top)) {
          set_block(~x, ~y=y + 1, ~z, Snow, region);
        }
      | Plain(_)
      | Forest(_)
      | Savanna
      | River
      | Pine_forest
      | Ocean
      | Barren_mountain
      | Desert(_)
      | Shore
      | Stone_shore => ()
      };
    },
  );
};

let make_clusters = (~spacing, ~f, ~biomes: Biome_overlay.t', region) => {
  open Core_kernel;
  let (x_off, z_off) = Minecraft.Region.region_offset(region);
  Point_cloud.init(
    ~cover_edges=false,
    ~side=Minecraft.Region.block_per_region_side,
    ~spacing,
    (lx, lz) => {
      let (x, z) = (lx + x_off, lz + z_off);
      f(Biome_overlay.biome_at(~x, ~z, biomes));
    },
  );
};

let fill_clusters =
    (
      ~spacing,
      ~biomes: Biome_overlay.t',
      ~can_place,
      ~place,
      cluster_centers,
      region,
    ) => {
  open Core_kernel;
  let (x_off, z_off) = Minecraft.Region.region_offset(region);
  let coords =
    Point_cloud.make_int_list(
      ~side=Minecraft.Region.block_per_region_side,
      ~spacing,
      (),
    );
  List.iter(
    coords,
    ~f=((lx, lz)) => {
      let (x, z) = (lx + x_off, lz + z_off);
      switch (
        Point_cloud.closest_point(
          cluster_centers,
          Float.of_int(lx),
          Float.of_int(lz),
        )
      ) {
      | (Point_cloud.{value: Some(cluster), _}, cluster_dist) =>
        if (can_place(Biome_overlay.biome_at(~x, ~z, biomes), cluster)) {
          /* TODO vary cluster size */
          let cluster_size = 5;
          let perc = cluster_size * 100 / (1 + Int.of_float(cluster_dist));
          if (Random.int(100) < perc) {
            place(~x, ~z, cluster, region);
          };
        }
      | _ => ()
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

let apply_flowers = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  open Core_kernel;
  let region = region;
  let cluster_centers =
    make_clusters(
      ~spacing=15,
      ~biomes,
      region,
      ~f=
        fun
        | Plain(flower)
        | Forest(flower) when Random.int(100) < flower.percentage =>
          Some(flower)
        | _ => None,
    );
  fill_clusters(
    ~spacing=3,
    ~biomes,
    ~can_place=
      (biome, cluster) =>
        switch (biome) {
        | Plain(biome_flower)
        | Forest(biome_flower) =>
          Minecraft.Block.equal_material(
            biome_flower.block,
            cluster.Biome_overlay.block,
          )
        | _ => false
        },
    ~place=
      (~x, ~z, cluster, region) =>
        place_flower(~x, ~z, cluster.Biome_overlay.block, region),
    cluster_centers,
    region,
  );
};

let cactus_has_space = (~x, ~y, ~z, region) => {
  Minecraft.Region.(
    switch (
      get_block_opt(region, ~x=x - 1, ~y, ~z),
      get_block_opt(region, ~x, ~y, ~z=z - 1),
      get_block_opt(region, ~x=x + 1, ~y, ~z),
      get_block_opt(region, ~x, ~y, ~z=z + 1),
    ) {
    | (Some(Air), Some(Air), Some(Air), Some(Air)) => true
    | _ => false
    }
  );
};

let place_cactus = (~x, ~z, region) => {
  open Core_kernel;
  let y = Minecraft.Region.height_at(region, ~x, ~z);
  let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
  switch (block) {
  | Sand when cactus_has_space(~x, ~y=y + 1, ~z, region) =>
    let height = Random.int_incl(1, 3);
    for (y in y + 1 to y + height) {
      Minecraft.Region.set_block(~x, ~y, ~z, Cactus, region);
    };
  | _ => ()
  };
};

let apply_cactus = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  open Core_kernel;
  let region = region;
  let cluster_centers =
    make_clusters(
      ~spacing=100,
      ~biomes,
      region,
      ~f=
        fun
        | Desert(cactus) when Random.int(100) < cactus.percentage =>
          Some(cactus)
        | _ => None,
    );
  fill_clusters(
    ~spacing=5,
    ~biomes,
    ~can_place=
      (biome, _cluster) =>
        switch (biome) {
        | Desert(_) => true
        | _ => false
        },
    ~place=(~x, ~z, _cluster, region) => place_cactus(~x, ~z, region),
    cluster_centers,
    region,
  );
};

let apply_tallgrass = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  let tallgrass =
    Point_cloud.init(
      ~cover_edges=false,
      ~side=Minecraft.Region.block_per_region_side,
      ~spacing=2,
      (_, _) =>
      Random.int(100) < 67
    );
  let (rx, rz) = Minecraft.Region.region_offset(region);
  Point_cloud.iter(tallgrass, ~f=(~x, ~y as z, value) =>
    if (value) {
      let x = int_of_float(x) + rx;
      let z = int_of_float(z) + rz;
      let y = Minecraft.Region.height_at(region, ~x, ~z);
      let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
      let block_above = Minecraft.Region.get_block(region, ~x, ~y=y + 1, ~z);
      switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
      | Forest(_)
      | Plain(_)
      | Savanna =>
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
        }
      | Pine_forest =>
        if (Random.int(100) < 20) {
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
        }
      | Desert(_) =>
        if (Random.int(100) < 1) {
          switch (block, block_above) {
          | (Sand, Air) =>
            Minecraft.Region.set_block(
              ~x,
              ~y=y + 1,
              ~z,
              Minecraft.Block.Dead_bush,
              region,
            )
          | _ => ()
          };
        }
      | Ocean
      | Barren_mountain
      | Snow_mountain
      | Shore
      | Stone_shore
      | River => ()
      };
    }
  );
};

let apply_sandstone = (region: Minecraft.Region.t) => {
  let region = region;
  Minecraft_converter.iter_blocks(region, (~x, ~z) => {
    Minecraft.Region.(
      Range.fold(0, block_per_chunk_vertical - 1, false, (below_is_air, y) => {
        switch (get_block(~x, ~y, ~z, region)) {
        | Air => true
        | Sand when below_is_air =>
          set_block(~x, ~y, ~z, Sandstone, region);
          false;
        | _ => false
        }
      })
      |> ignore
    )
  });
};

let apply_region = ((), region: Minecraft.Region.t) => {
  let (biomes, _) = Biome_overlay.require();
  apply_grass(biomes, region);
  apply_trees(biomes, region);
  apply_snow(biomes, region);
  apply_flowers(biomes, region);
  apply_cactus(biomes, region);
  apply_tallgrass(biomes, region);
  apply_sandstone(region);
};

let (require, prepare, apply) =
  Overlay.make_no_canon(
    "plant",
    prepare,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );