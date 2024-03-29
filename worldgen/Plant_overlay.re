open Bin_prot.Std;

[@deriving bin_io]
type t = unit;

let prepare = () => ();

let is_air_or_snow =
  fun
  | Minecraft.Block.Air
  | Snow(_) => true
  | _ => false;

let not_air_or_snow = b => !is_air_or_snow(b);

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
        let y =
          Option.value(
            Minecraft.Region.highest_such_block(
              region,
              ~x,
              ~z,
              not_air_or_snow,
            ),
            ~default=0,
          );
        let block = Minecraft.Region.get_block(region, ~x, ~y, ~z);
        switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
        | Forest(_) =>
          switch (block) {
          | Dirt
          | Grass_block =>
            Minecraft_template.place_ignoring(
              Oak_tree.random_tree(),
              region,
              ~ignorable=is_air_or_snow,
              ~x,
              ~y=y + 1,
              ~z,
            )
            |> ignore
          | _ => ()
          }
        | Pine_forest
        | Snow_taiga =>
          switch (block) {
          | Dirt
          | Grass_block =>
            Minecraft_template.place_ignoring(
              Spruce_tree.random_tree(),
              region,
              ~ignorable=is_air_or_snow,
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
        | Snow_plains
        | River => ()
        };
      };
    },
  );
};

let is_opaque_or_water = b => Minecraft.Block.(is_wet(b) || is_opaque(b));

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
      | Snow_mountain
      | Snow_plains
      | Snow_taiga =>
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

let snow_depth_at = (~x, ~z) => {
  let p =
    Mg_util.Perlin.at_opts(
      ~freq=20.,
      ~intervals=1,
      ~x=float(x),
      ~y=0.,
      ~z=float(z),
      (),
    );
  max(1, abs(int_of_float(p *. 10.) mod 10 - 5) - 3);
};

let apply_snow =
    (~force_depth=?, biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  let side = Overlay.Canon.require().side;
  let snow_edge_dist = (~x, ~z) => {
    let r = 3;
    let is_edge = ref(false);
    let dist = ref(Int.max_int);
    for (hz in z - r to z + r) {
      for (hx in x - r to x + r) {
        if (Grid.is_within_side(~x=hx, ~z=hz, side)) {
          switch (Biome_overlay.biome_at(~x=hx, ~z=hz, biomes)) {
          | Snow_mountain
          | Snow_plains
          | Snow_taiga => ()
          | _ =>
            is_edge := true;
            dist := min(dist^, abs(hx - x) + abs(hz - z));
          };
        };
      };
    };
    if (is_edge^) {
      dist^;
    } else {
      (-1);
    };
  };
  let snow_depth_with_edge = (~x, ~z) => {
    switch (snow_edge_dist(~x, ~z)) {
    | 1 => 1
    | 2 => snow_depth_at(~x, ~z) / 4
    | 3 => snow_depth_at(~x, ~z) / 2
    | _ => snow_depth_at(~x, ~z)
    };
  };
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;
      let y = height_at(region, ~x, ~z);
      let top = get_block(region, ~x, ~y, ~z);
      let biome = Biome_overlay.biome_at(~x, ~z, biomes);
      switch (biome) {
      | Snow_mountain
      | Snow_plains
      | Snow_taiga =>
        if (Minecraft.Block.should_receive_snow(top)) {
          let d =
            switch (force_depth) {
            | Some(d) => d
            | None => snow_depth_with_edge(~x, ~z)
            };
          set_block(~x, ~y=y + 1, ~z, Snow(d), region);
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
  open Core;
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
  open Core;
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
  open Core;
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
  open Core;
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
  open Core;
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

let apply_edge_cactus = (biomes: Biome_overlay.t', region: Minecraft.Region.t) => {
  open Core;
  let side = Overlay.Canon.require().side;
  let is_desert = (~x, ~z) => {
    switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
    | Desert(_) => true
    | _ => false
    };
  };
  let is_desert_edge = (~x, ~z) => {
    let r = 3;
    let is_edge = ref(false);
    for (z in z - r to z + r) {
      for (x in x - r to x + r) {
        if (Grid.is_within_side(~x, ~z, side)) {
          switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
          | Desert(_) => ()
          | _ => is_edge := true
          };
        };
      };
    };
    is_edge^;
  };
  let roll_cactus = (~x, ~z) => {
    switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
    | Desert(c) => Random.int(100) < c.percentage
    | _ => false
    };
  };

  let (x_off, z_off) = Minecraft.Region.region_offset(region);
  let coords =
    Point_cloud.make_int_list(
      ~side=Minecraft.Region.block_per_region_side,
      ~spacing=3,
      (),
    );
  List.iter(
    coords,
    ~f=((lx, lz)) => {
      let (x, z) = (lx + x_off, lz + z_off);
      if (is_desert(~x, ~z) && is_desert_edge(~x, ~z) && roll_cactus(~x, ~z)) {
        place_cactus(~x, ~z, region);
      };
    },
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
      | Snow_plains
      | Snow_taiga
      | Shore
      | Stone_shore
      | River => ()
      };
    }
  );
};

let is_near_river_at = (~x, ~z, base) => {
  let r = 1;
  let side = Base_overlay.side(base);
  let result = ref(false);
  for (z in z - r to z + r) {
    for (x in x - r to x + r) {
      if (Grid.is_within_side(~x, ~z, side)
          && Base_overlay.river_at(~x, ~z, base)) {
        result := true;
      };
    };
  };
  result^;
};

let apply_riverside =
    (biomes: Biome_overlay.t', base, region: Minecraft.Region.t) => {
  Minecraft.Region.iter_region_xz(region, ~f=(~x, ~z) => {
    switch (Biome_overlay.biome_at(~x, ~z, biomes)) {
    | Forest(_)
    | Plain(_)
    | Savanna
    | Pine_forest =>
      if (is_near_river_at(~x, ~z, base)) {
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
      }
    | Desert(_)
    | Ocean
    | Barren_mountain
    | Snow_mountain
    | Snow_plains
    | Snow_taiga
    | Shore
    | Stone_shore
    | River => ()
    }
  });
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
  let (base, _) = Base_overlay.require();
  let (biomes, _) = Biome_overlay.require();
  apply_grass(biomes, region);
  apply_snow(biomes, region);
  apply_riverside(biomes, base, region);
  apply_trees(biomes, region);
  /* Snow again to cover trees */
  apply_snow(~force_depth=1, biomes, region);
  apply_flowers(biomes, region);
  apply_edge_cactus(biomes, region);
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