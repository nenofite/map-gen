/* Make a world and level, put some dirt in the center, and save */
open! Core;

type tag =
  | Edge // Can be placed on edge of grid
  | Bottom; // Can be placed on bottom edge of grid

let test_stairs = region => {
  Building_old.stair_foundation(
    region,
    ~minx=14,
    ~maxx=15,
    ~y=10,
    ~minz=7,
    ~maxz=8,
  );
};

let make_building = (~x as ox, ~y as oy, ~z as oz, region) => {
  Tale.block("Making building", ~f=() => {
    let xs = 10;
    let zs = 10;
    let ys = 10;
    let wfc = Wave_building.prepare_wave(~xs, ~ys, ~zs);
    let (xs, ys, zs) = Wave_collapse.item_dims(wfc);
    for (z in 0 to zs - 1) {
      for (x in 0 to xs - 1) {
        for (y in 0 to ys - 1) {
          let b =
            Wave_collapse.item_at(
              wfc,
              ~x,
              ~y,
              ~z,
              ~default=Minecraft.Block.Gold_block,
            );
          Minecraft.Region.set_block(
            b,
            ~x=x + ox,
            ~y=y + oy,
            ~z=z + oz,
            region,
          );
        };
      };
    };
    Torching.illuminate_bounds(
      ~x=(ox, ox + xs),
      ~y=(oy, oy + ys),
      ~z=(oz, oz + zs),
      region,
    );
  });
};

let test = () => {
  open Minecraft;
  // TODO improve config
  Config.Paths.overlays_base := "seed-test/overlays";
  Config.Paths.world_level_base := "seed-test/worlds/seed-test";
  Config.Paths.create_directories();
  Config.Install.set_install_world_at(
    Some("/mnt/c/Users/Ben/AppData/Roaming/.minecraft/saves"),
  );

  let builder = World.make("test", ~generator=Generator.Flat, ());
  World.make_region(
    ~rx=0,
    ~rz=0,
    builder,
    r => {
      Region.(
        iter_region_xz(
          r,
          ~f=(~x, ~z) => {
            let height = 2;
            for (y in 1 to height - 2) {
              set_block(~x, ~y, ~z, Block.Dirt, r);
            };
            set_block(~x, ~y=height - 1, ~z, Block.Grass_block, r);
            set_block(~x, ~y=0, ~z, Block.Bedrock, r);
          },
        )
      );
      make_building(~x=11, ~y=1, ~z=2, r);
      make_building(~x=11, ~y=1, ~z=30, r);
      make_building(~x=35, ~y=1, ~z=2, r);
      make_building(~x=35, ~y=1, ~z=30, r);
    },
  );
};