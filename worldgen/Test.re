/* Make a world and level, put some dirt in the center, and save */

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

let test = () => {
  open Minecraft;
  let builder = World.make("heightmap", ~generator=Generator.Flat, ());
  World.make_region(
    ~rx=0,
    ~rz=0,
    builder,
    r => {
      open Region;
      for (x in 0 to pred(block_per_chunk_side * 4)) {
        for (z in 0 to pred(block_per_chunk_side * 4)) {
          let height = Random.int(6);
          set_block(~x, ~y=0, ~z, Block.Bedrock, r);
          for (y in 1 to height - 1) {
            set_block(~x, ~y, ~z, Block.Dirt, r);
          };
          set_block(~x, ~y=height, ~z, Block.Grass_block, r);
          set_block(~x, ~y=height + 1, ~z, Block.Torch, r);
        };
      };
      test_stairs(r);
      let ent = Entity.{id: "villager", x: 4., y: 10., z: 5.};
      add_entity(ent, r);
    },
  );
};
