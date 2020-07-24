/* Make a world and level, put some dirt in the center, and save */

let test_stairs = region => {
  let args =
    Minecraft_converter.{
      region,
      rx: 0,
      rz: 0,
      gx_offset: 0,
      gy_offset: 0,
      gsize: 0,
    };
  Building.stair_foundation(
    args,
    ~minx=14,
    ~maxx=15,
    ~y=10,
    ~minz=7,
    ~maxz=8,
  );
};

let test = () => {
  Minecraft.(
    World.make("hopefully", ~generator=Generator.Flat, builder => {
      World.make_region(
        ~rx=0,
        ~rz=0,
        builder,
        r => {
          open Region;
          for (x in 0 to pred(block_per_chunk_side * 4)) {
            for (z in 0 to pred(block_per_chunk_side * 4)) {
              for (y in 1 to Random.int(6)) {
                set_block(~x, ~y, ~z, Block.Grass, r);
              };
              set_block(~x, ~y=0, ~z, Block.Grass, r);
            };
          };
          test_stairs(r);
          let ent = Entity.{id: "villager", x: 4., y: 10., z: 5.};
          add_entity(ent, r);
        },
      )
    })
  );
};