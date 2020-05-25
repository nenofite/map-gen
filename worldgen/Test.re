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
    ~maxx=18,
    ~y=10,
    ~minz=7,
    ~maxz=11,
  );
};

let test = () => {
  open Minecraft;
  let world_config =
    World.{name: "hopefully", spawn: (0, 0, 0), generator: Generator.Flat};
  let region_path = World.save(world_config);

  let tree = Block_tree.create();
  open Block_tree;
  for (x in 0 to pred(Block_tree.block_per_chunk * 4)) {
    for (z in 0 to pred(Block_tree.block_per_chunk * 4)) {
      for (y in 1 to Random.int(6)) {
        set_block(tree, x, y, z, Block.Grass);
      };
      set_block(tree, x, 0, z, Block.Grass);
    };
  };
  test_stairs(tree);
  Block_tree.save_region(region_path, tree, 0, 0);
};