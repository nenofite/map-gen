/* Make a world and level, put some dirt in the center, and save */

let test = () => {
  let world_config =
    World.{name: "hopefully", spawn: (0, 0, 0), generator: Generator.Flat};
  let region_path = World.save(world_config);
  let tree = Block_tree.create();
  open Block_tree;
  for (x in 0 to pred(Block_tree.chunk_side)) {
    for (z in 0 to pred(Block_tree.chunk_side)) {
      set_block(tree, x, 0, z, Block.Dirt);
      if (x == 0
          || x == pred(Block_tree.chunk_side)
          || z == 0
          || z == pred(Block_tree.chunk_side)) {
        set_block(tree, x, 1, z, Block.Planks);
        set_block(tree, x, 2, z, Block.Planks);
        for (y in 3 to 50) {
          set_block(tree, x, y, z, Block.Glass);
        };
      };
      set_block(tree, x, 100, z, Block.Dirt);
    };
  };
  set_block(tree, 8, 100, 8, Block.Air);
  set_block(tree, 2, 1, 2, Block.Torch);
  Block_tree.save_region(region_path, tree, 0, 0);
};