/* Make a world and level, put some dirt in the center, and save */

let test = () => {
  let world_config =
    World.{name: "hopefully", spawn: (0, 0, 0), generator: Generator.Flat};
  let region_path = World.save(world_config);

  let tree = Block_tree.create();
  open Block_tree;
  for (x in 0 to pred(Block_tree.block_per_chunk)) {
    for (z in 0 to pred(Block_tree.block_per_chunk)) {
      set_block(tree, x, 0, z, Block.Grass);
    };
  };
  set_block(tree, 8, 1, 8, Block.Sapling);
  Block_tree.save_region(region_path, tree, 0, 0);
};