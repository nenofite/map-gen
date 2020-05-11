/* Make a world and level, put some dirt in the center, and save */

let test = () => {
  let tree = Block_tree.create();
  let world =
    World.{
      level:
        Level.{
          name: "hopefully",
          spawn: (0, 0, 0),
          generator: Generator.Flat,
          tree,
        },
    };
  let center = Block_tree.get_chunk(tree, 0, 0);
  for (x in 0 to pred(Block_tree.chunk_side)) {
    for (z in 0 to pred(Block_tree.chunk_side)) {
      Block_tree.set_block(center, x, 0, z, Block.Dirt);
      if (x == 0
          || x == pred(Block_tree.chunk_side)
          || z == 0
          || z == pred(Block_tree.chunk_side)) {
        Block_tree.set_block(center, x, 1, z, Block.Planks);
        Block_tree.set_block(center, x, 2, z, Block.Planks);
        for (y in 3 to 50) {
          Block_tree.set_block(center, x, y, z, Block.Glass);
        };
      };
      Block_tree.set_block(center, x, 100, z, Block.Dirt);
    };
  };
  World.save(world);
};