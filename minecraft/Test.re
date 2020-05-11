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
  {
    open Block_tree;
    for (x in 0 to pred(Block_tree.chunk_side)) {
      for (z in 0 to pred(Block_tree.chunk_side)) {
        set_block_any_chunk(tree, x, 0, z, Block.Dirt);
        if (x == 0
            || x == pred(Block_tree.chunk_side)
            || z == 0
            || z == pred(Block_tree.chunk_side)) {
          set_block_any_chunk(tree, x, 1, z, Block.Planks);
          set_block_any_chunk(tree, x, 2, z, Block.Planks);
          for (y in 3 to 50) {
            set_block_any_chunk(tree, x, y, z, Block.Glass);
          };
        };
        set_block_any_chunk(tree, x, 100, z, Block.Dirt);
      };
    };
    set_block_any_chunk(tree, 8, 100, 8, Block.Air);
    set_block_any_chunk(tree, 2, 1, 2, Block.Torch);
  };
  World.save(world);
};