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
    };
  };
  World.save(world);
};