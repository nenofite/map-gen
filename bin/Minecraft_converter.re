let scale_elevation = elevation => {
  /* Take -30 to 100 and map to 1 to 100 */
  (elevation + 30) * 99 / 130 + 1;
};
let sea_level = scale_elevation(0);

/** save creates a Minecraft world with the given heightmap */
let save = (world: Grid.t(River.tile)): unit => {
  let set_block_any_chunk = Minecraft.Block_tree.set_block_any_chunk;

  print_endline("Filling block tree...");
  /* Create the block tree */
  let tree = Minecraft.Block_tree.create();
  /* Fill in the heightmap */
  let horizontal_scale = 2;
  let vertical_scale = 1;
  for (x in 0 to pred(world.width)) {
    for (z in 0 to pred(world.height)) {
      let here = Grid.at(world, x, z);
      let elevation = scale_elevation(here.elevation);
      set_block_any_chunk(tree, x, 0, z, Minecraft.Block.Bedrock);

      for (y in 1 to elevation) {
        set_block_any_chunk(tree, x, y, z, Minecraft.Block.Dirt);
      };
      if (here.ocean) {
        for (y in elevation + 1 to sea_level) {
          set_block_any_chunk(tree, x, y, z, Minecraft.Block.Water);
        };
      } else if (Option.is_some(here.river)) {
        set_block_any_chunk(tree, x, elevation, z, Minecraft.Block.Water);
      };
      /* let height = here.elevation * vertical_scale;
         for (by in 0 to pred(height)) {
           for (bx in x * horizontal_scale to pred((x + 1) * horizontal_scale)) {
             for (bz in z * horizontal_scale to pred((z + 1) * horizontal_scale)) {
               Minecraft.Block_tree.set_block_any_chunk(
                 tree,
                 bx,
                 by,
                 bz,
                 Minecraft.Block.Dirt,
               );
             };
           };
         }; */
    };
  };
  print_endline("Saving...");
  /* Save the tree in a world */
  let world =
    Minecraft.(
      World.{
        level:
          Level.{
            name: "heightmap",
            spawn: (0, 0, 0),
            generator: Generator.Flat,
            tree,
          },
      }
    );
  Minecraft.World.save(world);
};