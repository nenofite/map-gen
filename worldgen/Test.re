/* Make a world and level, put some dirt in the center, and save */

let test_building = region => {
  let template =
    Util.read_file("buildings/test.txt", fin =>
      Template_txt.read_template(fin)
    );
  let piece = Building.{template, open_sides: [], usage: Common};
  let pieces = List.init(3, x => ((x, 0), piece));
  let assembly = Building.{pieces, center_piece: (7, 17)};
  let args =
    Minecraft_converter.{
      region,
      rx: 0,
      rz: 0,
      gx_offset: 0,
      gy_offset: 0,
      gsize: 0,
    };
  Building.apply_assembly(args, assembly);
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
  test_building(tree);
  Block_tree.save_region(region_path, tree, 0, 0);
};