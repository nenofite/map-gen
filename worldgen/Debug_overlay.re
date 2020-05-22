type t = unit;

type options = {glassify: Minecraft.Block.material => bool};

let prepare = () => ();

let apply_region = (options, (), args: Minecraft_converter.region_args) => {
  let {glassify} = options;
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  Minecraft_converter.iter_blocks(
    ~gx_offset, ~gy_offset, ~gsize, (~gx as _, ~gy as _, ~x, ~z) => {
    Minecraft.Block_tree.(
      for (y in 0 to block_per_region_vertical - 1) {
        let here = get_block(region, x, y, z);
        if (glassify(here)) {
          set_block(region, x, y, z, Minecraft.Block.Glass);
        };
      }
    )
  });
};

let overlay = options =>
  Overlay.make("debug", prepare, apply_region(options));