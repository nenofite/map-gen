open Core_kernel;

type options = {
  glassify: Minecraft.Block.material => bool,
  illuminate: bool,
};

let prepare = () => {
  let canon = Overlay.Canon.require();
  Draw.draw_grid(
    Overlay.Canon.(
      fun
      | Clear => 0
      | Bridgeable => 0x0000FF
      | Impassable => 0xFFFFFF
    ),
    "obstacles.bmp",
    canon.obstacles,
  );
};

let apply = (options, region: Minecraft.Region.t) => {
  let {glassify, illuminate} = options;
  Minecraft_converter.iter_blocks(region, (~x, ~z) => {
    Minecraft.Region.(
      for (y in 0 to block_per_region_vertical - 1) {
        let here = get_block(region, ~x, ~y, ~z);
        if (glassify(here)) {
          set_block(~x, ~y, ~z, Minecraft.Block.Glass, region);
        };
      }
    )
  });
  if (illuminate) {
    let (x_off, z_off) = Minecraft.Region.region_offset(region);
    let z = (z_off, z_off + Minecraft.Region.block_per_region_side - 1);
    let x = (x_off, x_off + Minecraft.Region.block_per_region_side - 1);
    let y = (0, Minecraft.Region.block_per_region_vertical - 1);
    Torching.illuminate_bounds(~x, ~y, ~z, region);
  };
};
