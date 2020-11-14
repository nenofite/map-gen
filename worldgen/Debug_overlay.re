open Core_kernel;

type options = {
  glassify: Minecraft.Block.material => bool,
  illuminate: bool,
};

let prepare = (canon: Canonical_overlay.t, ()) => {
  Draw.draw_grid(
    fun
    | false => 0
    | true => 0xFFFFFF,
    "obstacles.bmp",
    canon.obstacles,
  );
};

let apply_region = (options, (), args: Minecraft_converter.region_args) => {
  let {glassify, illuminate} = options;
  Minecraft_converter.iter_blocks(args.region, (~x, ~z) => {
    Minecraft.Region.(
      for (y in 0 to block_per_region_vertical - 1) {
        let here = get_block(args.region, ~x, ~y, ~z);
        if (glassify(here)) {
          set_block(~x, ~y, ~z, Minecraft.Block.Glass, args.region);
        };
      }
    )
  });
  if (illuminate) {
    let (x_off, z_off) = Minecraft.Region.region_offset(args.region);
    let z = (z_off, z_off + Minecraft.Region.block_per_region_side - 1);
    let x = (x_off, x_off + Minecraft.Region.block_per_region_side - 1);
    let y = (0, Minecraft.Region.block_per_region_vertical - 1);
    Torching.illuminate_bounds(~x, ~y, ~z, args.region);
  };
};

let overlay = (canon, options): Overlay.monad(unit) =>
  Overlay.make(
    "debug",
    prepare(canon),
    apply_region(options),
    bin_reader_unit,
    bin_writer_unit,
  );