open Core_kernel;

type options = {glassify: Minecraft.Block.material => bool};

let prepare = (canon: Canonical_overlay.t, ()) => {
  Draw.draw_sparse_grid(
    ~point_size=1,
    fun
    | None => 0
    | Some () => 0xFFFFFF,
    "obstacles.bmp",
    canon.obstacles,
  );
};

let apply_region = (options, (), args: Minecraft_converter.region_args) => {
  let {glassify} = options;
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
};

let overlay = (canon, options): Overlay.monad(unit) =>
  Overlay.make(
    "debug",
    prepare(canon),
    apply_region(options),
    bin_reader_unit,
    bin_writer_unit,
  );