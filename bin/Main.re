Printexc.record_backtrace(true);
Random.self_init();

Minecraft.Test.test();

let world =
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> Draw.phase("grid-height.ppm", Heightmap.colorize)
      /* @@> Erosion.phase
         @@> draw_phase("grid-erosion.ppm", Erosion.colorize), */
      @> River.phase
      @> Draw.phase("grid-river.ppm", River.colorize),
    )
  );

Minecraft_converter.save(world);