Printexc.record_backtrace(true);
Random.self_init();

Minecraft.Test.test();

while (true) {
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

  print_endline("Next or save [N/s]");
  switch (read_line()) {
  | "S"
  | "s"
  | "save" =>
    Minecraft_converter.save(world);
    exit(0);
  | "N"
  | "n"
  | "next"
  | _ => ()
  };
};