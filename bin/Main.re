Printexc.record_backtrace(true);
Random.self_init();

Minecraft.Test.test();

/* while (true) {
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
     )
     |> ignore;

     print_endline("Next?");
     read_line() |> ignore;
   }; */