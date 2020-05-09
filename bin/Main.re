Printexc.record_backtrace(true);
Random.self_init();

let draw_phase = (file, colorize) => {
  let name = Printf.sprintf("Draw %s", file);
  Phase_chain.phase(
    name,
    grid => {
      Draw.draw_grid(colorize, file, grid);
      grid;
    },
  );
};

while (true) {
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> draw_phase("grid-height.ppm", Heightmap.colorize)
      /* @@> Erosion.phase
         @@> draw_phase("grid-erosion.ppm", Erosion.colorize), */
      @> River.phase
      @> draw_phase("grid-river.ppm", River.colorize),
    )
  )
  |> ignore;

  print_endline("Next?");
  read_line() |> ignore;
};