Printexc.record_backtrace(true);

let draw_phase = (file, colorize) => {
  Phase_chain.(
    (
      grid => {
        Draw.draw_grid(colorize, file, grid);
        grid;
      }
    )
    @> finish
  );
};

Phase_chain.(
  run_all(
    Tectonic.phase(3, 3)
    @@> Heightmap.phase
    @@> draw_phase("grid-height.ppm", Heightmap.colorize)
    /* @@> Erosion.phase
       @@> draw_phase("grid-erosion.ppm", Erosion.colorize), */
    @@> River.phase
    @@> draw_phase("grid-river.ppm", River.colorize),
  )
);