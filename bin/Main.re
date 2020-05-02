Printexc.record_backtrace(true);

/* let g = Tectonic.run_phase(3, 3) |> Continent.run_phase |> Biome.run_phase; */
let g = Tectonic.run_phase(3, 3) |> Heightmap.run_phase;
/* Print.print(g, Biome.print); */
Draw.draw_grid(Heightmap.colorize, "grid.ppm", g);