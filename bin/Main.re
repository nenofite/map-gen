Printexc.record_backtrace(true);

let g = Phase_chain.(run_all(Tectonic.phase(3, 3) @@> Heightmap.phase));
/* Print.print(g, Biome.print); */
Draw.draw_grid(Heightmap.colorize, "grid.ppm", g);