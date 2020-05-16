/** draw_grid creates a .PPM bitmap file with each pixel representing a tile on the grid */
let draw_grid = (colorizer: 'a => int, file: string, grid: Grid.t('a)): unit => {
  let out = open_out(file);
  Printf.fprintf(out, "P6\n%d %d\n255\n", grid.side, grid.side);
  Grid.scan_iter(
    grid,
    (_x, _y, n) => {
      let rgb = colorizer(n);
      output_char(out, char_of_int((rgb land 0xFF0000) lsr 16));
      output_char(out, char_of_int((rgb land 0x00FF00) lsr 8));
      output_char(out, char_of_int(rgb land 0x0000FF));
    },
  );
  output_char(out, '\n');
  close_out(out);
};

let phase = (file, colorize) => {
  let name = Printf.sprintf("Draw %s", file);
  Phase_chain.phase(
    name,
    grid => {
      draw_grid(colorize, file, grid);
      grid;
    },
  );
};