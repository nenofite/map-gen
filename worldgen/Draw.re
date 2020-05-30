let draw =
    (colorizer: (int, int) => int, width: int, height: int, file: string)
    : unit => {
  open Images;
  open OImages;

  let img = (new rgb24)(width, height);
  for (y in 0 to pred(height)) {
    for (x in 0 to pred(width)) {
      let rgb = colorizer(x, y);
      let r = (rgb land 0xFF0000) lsr 16;
      let g = (rgb land 0x00FF00) lsr 8;
      let b = rgb land 0x0000FF;
      img#set(x, y, {r, g, b});
    };
  };
  img#save(file, Some(Png), []);
  ();
};

/** draw_grid creates a .PPM bitmap file with each pixel representing a tile on the grid */
let draw_grid = (colorizer: 'a => int, file: string, grid: Grid.t('a)): unit => {
  draw(
    (x, y) => colorizer(Grid.at(grid, x, y)),
    grid.side,
    grid.side,
    file,
  );
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

let draw_sparse_grid =
    (colorizer: option('a) => int, file: string, grid: Sparse_grid.t('a))
    : unit => {
  draw(
    (x, y) => colorizer(Sparse_grid.at(grid, x, y)),
    grid.side,
    grid.side,
    file,
  );
};