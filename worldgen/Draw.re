let images_color_of_int = color => {
  let (r, g, b) = Color.split_rgb(color);
  Images.{r, g, b};
};

let draw =
    (colorizer: (int, int) => int, width: int, height: int, file: string)
    : unit => {
  open Images;
  open OImages;

  let file = Config.Paths.drawing(file);

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
  img#save(file, Some(Bmp), []);
  ();
};

let draw_griddable =
    (
      type t,
      type elt,
      module G: Grid.Griddable.S0 with type t = t and type elt = elt,
      ~f: elt => int,
      ~file: string,
      grid: t,
    )
    : unit => {
  let s = G.side(grid);
  draw((x, y) => f(G.get(~x, ~z=y, grid)), s, s, file);
};

/** draw_grid creates a .PPM bitmap file with each pixel representing a tile on the grid */
let draw_grid = (colorizer: 'a => int, file: string, grid: Grid.t('a)): unit => {
  draw(
    (x, y) => colorizer(Grid.Compat.at(grid, x, y)),
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

let draw_points =
    (
      ~side,
      ~default_point_size=max(side / 100, 3),
      ~default_color=0xFFFFFF,
      ~background=0,
      file: string,
      f,
    )
    : 'a => {
  open Images;
  open Rgb24;

  let file = Config.Paths.drawing(file);

  let img = make(side, side, images_color_of_int(background));

  let draw_point = (~color=default_color, ~size=default_point_size, x, y) => {
    let min_x = x - size / 2;
    let min_y = y - size / 2;
    let max_x = min_x + size - 1;
    let max_y = min_y + size - 1;
    let icolor = images_color_of_int(color);
    for (y in min_y to max_y) {
      for (x in min_x to max_x) {
        if (0 <= x && x < side && 0 <= y && y < side) {
          set(img, x, y, icolor);
        };
      };
    };
  };

  let result = f(~draw_point);
  save(file, Some(Bmp), [], Rgb24(img));
  result;
};

let draw_sparse_grid =
    (
      ~point_size=?,
      colorizer: option('a) => int,
      file: string,
      grid: Sparse_grid.t('a),
    )
    : unit => {
  draw_points(
    ~side=grid.side,
    ~background=colorizer(None),
    ~default_point_size=?point_size,
    file,
    (~draw_point) => {
    Sparse_grid.iter(grid, ((x, z), v) => {
      draw_point(~color=colorizer(Some(v)), x, z)
    })
  });
};
