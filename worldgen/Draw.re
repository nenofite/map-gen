let images_color_of_int = color => {
  let (r, g, b) = Mg_util.Color.split_rgb(color);
  Images.{r, g, b};
};

/** draw_grid creates a .PPM bitmap file with each pixel representing a tile on the grid */
let draw_grid = (colorize: 'a => int, file: string, grid: Grid.t('a)): unit => {
  let l = Progress_view.push_layer();
  Progress_view.update(
    ~title=file,
    ~draw_dense=
      ((), x, z) =>
        if (Grid.is_within(x, z, grid)) {
          Some(colorize(Grid.Mut.get(~x, ~z, grid)));
        } else {
          None;
        },
    ~state=(),
    l,
  );
  Progress_view.save(~side=Grid.side(grid), file);
  Progress_view.remove_layer(l);
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
