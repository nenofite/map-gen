type t = {
  pixels_per_tile: int,
  width: int,
  height: int,
};

let make_window = (~pixels_per_tile=2, ~width=400, ~height=300, ()) => {
  let w = {pixels_per_tile, width, height};
  open Graphics;
  open_graph(
    Printf.sprintf(
      " %dx%d",
      width * pixels_per_tile,
      height * pixels_per_tile,
    ),
  );
  set_window_title("Progress View");
  auto_synchronize(false);
  w;
};

let close_window = (_w: t) => {
  Graphics.close_graph();
};

let update =
    (
      ~center_x: int,
      ~center_z: int,
      title: string,
      draw_tiles:
        (
          ~x: (int, int),
          ~z: (int, int),
          (int, int, (int, int, int)) => unit
        ) =>
        unit,
      w: t,
    ) => {
  open Graphics;
  let wwidth = w.width * w.pixels_per_tile;
  let wheight = w.height * w.pixels_per_tile;
  let min_x = center_x - w.width / 2;
  let min_z = center_z - w.height / 2;
  let max_x = min_x + w.width;
  let max_z = min_z + w.height;
  set_color(black);
  fill_rect(0, 0, wwidth, wheight);

  let draw_tile = (x, z, (r, g, b)) => {
    let wx = (x - min_x) * w.pixels_per_tile;
    let wy = (max_z - z) * w.pixels_per_tile;
    set_color(rgb(r, g, b));
    fill_rect(wx, wy, w.pixels_per_tile, w.pixels_per_tile);
  };

  draw_tiles(~x=(min_x, max_x), ~z=(min_z, max_z), draw_tile);
  set_window_title(title);
  synchronize();
};

let manual_test = () => {
  let w = make_window();
  let draw = (~x as (min_x, max_x), ~z as (min_z, max_z), set_coord) => {
    for (z in min_z to max_z) {
      for (x in min_x to max_x) {
        if (x mod 3 == 0) {
          set_coord(x, z, (0, 0, 255));
        };
        if (x == z) {
          set_coord(x, z, (255, 0, 0));
        };
      };
    };
  };
  update(~center_x=200, ~center_z=200, "howdy", draw, w);
  ();
};