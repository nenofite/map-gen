type t = {
  pixels_per_tile: int,
  width: int,
  height: int,
};

let make_window = (~pixels_per_tile=2, ~width=400, ~height=400, ()) => {
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
      ~zoom: int,
      ~center_x: int,
      ~center_z: int,
      title: string,
      draw_tiles:
        (
          ~zoom: int,
          ~x: (int, int),
          ~z: (int, int),
          (int, int, (int, int, int)) => unit
        ) =>
        unit,
      w: t,
    ) => {
  open Graphics;
  let tiles_width = w.width * zoom;
  let tiles_height = w.height * zoom;
  let wwidth = w.width * w.pixels_per_tile;
  let wheight = w.height * w.pixels_per_tile;
  let min_x = center_x - tiles_width / 2;
  let min_z = center_z - tiles_height / 2;
  let max_x = min_x + tiles_width;
  let max_z = min_z + tiles_height;
  set_color(black);
  fill_rect(0, 0, wwidth, wheight);

  let draw_tile = (x, z, (r, g, b)) => {
    let wx = (x - min_x) / zoom * w.pixels_per_tile;
    /* In Minecraft, increasing Z moves south, whereas in Graphics increasing Y
     * moves up. So we reverse the vertical axis when drawing: */
    let wy = (max_z - z) / zoom * w.pixels_per_tile;
    set_color(rgb(r, g, b));
    fill_rect(wx, wy, w.pixels_per_tile, w.pixels_per_tile);
  };

  draw_tiles(~zoom, ~x=(min_x, max_x), ~z=(min_z, max_z), draw_tile);
  set_window_title(title);
  synchronize();
};

let manual_test = () => {
  let w = make_window();
  let draw =
      (~zoom as _, ~x as (min_x, max_x), ~z as (min_z, max_z), set_coord) => {
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
  update(~zoom=1, ~center_x=200, ~center_z=200, "howdy", draw, w);
  ();
};