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
  let view =
    View.calculate(
      ~zoom,
      ~center_x,
      ~center_z,
      ~width=w.width,
      ~height=w.height,
      ~pixels_per_tile=w.pixels_per_tile,
    );
  set_color(black);
  fill_rect(0, 0, view.wwidth, view.wheight);

  let draw_tile =
    View.apply_to_draw(
      view,
      ~draw=(~wx, ~wy, ~color as (r, g, b)) => {
        set_color(rgb(r, g, b));
        fill_rect(wx, wy, w.pixels_per_tile, w.pixels_per_tile);
      },
    );

  draw_tiles(
    ~zoom,
    ~x=(view.min_x, view.max_x),
    ~z=(view.min_z, view.max_z),
    draw_tile,
  );
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