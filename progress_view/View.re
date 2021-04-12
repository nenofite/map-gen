type t = {
  tiles_width: int,
  tiles_height: int,
  wwidth: int,
  wheight: int,
  min_x: int,
  min_z: int,
  max_x: int,
  max_z: int,
  zoom: int,
  pixels_per_tile: int,
};

let calculate =
    (
      ~zoom: int,
      ~center_x: int,
      ~center_z: int,
      ~width: int,
      ~height: int,
      ~pixels_per_tile: int,
    ) => {
  let tiles_width = width * zoom;
  let tiles_height = height * zoom;
  let wwidth = width * pixels_per_tile;
  let wheight = height * pixels_per_tile;
  let min_x = center_x - tiles_width / 2;
  let min_z = center_z - tiles_height / 2;
  let max_x = min_x + tiles_width;
  let max_z = min_z + tiles_height;
  {
    tiles_width,
    tiles_height,
    wwidth,
    wheight,
    min_x,
    min_z,
    max_x,
    max_z,
    zoom,
    pixels_per_tile,
  };
};

let apply_to_draw = (~draw, view) => {
  let {min_x, min_z, zoom, pixels_per_tile, _} = view;
  let draw_tile = (x, z, color) => {
    let wx = (x - min_x) / zoom * pixels_per_tile;
    let wy = (z - min_z) / zoom * pixels_per_tile;
    draw(~wx, ~wy, ~color);
  };
  draw_tile;
};
