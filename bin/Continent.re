type tile =
  | Ocean
  | Land
  | Mountain;

let show = tile =>
  switch (tile) {
  | Ocean => "~"
  | Land => "+"
  | Mountain => "▲"
  };

let convert = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.init(
    tectonic.width,
    tectonic.height,
    (x, y) => {
      let here = Grid.at(tectonic, x, y);
      let (px, py) = Tectonic.xy_of_direction(here.direction);
      let toward = Grid.at'(tectonic, x + px, y + py);
      if (Tectonic.are_opposed(here.direction, toward.direction)) {
        Mountain;
      } else if (here.is_ocean) {
        Ocean;
      } else {
        Land;
      };
    },
  );
};

let run_phase = (tectonic: Grid.t(Tectonic.tile)): Grid.t(tile) => {
  convert(tectonic);
};