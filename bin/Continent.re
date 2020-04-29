type tile =
  | Ocean
  | Land
  | Mountain;

let print = tile =>
  ANSITerminal.(
    switch (tile) {
    | Ocean => print_string([blue], "~")
    | Land => print_string([default], "+")
    | Mountain => print_string([white], "▲")
    }
  );

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