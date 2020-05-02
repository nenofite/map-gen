type tile = int;

let colorize = (elevation: tile): int => (elevation + 100) * 0x010101;

let convert = (tectonic: Grid.t(Tectonic.tile)) => {
  Grid.init(
    tectonic.width,
    tectonic.height,
    (x, y) => {
      let here = Grid.at(tectonic, x, y);
      let (px, py) = Tectonic.xy_of_direction(here.direction);
      let toward = Grid.at'(tectonic, x + px, y + py);
      if (Tectonic.are_opposed(here.direction, toward.direction)) {
        /* Mountain */
        60 + Random.int(40);
      } else if (here.is_ocean) {
        /* Ocean */
        (-100) + Random.int(40);
      } else {
        /* Land */
        5 + Random.int(5);
      };
    },
  );
};

let fill = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.fast_sort(Int.compare, elevations);
  let index = Random.int(3);
  let between = Random.float(1.0);
  let new_elevation =
    elevations[index]
    + int_of_float(
        float_of_int(elevations[index + 1] - elevations[index]) *. between,
      );
  new_elevation;
};

let run_phase = (tectonic: Grid.t(Tectonic.tile)): Grid.t(tile) => {
  convert(tectonic)
  |> Util.times(Subdivide.subdivide_with_fill(_, fill), 2, _);
};