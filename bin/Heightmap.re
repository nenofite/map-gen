type tile = int;

let colorize = (tile: tile): int => (tile + 50) * 255 / 150 * 0x010101;

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
        30 + Random.int(70);
      } else if (here.is_ocean) {
        /* Ocean */
        (-50) + Random.int(50);
      } else {
        /* Land */
        1 + Random.int(10);
      };
    },
  );
};

let fill_weighted = (a, b, c, d) => {
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

let fill_avg = (a, b, c, d) => {
  let elevations = [|a, b, c, d|];
  Array.fast_sort(Int.compare, elevations);
  let between = Random.float(1.0);
  let new_elevation =
    elevations[0]
    + int_of_float(float_of_int(elevations[3] - elevations[0]) *. between);
  new_elevation;
};

let run_phase = (tectonic: Grid.t(Tectonic.tile)): Grid.t(tile) => {
  convert(tectonic)
  |> Util.times(Subdivide.subdivide_with_fill(_, fill_weighted), 1, _)
  |> Util.times(Subdivide.subdivide_with_fill(_, fill_avg), 1, _);
};

let phase =
  Phase_chain.(
    convert(_)
    @> Subdivide.subdivide_with_fill(_, fill_weighted)
    @> Subdivide.subdivide_with_fill(_, fill_avg)
    @> finish
  );