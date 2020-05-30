type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
};

type split_direction =
  | Along_x
  | Along_z;

type block = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
  next_split: split_direction,
};

type output = {
  add_elevation: Grid.t(int),
  blocks: list(block),
};

let side = 128;
let max_elevation = 150;
let min_elevation = 0;
let base_elevation = 80;
let elevation_range = 10;
let min_cardinal_road_off = side / 3;
let max_cardinal_road_off = side * 2 / 3;

let make_input = () => {
  let elevation =
    Phase_chain.(
      run_all(
        phase("init", () =>
          Grid.init(4, (_x, _y) =>
            Random.int(elevation_range) + base_elevation
          )
        )
        @> phase_repeat(2, "subdivide", Subdivide.subdivide)
        @> phase_repeat(
             3,
             "line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> avg)),
           ),
      )
    );
  let obstacles = Sparse_grid.make(elevation.side);
  {elevation, obstacles};
};

let draw_rect = (img, min_x, max_x, min_z, max_z, border_color) => {
  for (x in min_x to max_x) {
    img#set(x, min_z, border_color);
    img#set(x, max_z, border_color);
  };
  for (z in min_z to max_z) {
    img#set(min_x, z, border_color);
    img#set(max_x, z, border_color);
  };
};

let draw = (input: input, output: output, file) => {
  open Images;
  open OImages;

  let colorize_elevation = ((elev, add)) => {
    let total = elev + add;
    let v =
      Mg_util.Floats.(
        ~~(
          ~.(total - min_elevation)
          /. ~.(max_elevation - min_elevation)
          *. 255.
        )
      );
    if (add > 0) {
      {r: 0, g: v, b: 0};
    } else {
      {r: v, g: v, b: v};
    };
  };

  let combined = Grid.zip(input.elevation, output.add_elevation);
  let side = combined.side;
  let img = (new rgb24)(side, side);

  Grid.iter(combined, (x, y, c) => {img#set(x, y, colorize_elevation(c))});

  List.iter(
    block => {
      let {min_x, max_x, min_z, max_z, _} = block;
      draw_rect(img, min_x, max_x, min_z, max_z, {r: 0, g: 0, b: 255});
    },
    output.blocks,
  );

  img#save(file, Some(Png), []);
  ();
};

let lay_cardinal_roads = (input: input) => {
  /* Find the line within middle 1/3 that has the flattest elevation */
  let calc_elevation_range = (min_x, max_x, min_z, max_z) => {
    let start = Grid.at(input.elevation, min_x, min_z);
    let (min_elev, max_elev) =
      Range.fold(min_z, max_z, (start, start), (cur, z) =>
        Range.fold(
          min_x,
          max_x,
          cur,
          ((cur_min, cur_max), x) => {
            let here = Grid.at(input.elevation, x, z);
            (min(cur_min, here), max(cur_max, here));
          },
        )
      );
    max_elev - min_elev;
  };

  let (road_along_z, _) =
    Range.fold(
      min_cardinal_road_off,
      max_cardinal_road_off,
      ((-1), Int.max_int),
      ((_, cur_range) as cur, x) => {
        let here_range = calc_elevation_range(x - 1, x, 0, side - 1);
        if (here_range < cur_range) {
          (x, here_range);
        } else {
          cur;
        };
      },
    );

  let (road_along_x, _) =
    Range.fold(
      min_cardinal_road_off,
      max_cardinal_road_off,
      ((-1), Int.max_int),
      ((_, cur_range) as cur, z) => {
        let here_range = calc_elevation_range(0, side - 1, z - 1, z);
        if (here_range < cur_range) {
          (z, here_range);
        } else {
          cur;
        };
      },
    );

  /* Slice blocks based on these cardinal roads */
  let next_split = Random.bool() ? Along_x : Along_z;
  let nw = {
    min_x: 0,
    max_x: road_along_z - 1 - 1,
    min_z: 0,
    max_z: road_along_x - 1 - 1,
    next_split,
  };
  let ne = {
    min_x: road_along_z + 1,
    max_x: side - 1,
    min_z: 0,
    max_z: road_along_x - 1 - 1,
    next_split,
  };
  let sw = {
    min_x: 0,
    max_x: road_along_z - 1 - 1,
    min_z: road_along_x + 1,
    max_z: side - 1,
    next_split,
  };
  let se = {
    min_x: road_along_z + 1,
    max_x: side - 1,
    min_z: road_along_x + 1,
    max_z: side - 1,
    next_split,
  };

  [nw, ne, sw, se];
};

let run = (input: input): output => {
  let add_elevation =
    Grid.init(input.elevation.side, (x, y) =>
      if (10 <= x && x < 200 && 20 <= y && y < 300) {
        3;
      } else {
        0;
      }
    );
  let blocks = lay_cardinal_roads(input);
  {add_elevation, blocks};
};

let test = () => {
  let input = make_input();
  let output = run(input);
  draw(input, output, "town_proto.png");
};