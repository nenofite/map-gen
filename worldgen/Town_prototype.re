type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
};

type output = {add_elevation: Grid.t(int)};

let max_elevation = 150;
let min_elevation = 0;
let elevation_range = 10;

let make_input = () => {
  let elevation =
    Phase_chain.(
      run_all(
        phase("init", () =>
          Grid.init(4, (_x, _y) => Random.int(elevation_range) + 80)
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
  img#save(file, Some(Png), []);
  ();
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
  {add_elevation: add_elevation};
};

let test = () => {
  let input = make_input();
  let output = run(input);
  draw(input, output, "town_proto.png");
};