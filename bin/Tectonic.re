type direction =
  | N
  | E
  | S
  | W;

type intermediate = {
  direction,
  is_ocean: bool,
};

type tile =
  | Ocean
  | Plain
  | Mountain;

let show = tile =>
  switch (tile.direction) {
  | N => "⬆️"
  | E => "➡️"
  | S => "⬇️"
  | W => "⬅️"
  };

let xy_of_direction = direction =>
  switch (direction) {
  | N => (0, (-1))
  | E => (1, 0)
  | S => (0, 1)
  | W => ((-1), 0)
  };

let are_opposed = (a, b) => {
  switch (a, b) {
  | (N, S)
  | (E, W)
  | (S, N)
  | (W, E) => true
  | _ => false
  };
};

let random_direction = () =>
  switch (Random.int(4)) {
  | 0 => N
  | 1 => E
  | 2 => S
  | _ => W
  };

let generate = () => {
  let size = 48;
  let point_cloud =
    Point_cloud.init(
      ~width=48,
      ~height=48,
      ~spacing=8,
      (_, _) => {
        let direction = random_direction();
        let is_ocean = Random.int(100) < 50;
        {direction, is_ocean};
      },
    );
  let edge = {direction: S, is_ocean: true};
  Grid.init(size, size, (x, y) =>
    Point_cloud.nearest_with_edge(
      point_cloud,
      edge,
      float_of_int(x),
      float_of_int(y),
    )
  );
};

let convert_intermediate = (grid: Grid.t(intermediate)) => {
  Grid.init(
    grid.width,
    grid.height,
    (x, y) => {
      let {direction, is_ocean} = Grid.at(grid, x, y);
      let (px, py) = xy_of_direction(direction);
      let toward = Grid.at'(grid, x + px, y + py);
      if (!is_ocean && are_opposed(direction, toward.direction)) {
        Mountain;
      } else if (is_ocean) {
        Ocean;
      } else {
        Plain;
      };
    },
  );
};

let phase =
  Phase_chain.(
    phase("Generate tectonic plates", generate(_))
    @> phase("Convert", convert_intermediate(_))
    @> phase_repeat(1, "Subdivide tectonic", Subdivide.subdivide)
  );