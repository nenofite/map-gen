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

let generate = (width, height) => {
  Grid.init(
    width,
    height,
    (x, y) => {
      let direction =
        switch (Random.int(4)) {
        | 0 => N
        | 1 => E
        | 2 => S
        | _ => W
        };
      /* Edges are always ocean */
      let is_edge = /*x == 0 || x == width - 1 ||*/ y == 0 || y == height - 1;
      let is_ocean = is_edge || Random.int(100) < 50;
      {direction, is_ocean};
    },
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

let phase = (width, height) => {
  Phase_chain.(
    (() => generate(width, height))
    @> repeat(4, Subdivide.subdivide)
    @@> convert_intermediate(_)
    @> finish
  );
};