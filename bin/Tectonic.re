type direction =
  | N
  | E
  | S
  | W;

type tile = {
  direction,
  is_ocean: bool,
};

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
    (_, _) => {
      let direction =
        switch (Random.int(4)) {
        | 0 => N
        | 1 => E
        | 2 => S
        | _ => W
        };
      let is_ocean = Random.int(100) < 67;
      {direction, is_ocean};
    },
  );
};

let fill = (a, b, c, d) => {
  switch (Random.int(4)) {
  | 0 => a
  | 1 => b
  | 2 => c
  | _ => d
  };
};

let subdivide = Subdivide.subdivide(_, fill);

let run_phase = (width, height) => {
  generate(width, height) |> Util.times(subdivide, 3, _);
};