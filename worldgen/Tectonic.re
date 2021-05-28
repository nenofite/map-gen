type direction =
  | N
  | E
  | S
  | W;

type intermediate = {
  id: int,
  direction,
  is_ocean: bool,
};

type tile =
  | Ocean
  | Trench
  | Plain
  | Mountain;

let show = tile =>
  switch (tile.direction) {
  | N => "⬆️"
  | E => "➡️"
  | S => "⬇️"
  | W => "⬅️"
  };

let draw_intermediate = tile => {
  let frac = float_of_int(tile.id) /. 40.;
  Color.blend(0, 0xFFFFFF, frac);
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

let generate = target_size => {
  let size = target_size / 32;
  let next_id = ref(1);
  let edge = {direction: S, is_ocean: true, id: 0};
  let continents =
    Point_cloud.init(
      ~side=size,
      ~spacing=size / 7,
      ~cover_edges=false,
      ~edge_f=(_, _) => edge,
      (_, _) => {
        let direction = random_direction();
        let is_ocean = Random.int(100) < 30;
        let id = next_id^;
        next_id := id + 1;
        {direction, is_ocean, id};
      },
    )
    |> Point_cloud.subdivide(~cover_edges=false, ~spacing=8)
    |> Point_cloud.subdivide(~cover_edges=false, ~spacing=4);
  let plates =
    Point_cloud.init(~side=size, ~cover_edges=false, ~spacing=2, (x, y) => {
      Point_cloud.nearest_with_edge(
        continents,
        edge,
        float_of_int(x),
        float_of_int(y),
      )
    });
  Grid_compat.init(size, (x, y) =>
    Point_cloud.nearest_with_edge(
      plates,
      edge,
      float_of_int(x),
      float_of_int(y),
    )
  );
};

let convert_intermediate = (grid: Grid.t(intermediate)) => {
  Grid_compat.map(
    grid,
    (x, y, here) => {
      let {direction, is_ocean, _} = here;
      let collision =
        List.exists(
          neigh => {are_opposed(direction, neigh.direction)},
          Grid_compat.neighbors(grid, x, y),
        );
      if (collision) {
        if (is_ocean) {Trench} else {Mountain};
      } else if (is_ocean) {
        Ocean;
      } else {
        Plain;
      };
    },
  );
};

let phase = side =>
  Phase_chain.(
    run_all(
      phase("Generate tectonic plates", () => generate(side))
      @> Draw.phase("plates.png", draw_intermediate)
      @> phase("Convert", convert_intermediate(_)),
    )
  );
