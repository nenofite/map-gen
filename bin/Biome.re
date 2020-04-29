type tile =
  | Deep_ocean
  | Shallow_ocean
  | Beach
  | Plain
  | Hill
  | Mountain
  | Volcano;

let print = tile =>
  ANSITerminal.(
    switch (tile) {
    | Deep_ocean => print_string([blue], "~")
    | Shallow_ocean => print_string([cyan], "~")
    | Beach => print_string([yellow], "-")
    | Plain => print_string([green], "\"")
    | Hill => print_string([default], "∩")
    | Mountain => print_string([white], "▲")
    | Volcano => print_string([red], "▲")
    }
  );

let convert = (continent: Grid.t(Continent.tile)): Grid.t(tile) => {
  Grid.init(
    continent.width,
    continent.height,
    (x, y) => {
      let here = Grid.at(continent, x, y);
      Continent.(
        switch (here) {
        | Ocean => Deep_ocean
        | Land => Plain
        | Mountain => Mountain
        }
      );
    },
  );
};

let fill_edges = (old_grid: Grid.t(tile)) => {
  Grid.init(
    old_grid.width,
    old_grid.height,
    (x, y) => {
      let old = Grid.at(old_grid, x, y);
      let neighbors = Grid.neighbors(old_grid, x, y);
      switch (old) {
      | Mountain =>
        if (Random.int(100) < 1) {
          Volcano;
        } else {
          Mountain;
        }
      | Plain =>
        if (Array.mem(Deep_ocean, neighbors)) {
          Beach;
        } else if (Array.mem(Mountain, neighbors)) {
          Hill;
        } else {
          Plain;
        }
      | Deep_ocean =>
        if (Array.exists(x => x != Deep_ocean, neighbors)) {
          Shallow_ocean;
        } else {
          Deep_ocean;
        }
      | x => x
      };
    },
  );
};

let run_phase = (continent: Grid.t(Continent.tile)): Grid.t(tile) => {
  convert(continent)
  |> Util.times(Subdivide.subdivide, 2, _)
  |> fill_edges
  |> Util.times(Subdivide.subdivide, 0, _);
};