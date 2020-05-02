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

let colorize = tile =>
  switch (tile) {
  | Deep_ocean => 0x0D24BF
  | Shallow_ocean => 0x6A71F7
  | Beach => 0xC7C43F
  | Plain => 0x52C137
  | Hill => 0x8CF77A
  | Mountain => 0xFFFFFF
  | Volcano => 0xB9311A
  };

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

type biome_rule = (tile, array(tile)) => option(tile);

/** cascade applies each rule in turn until one returns [Some] */
let rec cascade = (rules, here, neighbors) => {
  switch (rules) {
  | [rule, ...rules] =>
    switch (rule(here, neighbors)) {
    | Some(_) as s => s
    | None => cascade(rules, here, neighbors)
    }
  | [] => None
  };
};

let make_volcanoes: biome_rule =
  (here, neighbors) =>
    switch (here) {
    | Mountain =>
      if (Random.int(100) < 1) {
        Some(Volcano);
      } else {
        None;
      }
    | _ => None
    };

let make_beaches: biome_rule =
  (here, neighbors) =>
    switch (here) {
    | Plain
        when
          Array.exists(x => x == Deep_ocean || x == Shallow_ocean, neighbors) =>
      Some(Beach)
    | _ => None
    };

let make_foothills: biome_rule =
  (here, neighbors) =>
    switch (here) {
    | Plain when Array.mem(Mountain, neighbors) => Some(Hill)
    | _ => None
    };

let make_shallow_ocean: biome_rule =
  (here, neighbors) =>
    switch (here) {
    | Deep_ocean when Array.exists(x => x != Deep_ocean, neighbors) =>
      Some(Shallow_ocean)
    | _ => None
    };

let all_rules = [
  make_volcanoes,
  make_beaches,
  make_foothills,
  make_shallow_ocean,
];

let apply_rules =
    (rules: list(biome_rule), old_grid: Grid.t(tile)): Grid.t(tile) => {
  Grid.init(
    old_grid.width,
    old_grid.height,
    (x, y) => {
      let old = Grid.at(old_grid, x, y);
      let neighbors = Grid.neighbors(old_grid, x, y);
      cascade(rules, old, neighbors) |> Option.value(_, ~default=old);
    },
  );
};

let run_phase = (continent: Grid.t(Continent.tile)): Grid.t(tile) => {
  convert(continent)
  |> apply_rules([make_foothills, make_shallow_ocean], _)
  |> Util.times(Subdivide.subdivide, 1, _)
  |> apply_rules([make_volcanoes, make_beaches], _)
  |> Util.times(Subdivide.subdivide, 0, _);
};