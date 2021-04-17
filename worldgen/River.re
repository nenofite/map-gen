open Core_kernel;

module Tile = {
  module T = {
    [@deriving (eq, ord, sexp, bin_io)]
    type t = {
      elevation: int,
      river: bool,
      ocean: bool,
    };
  };
  module T1 = {
    include T;
    include Comparable.Make(T);
  };
  module T2 = {
    include T1;
    module Grid = Grid.Make0(T1);
  };
  include T2;
};

[@deriving bin_io]
type tile = Tile.t;
let has_river = (t: tile) => t.river;
let has_ocean = (t: tile) => t.ocean;
let has_water = (t: tile) => has_river(t) || has_ocean(t);

let empty_tile = Tile.{elevation: 0, river: false, ocean: false};

let min_river_length = 100;
let min_source_elevation = Heightmap.mountain_level - 5;
let max_source_elevation = Heightmap.mountain_level + 5;

let increase_width_every = 100;

let colorize = (tile: tile): int => {
  let base = Heightmap.colorize(tile.elevation * Heightmap.precision_coef);
  let blue = 0x0000FF;
  if (has_water(tile)) {
    Color.blend(base, blue, 0.5);
  } else {
    base;
  };
};

let colorize_hiprec = (tile: tile): int => {
  let base = Heightmap.colorize(tile.elevation);
  let blue = 0x0000FF;
  if (has_water(tile)) {
    Color.blend(base, blue, 0.5);
  } else {
    base;
  };
};

let convert = (~alloc_side: int, old_grid: Grid.Mut.t(int)) => {
  Grid.Mut.init(
    ~alloc_side,
    ~side=Grid.Mut.side(old_grid),
    empty_tile,
    ~f=(~x, ~z) => {
      let elevation = Grid.Mut.get(~x, ~z, old_grid);
      let ocean = elevation <= Heightmap.sea_level * Heightmap.precision_coef;
      Tile.{elevation, ocean, river: false};
    },
  );
};

let compare_elevations = (a, b) => {
  let (Tile.{elevation: ae, _}, _, _) = a;
  let (Tile.{elevation: be, _}, _, _) = b;
  Int.compare(ae, be);
};

/** fall_to determines which neighbor the river will flow to */
let fall_to = (here, neighbors) => {
  let neighbors = List.sort(~compare=compare_elevations, neighbors);
  let (lowest, lx, ly) as l = List.hd_exn(neighbors);
  let cmp = compare_elevations(l, (here, 0, 0));
  if (cmp < 0) {
    Ok((lx, ly));
  } else {
    Error(lowest.elevation);
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = (grid: Grid.Mut.t(tile)) => {
  let coords =
    Grid.Mut.fold(grid, ~init=[], ~f=(~x, ~z, acc, here) =>
      if (!has_ocean(here)
          && min_source_elevation
          * Heightmap.precision_coef <= here.elevation
          && here.elevation <= max_source_elevation
          * Heightmap.precision_coef) {
        let neighbors = Grid.Mut.neighbors_coords(grid, ~x, ~z);
        if (Result.is_ok(fall_to(here, neighbors))) {
          [(x, z, Random.bits()), ...acc];
        } else {
          acc;
        };
      } else {
        acc;
      }
    );
  let compare = ((_, _, a), (_, _, b)) => Int.compare(a, b);
  List.sort(~compare, coords) |> List.rev_map(~f=((x, y, _)) => (x, y));
};

/**
  place_river_tile modifies the given tile to have a river on it. If there's
  already a river there, it raises [Invalid_argument]
 */
let place_river_tile = (grid, ~x, ~z, ~elev, ~radius) => {
  let river = Tile.{elevation: elev, river: true, ocean: false};
  let bank = Tile.{elevation: elev, river: false, ocean: false};
  let rd = radius / 2;
  let ru = (radius + 1) / 2;
  for (z in z - rd - 1 to z + ru + 1) {
    for (x in x - rd - 1 to x + ru + 1) {
      let here: tile = Grid.Mut.get(~x, ~z, grid);
      if (here.elevation < elev && !has_water(here)) {
        Grid.Mut.set(~x, ~z, bank, grid);
      };
    };
  };
  for (z in z - rd to z + ru) {
    for (x in x - rd to x + ru) {
      let here: tile = Grid.Mut.get(~x, ~z, grid);
      if (!has_ocean(here)) {
        Grid.Mut.set(~x, ~z, river, grid);
      };
    };
  };
};

let place_river = (path, ~grid) => {
  let rec add_params = (rest, ls, ~elapsed, ~radius) =>
    switch (rest) {
    | [] => ls
    | [(x, z), ...rest] =>
      let (elapsed, radius) =
        if (elapsed >= increase_width_every) {
          (0, radius + 1);
        } else {
          (elapsed + 1, radius);
        };
      let elev = Grid.Mut.get(~x, ~z, grid).Tile.elevation;
      add_params(rest, [(x, z, elev, radius), ...ls], ~elapsed, ~radius);
    };
  let path_with_params =
    add_params(path, [], ~elapsed=0, ~radius=1) |> List.rev;
  List.iter(path_with_params, ~f=((x, z, elev, radius)) => {
    place_river_tile(grid, ~x, ~z, ~elev, ~radius)
  });
};

let raise_elevation_to = (elevation, ~x, ~z, ~grid) => {
  Grid.Mut.update(grid, ~x, ~z, ~f=here => Tile.{...here, elevation})
  |> ignore;
};

/**
  flow_river moves downhill until the river reaches the ocean, another river,
  or a local minimum. If the river reaches a local minimum before reaching
  the ocean, it deposits sediment there and tries again from the previous
  tile. If the river reaches an ocean or another river, it succeeds and
  returns the path it took. The returned path goes from ocean to source.
 */
let flow_river = (grid, start_x, start_z) => {
  let start_elev = Grid.Mut.get(~x=start_x, ~z=start_z, grid).Tile.elevation;
  let rec go = (x, z, prev_elev, path, ~length_so_far, ~tries) => {
    ignore(prev_elev);
    let here = Grid.Mut.get(~x, ~z, grid);
    if (!has_water(here)) {
      let next_path = [(x, z), ...path];
      let neighbors = Grid.Mut.neighbors_coords(grid, ~x, ~z);
      switch (fall_to(here, neighbors)) {
      | Ok((next_x, next_z)) =>
        assert(next_x != x || next_z != z);
        go(
          next_x,
          next_z,
          here.elevation,
          next_path,
          ~length_so_far=length_so_far + 1,
          ~tries,
        );
      | Error(lowest_elev) =>
        raise_elevation_to(lowest_elev + 1, ~x, ~z, ~grid);
        if (tries <= max(10, length_so_far)) {
          go(
            start_x,
            start_z,
            start_elev,
            [],
            ~length_so_far=0,
            ~tries=tries + 1,
          );
        } else {
          Stats.record(`Failed_river_length, List.length(path));
          Stats.record(`C("tries before failing"), tries);
          None;
        };
      };
    } else if (List.length(path) > min_river_length) {
      Stats.record_custom(
        "river length x tries",
        (f, ()) => Printf.fprintf(f, "%d,%d", List.length(path), tries),
        (),
      );
      Stats.record(`C("tries before succeeding"), tries);
      /* We've made a river! Only accept if it's long enough */
      Some(List.rev(path));
    } else {
      Stats.record(`Failed_river_length, List.length(path));
      Stats.record(`C("tries before failing"), tries);
      None;
      /* Too short */
    };
  };
  go(start_x, start_z, start_elev, [], ~length_so_far=0, ~tries=0);
};

let try_and_place_longest_river = (sources, ~amount_to_try, ~grid) => {
  let rec try_rivers = (rivers, amount, sources) =>
    switch (sources) {
    | [] => (rivers, sources)
    | _ when amount >= amount_to_try => (rivers, sources)
    | [(x, z), ...sources] =>
      switch (flow_river(grid, x, z)) {
      | Some(r) =>
        Tale.logf("Flowed river %d of %d", amount, amount_to_try);
        try_rivers([(r, List.length(r)), ...rivers], amount + 1, sources);
      | None => try_rivers(rivers, amount, sources)
      }
    };
  let (tries, sources) = try_rivers([], 0, sources);
  tries
  |> List.max_elt(~compare=((_, al), (_, bl)) => Int.compare(bl, al))
  |> Option.iter(~f=((r, _l)) => place_river(r, ~grid));
  sources;
};

let add_rivers =
    (grid, ~amount_to_try_each, ~amount_to_keep): Grid.Mut.t(tile) => {
  Tale.log("Adding rivers...");
  let sources = river_sources(grid);
  Mg_util.Range.fold(1, amount_to_keep, sources, (sources, i) => {
    Tale.blockf("Placing river %d of %d", i, amount_to_keep, ~f=() => {
      try_and_place_longest_river(
        sources,
        ~amount_to_try=amount_to_try_each,
        ~grid,
      )
    })
  })
  |> ignore;
  grid;
};

let phase = (~alloc_side, input) => {
  Tale.block("Flow rivers", ~f=() => {
    let m = convert(~alloc_side, input);
    add_rivers(m, ~amount_to_try_each=10, ~amount_to_keep=10);
  });
};
