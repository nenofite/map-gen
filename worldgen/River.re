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

let min_river_length = 30;
let min_source_elevation = Heightmap.mountain_level - 5;
let max_source_elevation = Heightmap.mountain_level + 5;

let colorize = (tile: tile): int => {
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
      let ocean = elevation <= Heightmap.sea_level;
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
  let (_, lx, ly) as l = List.hd_exn(neighbors);
  let cmp = compare_elevations(l, (here, 0, 0));
  if (cmp < 0) {
    Some((lx, ly));
  } else {
    None;
  };
};

let rec array_find = (f, array, i) =>
  if (i < Array.length(array)) {
    if (f(array[i])) {
      Some(array[i]);
    } else {
      array_find(f, array, i + 1);
    };
  } else {
    None;
  };

/**
  fall_to_with_flat_dir determines which neighbor the river will flow to:

  1. If there is lower land, move in that direction.
  2. Otherwise, if [flat_dir] is level with us, move in that direction.
  3. Otherwise, pick a random flat direction.
 */
let fall_to_with_flat_dir = (here, neighbors, flat_dir) => {
  let (flat_dir_x, flat_dir_y) = flat_dir;
  assert(flat_dir_x != 0 || flat_dir_y != 0);
  let flat_dir_n =
    List.find_exn(
      ~f=((_, x, y)) => x == flat_dir_x && y == flat_dir_y,
      neighbors,
    );

  let neighbors = List.sort(~compare=compare_elevations, neighbors);
  let (_, lowest_x, lowest_y) as lowest = List.hd_exn(neighbors);

  if (compare_elevations(lowest, (here, 0, 0)) < 0) {
    Some((lowest_x, lowest_y));
  } else if (compare_elevations(flat_dir_n, (here, 0, 0)) <= 0) {
    Some(flat_dir);
  } else {
    Mg_util.shuffle(neighbors)
    |> List.find(~f=x => compare_elevations(x, (here, 0, 0)) <= 0)
    |> Option.map(~f=((_, x, y)) => (x, y));
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = (grid: Grid.Mut.t(tile)) => {
  let coords =
    Grid.Mut.fold(grid, ~init=[], ~f=(~x, ~z, acc, here) =>
      if (!has_ocean(here)
          && min_source_elevation <= here.elevation
          && here.elevation <= max_source_elevation) {
        let neighbors = Grid.Mut.neighbors_coords(grid, ~x, ~z);
        if (Option.is_some(fall_to(here, neighbors))) {
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
let place_river_tile = (grid, x, z) => {
  Grid.Mut.update(grid, ~x, ~z, ~f=here =>
    switch (here) {
    | Tile.{river: true, _} =>
      raise(Invalid_argument("Tile already has river"))
    | Tile.{river: false, _} as here => {...here, river: true}
    }
  );
};

/**
  deposit_sediment increases the elevation by 1 at the given coordinate.
 */
let deposit_sediment = (grid, x, z) => {
  Grid.Mut.update(grid, ~x, ~z, ~f=here =>
    Tile.{...here, elevation: here.elevation + 1}
  );
};

/**
  current_flow_direction gets the river's current direction given path and
  current coordinate. If the path is empty, this defaults to south.
 */
let current_flow_direction = (path, x, y) => {
  switch (path) {
  | [(px, py), ..._] => (x - px, y - py)
  | [] => (0, 1)
  };
};

/**
  flow_river moves downhill until the river reaches the ocean, another river,
  or a local minimum. If the river reaches a local minimum before reaching
  the ocean, it deposits sediment there and tries again from the previous
  tile. If the river reaches an ocean or another river, it succeeds and
  returns the path it took.
 */
let rec flow_river = (grid, path, x, z) => {
  let here = Grid.Mut.get(~x, ~z, grid);
  if (!has_water(here)) {
    let next_path = [(x, z), ...path];
    let neighbors = Grid.Mut.neighbors_offsets(grid, ~x, ~z);
    let flat_dir = current_flow_direction(path, x, z);
    switch (fall_to_with_flat_dir(here, neighbors, flat_dir)) {
    | Some((dx, dz)) =>
      let side = Grid.Mut.side(grid);
      let next_x = (x + dx) % side;
      let next_z = (z + dz) % side;
      assert(next_x != x || next_z != z);
      if (!List.exists(~f=((lx, lz)) => lx == next_x && lz == next_z, path)) {
        flow_river(grid, next_path, next_x, next_z);
      } else {
        /* We formed a loop. Deposit sediment and backtrack. */
        let grid = deposit_sediment(grid, x, z);
        switch (path) {
        | [(previous_x, previous_y), ...previous_path] =>
          flow_river(grid, previous_path, previous_x, previous_y)
        | [] => flow_river(grid, [], x, z)
        };
      };
    | None =>
      /* We're stuck in a ditch. Deposit sediment and backtrack. */
      let grid = deposit_sediment(grid, x, z);
      switch (path) {
      | [(previous_x, previous_y), ...previous_path] =>
        flow_river(grid, previous_path, previous_x, previous_y)
      | [] => flow_river(grid, [], x, z)
      };
    };
  } else if (List.length(path) > min_river_length) {
    /* We've made a river! Only accept if it's long enough */
    Some(path);
  } else {
    None;
        /* Too short */
  };
};

/**
  river finds a non-ocean tile with an elevation between plains and
  mountains, then creates a river with the given id there. The river is only
  kept if it can reach the ocean.
 */
let river = (grid: Grid.Mut.t(tile), _id: int, source_x: int, source_y: int) => {
  switch (flow_river(grid, [], source_x, source_y)) {
  | Some(path) =>
    Some(
      List.fold(
        ~f=(grid, (x, y)) => place_river_tile(grid, x, y),
        ~init=grid,
        path,
      ),
    )
  | None => None
  };
};

let add_rivers = (grid, target_amount): Grid.Mut.t(tile) => {
  let sources = river_sources(grid);
  let target_amount = min(target_amount, List.length(sources));

  let rec go = (amount, sources) =>
    switch (sources) {
    | [] => amount
    | _ when amount >= target_amount => amount
    | [(x, z), ...sources] =>
      switch (river(grid, amount, x, z)) {
      | Some(_grid) => go(amount + 1, sources)
      | None => go(amount, sources)
      }
    };
  let succeeded = go(0, sources);
  Tale.logf("Successfully placed %d of %d rivers", succeeded, target_amount);
  grid;
};

let phase = (~alloc_side, input) => {
  Tale.block("Flow rivers", ~f=() => {
    let m = convert(~alloc_side, input);
    add_rivers(m, 100);
  });
};
