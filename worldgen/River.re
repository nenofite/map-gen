open Core_kernel;

type state = {
  river_depth: Grid.Mut.t(int),
  elevation: Grid.Mut.t(int),
};

let min_river_length = 100;
let min_source_elevation = Heightmap.mountain_level - 5;
let max_source_elevation = Heightmap.mountain_level + 5;

let increase_width_every = 100;

let side = state => Grid.Mut.side(state.elevation);

let elevation_at = (~x, ~z, state) => Grid.Mut.get(~x, ~z, state.elevation);

let ocean_at = (~x, ~z, state) => {
  Grid.Mut.get(~x, ~z, state.elevation) <= Heightmap.sea_level
  * Heightmap.precision_coef;
};

let river_at = (~x, ~z, state) => {
  Grid.Mut.get(~x, ~z, state.river_depth) > 0;
};

let river_depth_at = (~x, ~z, state) =>
  Grid.Mut.get(~x, ~z, state.river_depth);

let convert = (elevation: Grid.Mut.t(int)) => {
  let river_depth = Grid.Mut.create(~side=Grid.Mut.side(elevation), 0);
  {elevation, river_depth};
};

let compare_elevations = (a, b) => {
  let (ae, _, _) = a;
  let (be, _, _) = b;
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
    Error(lowest);
  };
};

/** river_sources gets all potential river sources on the map, in random order */
let river_sources = state => {
  let coords =
    Grid.Mut.fold(state.elevation, ~init=[], ~f=(~x, ~z, acc, here) =>
      if (!ocean_at(~x, ~z, state)
          && min_source_elevation
          * Heightmap.precision_coef <= here
          && here <= max_source_elevation
          * Heightmap.precision_coef) {
        let neighbors = Grid.Mut.neighbors_coords(state.elevation, ~x, ~z);
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

let place_river_tile = (state, ~x, ~z, ~elev, ~radius) => {
  let rd = radius / 2;
  let ru = (radius + 1) / 2;
  let {elevation, river_depth} = state;
  for (z in z - rd - 1 to z + ru + 1) {
    for (x in x - rd - 1 to x + ru + 1) {
      let here_elev = Grid.Mut.get(~x, ~z, elevation);
      let here_depth = Grid.Mut.get(~x, ~z, river_depth);
      if (here_elev < elev && here_depth == 0) {
        Grid.Mut.set(~x, ~z, elev, elevation);
      };
    };
  };
  for (z in z - rd to z + ru) {
    for (x in x - rd to x + ru) {
      let here_elev = Grid.Mut.get(~x, ~z, elevation);
      let here_depth = Grid.Mut.get(~x, ~z, river_depth);
      let target_depth = 1;
      // TODO
      ignore(here_elev);
      if (!ocean_at(~x, ~z, state) && here_depth < target_depth) {
        Grid.Mut.set(~x, ~z, target_depth, river_depth);
      };
    };
  };
};

let place_river = (path, ~state) => {
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
      let elev = Grid.Mut.get(~x, ~z, state.elevation);
      add_params(rest, [(x, z, elev, radius), ...ls], ~elapsed, ~radius);
    };
  let path_with_params =
    add_params(path, [], ~elapsed=0, ~radius=1) |> List.rev;
  List.iter(path_with_params, ~f=((x, z, elev, radius)) => {
    place_river_tile(state, ~x, ~z, ~elev, ~radius)
  });
};

let raise_elevation_to = (elevation, ~x, ~z, ~state) => {
  // TODO misnomer?
  Grid.Mut.set(elevation, state.elevation, ~x, ~z);
};

/**
  flow_river moves downhill until the river reaches the ocean, another river,
  or a local minimum. If the river reaches a local minimum before reaching
  the ocean, it deposits sediment there and tries again from the previous
  tile. If the river reaches an ocean or another river, it succeeds and
  returns the path it took. The returned path goes from ocean to source.
 */
let flow_river = (state, start_x, start_z) => {
  let rec go = (x, z, path, ~length_so_far, ~tries) =>
    if (!ocean_at(~x, ~z, state) && !river_at(~x, ~z, state)) {
      let here = Grid.Mut.get(~x, ~z, state.elevation);
      let next_path = [(x, z), ...path];
      let neighbors = Grid.Mut.neighbors_coords(state.elevation, ~x, ~z);
      switch (fall_to(here, neighbors)) {
      | Ok((next_x, next_z)) =>
        assert(next_x != x || next_z != z);
        go(
          next_x,
          next_z,
          next_path,
          ~length_so_far=length_so_far + 1,
          ~tries,
        );
      | Error(lowest_elev) =>
        raise_elevation_to(lowest_elev + 1, ~x, ~z, ~state);
        if (tries <= max(10, length_so_far)) {
          go(start_x, start_z, [], ~length_so_far=0, ~tries=tries + 1);
        } else {
          None;
        };
      };
    } else if (List.length(path) > min_river_length) {
      /* We've made a river! Only accept if it's long enough */
      Some(
        List.rev(path),
      );
    } else {
      None;
          /* Too short */
    };
  go(start_x, start_z, [], ~length_so_far=0, ~tries=0);
};

let rec remove_first = (ls, ~f) =>
  switch (ls) {
  | [] => []
  | [a, ...rest] when f(a) => rest
  | [a, ...rest] => [a, ...remove_first(rest, ~f)]
  };

let try_and_place_longest_river = (sources, ~amount_to_try, ~state) => {
  let rec try_rivers = (rivers, amount, good_sources, sources) =>
    switch (sources) {
    | [] => (rivers, good_sources, sources)
    | _ when amount >= amount_to_try => (rivers, good_sources, sources)
    | [(x, z) as source, ...rest_sources] =>
      switch (flow_river(state, x, z)) {
      | Some(r) =>
        Tale.logf("Flowed river %d of %d", amount, amount_to_try);
        try_rivers(
          [(r, List.length(r), source), ...rivers],
          amount + 1,
          [source, ...good_sources],
          rest_sources,
        );
      | None => try_rivers(rivers, amount, good_sources, rest_sources)
      }
    };
  let (tries, good_sources, rest_sources) = try_rivers([], 0, [], sources);
  let best =
    List.max_elt(tries, ~compare=((_, al, _), (_, bl, _)) =>
      Int.compare(bl, al)
    );
  switch (best) {
  | None => rest_sources
  | Some((r, _l, source)) =>
    place_river(r, ~state);
    remove_first(good_sources, ~f=s => Poly.equal(s, source)) @ rest_sources;
  };
};

let add_rivers = (state, ~amount_to_try_each, ~amount_to_keep) => {
  Tale.log("Adding rivers...");
  let sources = river_sources(state);
  let _leftover =
    Mg_util.Range.fold(1, amount_to_keep, sources, (sources, i) => {
      Tale.blockf("Placing river %d of %d", i, amount_to_keep, ~f=() => {
        try_and_place_longest_river(
          sources,
          ~amount_to_try=amount_to_try_each,
          ~state,
        )
      })
    });
  ();
};

let phase = input => {
  Tale.block("Flow rivers", ~f=() => {
    let state = convert(input);
    add_rivers(state, ~amount_to_try_each=30, ~amount_to_keep=10);
    state;
  });
};
