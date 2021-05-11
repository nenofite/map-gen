open Core_kernel;

type state = {
  inflow: Grid.Mut.t(int),
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
  let inflow = Grid.Mut.create(~side=Grid.Mut.side(elevation), 0);
  {elevation, river_depth, inflow};
};

/** fall_to determines which neighbor the river will flow to */
let fall_to = (~x, ~z, state) => {
  let here = elevation_at(~x, ~z, state);
  let rec go = (lowest, lx, lz, neighbors) =>
    switch (neighbors) {
    | [(dx, dz), ...neighbors] =>
      let nelev = elevation_at(~x=x + dx, ~z=z + dz, state);
      if (nelev < lowest) {
        go(nelev, x + dx, z + dz, neighbors);
      } else {
        go(lowest, lx, lz, neighbors);
      };
    | [] =>
      if (lowest < here) {
        Ok((lx, lz));
      } else {
        Error(lowest);
      }
    };
  go(here, 0, 0, Grid.Griddable.eight_directions);
};

let place_river_tile = (state, ~x as cx, ~z as cz, ~elev, ~radius) => {
  let rd = radius / 2;
  let ru = (radius + 1) / 2;
  let {elevation, river_depth, inflow: _} = state;
  for (z in cz - rd - 1 to cz + ru + 1) {
    for (x in cx - rd - 1 to cx + ru + 1) {
      let here_elev = Grid.Mut.get(~x, ~z, elevation);
      let here_depth = Grid.Mut.get(~x, ~z, river_depth);
      if (here_elev < elev && here_depth == 0) {
        Grid.Mut.set(~x, ~z, elev, elevation);
      };
    };
  };
  for (z in cz - rd to cz + ru) {
    for (x in cx - rd to cx + ru) {
      let dist = abs(x - cx) + abs(z - cz);
      let here_depth = Grid.Mut.get(~x, ~z, river_depth);
      let target_depth = max(1, radius - dist);
      if (!ocean_at(~x, ~z, state)) {
        Grid.Mut.set(~x, ~z, elev, elevation);
        if (here_depth < target_depth) {
          Grid.Mut.set(~x, ~z, target_depth, river_depth);
        };
      };
    };
  };
};

let place_river = (path, ~state) => {
  Tale.logf("Placing river with length %d", List.length(path));
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

let incr_inflow = (~x, ~z, state) =>
  Grid.Mut.update(~x, ~z, ~f=Int.succ, state.inflow) |> ignore;

let dig_raindrop = (~x, ~z, state) => {
  let rec go = (x, z) =>
    if (!ocean_at(~x, ~z, state)) {
      switch (fall_to(~x, ~z, state)) {
      | Ok((next_x, next_z)) =>
        assert(next_x != x || next_z != z);
        go(next_x, next_z);
      | Error(lowest_elev) =>
        Grid.Mut.set(lowest_elev + 1, state.elevation, ~x, ~z)
      };
    };
  go(x, z);
};
let flow_raindrop = (~x, ~z, state) => {
  let rec go = (x, z) => {
    incr_inflow(~x, ~z, state);
    if (!ocean_at(~x, ~z, state)) {
      switch (fall_to(~x, ~z, state)) {
      | Ok((next_x, next_z)) =>
        assert(next_x != x || next_z != z);
        go(next_x, next_z);
      | Error(_lowest_elev) => ()
      };
    };
  };
  go(x, z);
};

let flow_all_raindrops = state => {
  let dig_revs = 10;
  let starts = Point_cloud.make_int_list(~side=side(state), ~spacing=4, ());
  for (i in 1 to dig_revs) {
    Tale.blockf("Digging %d of %d", i, dig_revs, ~f=() => {
      starts |> List.iter(~f=((x, z)) => dig_raindrop(~x, ~z, state))
    });
  };
  Tale.block("Flowing", ~f=() => {
    starts |> List.iter(~f=((x, z)) => flow_raindrop(~x, ~z, state))
  });
};

let reconstruct_biggest_flow = (~x, ~z, state) => {
  let rec go = (x, z, ls) => {
    let ls = [(x, z), ...ls];
    let here = Grid.Mut.get(~x, ~z, state.elevation);
    let best_upstream =
      Grid.Mut.neighbors_coords(state.elevation, ~x, ~z)
      |> List.filter_map(~f=((elev, x, z)) =>
           if (elev > here) {
             let inflow = Grid.Mut.get(~x, ~z, state.inflow);
             Some((inflow, x, z));
           } else {
             None;
           }
         )
      |> List.max_elt(~compare=((a, _, _), (b, _, _)) => Int.compare(a, b));
    switch (best_upstream) {
    | Some((_, x, z)) => go(x, z, ls)
    | None => ls
    };
  };
  go(x, z, []);
};

let second_biggest_flow = (~x, ~z, state) => {
  let here = Grid.Mut.get(~x, ~z, state.elevation);
  let best_upstream =
    Grid.Mut.neighbors_coords(state.elevation, ~x, ~z)
    |> List.filter_map(~f=((elev, x, z)) =>
         if (elev > here) {
           let inflow = Grid.Mut.get(~x, ~z, state.inflow);
           Some((inflow, x, z));
         } else {
           None;
         }
       )
    |> List.sort(~compare=((a, _, _), (b, _, _)) => Int.compare(b, a))
    |> List.drop(_, 1)
    |> List.hd;
  switch (best_upstream) {
  | Some((_, x, z)) => reconstruct_biggest_flow(~x, ~z, state)
  | None => failwith("no second best")
  };
};

let reconstruct_fork = (~river, ~offset, state) => {
  let root = List.drop(river, offset);
  switch (List.hd(root)) {
  | Some((x, z)) => second_biggest_flow(~x, ~z, state)
  | None => []
  };
};

let river_and_fork = (~x, ~z, state) => {
  let river = reconstruct_biggest_flow(~x, ~z, state);
  [
    river,
    reconstruct_fork(~river, ~offset=10, state),
    reconstruct_fork(~river, ~offset=20, state),
    reconstruct_fork(~river, ~offset=30, state),
    reconstruct_fork(~river, ~offset=50, state),
    reconstruct_fork(~river, ~offset=80, state),
    reconstruct_fork(~river, ~offset=130, state),
    reconstruct_fork(~river, ~offset=210, state),
    reconstruct_fork(~river, ~offset=340, state),
  ];
};

let biggest_ocean_inflows = state => {
  Grid.Mut.fold(state.inflow, ~init=[], ~f=(~x, ~z, ls, inflow) =>
    if (inflow > 0 && ocean_at(~x, ~z, state)) {
      [(inflow, x, z), ...ls];
    } else {
      ls;
    }
  )
  |> List.sort(~compare=((a, _, _), (b, _, _)) => Int.compare(b, a))
  |> List.take(_, 5)
  |> List.map(~f=((_, x, z)) => (x, z));
};

let rec remove_first = (ls, ~f) =>
  switch (ls) {
  | [] => []
  | [a, ...rest] when f(a) => rest
  | [a, ...rest] => [a, ...remove_first(rest, ~f)]
  };

let add_rivers = state => {
  Tale.block("Flowing raindrops", ~f=() => {flow_all_raindrops(state)});
  List.iteri(biggest_ocean_inflows(state), ~f=(i, r) =>
    Tale.blockf(
      "Placing river %d",
      i,
      ~f=() => {
        let (x, z) = r;
        let rivers =
          Tale.block("Reconstructing", ~f=() =>
            river_and_fork(~x, ~z, state)
          );
        Tale.block("Placing", ~f=() =>
          List.iter(rivers, ~f=place_river(~state))
        );
      },
    )
  );
};

let phase = input => {
  Tale.block("Flow rivers", ~f=() => {
    let state = convert(input);
    add_rivers(state);
    state;
  });
};
