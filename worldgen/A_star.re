type node = {
  coord: (int, int),
  came_from: (int, int),
  g_score: float,
  f_score: float,
};

let rec reconstruct_path = (current, nodes, path) => {
  /* Add this node to the path */
  let path = [current.coord, ...path];
  if (current.coord != current.came_from) {
    let (cf_x, cf_y) = current.came_from;
    let came_from =
      switch (Sparse_grid.at(nodes, cf_x, cf_y)) {
      | Some(cf) => cf
      | None => raise(Failure("could not reconstruct path"))
      };
    reconstruct_path(came_from, nodes, path);
  } else {
    /* This is the start node */
    path;
  };
};

let rec run' =
        (
          ~grid_side,
          ~goalf,
          ~edge_cost,
          ~heuristic,
          ~nodes,
          ~open_set,
          ~max_iters,
        ) =>
  max_iters <= 0
    ? None  /* TODO if we run out of iters, maybe check if goal is in the open set and take the sub-optimal path */
    : (
      switch (Priority_queue.extract(open_set)) {
      | (None, _open_set) => None /* no path */
      | (Some(current), _open_set) when goalf(current.coord) =>
        Some(reconstruct_path(current, nodes, [])) /* found a path! */
      | (Some(current), open_set) =>
        let (current_x, current_y) = current.coord;
        let (nodes, open_set) =
          List.fold_left(
            ((nodes, open_set) as no_change, (dx, dy)) => {
              let (neigh_x, neigh_y) as neigh_coord = (
                current_x + dx,
                current_y + dy,
              );
              if (Sparse_grid.is_within(grid_side, neigh_x, neigh_y)) {
                switch (edge_cost(current.coord, neigh_coord)) {
                | None => no_change /* cannot traverse */
                | Some(c) =>
                  let new_g_score = current.g_score +. c;
                  switch (Sparse_grid.at(nodes, neigh_x, neigh_y)) {
                  | Some({g_score: old_g_score, _})
                      when new_g_score >= old_g_score => no_change
                  | None
                  | Some(_) =>
                    let new_node = {
                      coord: neigh_coord,
                      came_from: current.coord,
                      g_score: new_g_score,
                      f_score: new_g_score +. heuristic(neigh_coord),
                    };
                    let nodes =
                      Sparse_grid.put(nodes, neigh_x, neigh_y, new_node);
                    let open_set =
                      Priority_queue.insert(
                        open_set,
                        new_node.f_score,
                        new_node,
                      );
                    (nodes, open_set);
                  };
                };
              } else {
                /* goes off grid */
                no_change;
              };
            },
            (nodes, open_set),
            Grid.Griddable.four_directions,
          );
        run'(
          ~grid_side,
          ~goalf,
          ~edge_cost,
          ~heuristic,
          ~nodes,
          ~open_set,
          ~max_iters=max_iters - 1,
        );
      }
    );

/**
  uses the A* algorithm to find a path between any point in starts and any
  point in goal. See run for more details
*/
let run_multi =
    (~grid_side, ~starts, ~goalf, ~edge_cost, ~heuristic)
    : option(list((int, int))) => {
  let start_nodes =
    List.map(
      coord =>
        {coord, came_from: coord, g_score: 0., f_score: heuristic(coord)},
      starts,
    );
  let open_set =
    Priority_queue.(
      List.fold_left(
        (open_set, start_node) =>
          insert(open_set, start_node.f_score, start_node),
        empty,
        start_nodes,
      )
    );
  let nodes =
    Sparse_grid.(
      List.fold_left(
        (nodes, start_node) => {
          let (x, y) = start_node.coord;
          put(nodes, x, y, start_node);
        },
        make(grid_side),
        start_nodes,
      )
    );
  run'(
    ~grid_side,
    ~goalf,
    ~edge_cost,
    ~heuristic,
    ~nodes,
    ~open_set,
    ~max_iters=100000,
  );
};

/**
  run uses the A* algorithm to find a path from start to goal. It operates on
  a grid moving in cardinal directions.

  edge_cost returns the actual cost to from one coord to another, or None if
  not traversable.

  heuristic returns the estimated cost from one coord to the goal.

  This does not currently work with grid wrapping, partly because the
  heuristic would need to be a bit more complicated and partly because that
  is currently unneeded.
 */
let run =
    (~grid_side, ~start, ~goal, ~edge_cost, ~heuristic)
    : option(list((int, int))) =>
  run_multi(
    ~grid_side,
    ~starts=[start],
    ~goalf=coord => coord == goal,
    ~edge_cost,
    ~heuristic,
  );

let distance_2d = ((ax, ay), (bx, by)) => {
  let f = float_of_int;
  sqrt((f(bx) -. f(ax)) ** 2. +. (f(by) -. f(ay)) ** 2.);
};

let distance_3d = ((ax, ay, az), (bx, by, bz)) => {
  let f = float_of_int;
  sqrt(
    (f(bx) -. f(ax))
    ** 2.
    +. (f(by) -. f(ay))
    ** 2.
    +. (f(bz) -. f(az))
    ** 2.,
  );
};

let test_print_path =
  fun
  | None => print_endline("no path")
  | Some(path) =>
    List.iter(((x, y)) => Printf.printf("%d,%d\n", x, y), path);

let%expect_test "simple path" = {
  run(
    ~grid_side=4,
    ~start=(0, 0),
    ~goal=(3, 0),
    ~edge_cost=(a, b) => Some(distance_2d(a, b)),
    ~heuristic=distance_2d((3, 0)),
  )
  |> test_print_path;
  %expect
  {|
    0,0
    1,0
    2,0
    3,0
  |};
};

let%expect_test "no path" = {
  run(
    ~grid_side=4,
    ~start=(0, 0),
    ~goal=(3, 0),
    ~edge_cost=(_, _) => None,
    ~heuristic=distance_2d((3, 0)),
  )
  |> test_print_path;
  %expect
  {| no path |};
};
