type niceness =
  | Dirt
  | Paved;

type road = {
  elevation: int,
  niceness,
};

type t = {
  pois: list((int, int)),
  roads: Sparse_grid.t(road),
};

let colorizer =
  fun
  | None => 0
  | Some(_road) => 0xFFFFFF;

let rec all_pairs = (list, result) =>
  switch (list) {
  | [] => result
  | [a, ...list] =>
    let result = List.map(b => (a, b), list) @ result;
    all_pairs(list, result);
  };

let heuristic = (base: Grid.t(Base_overlay.tile), (ax, ay), (bx, by)) => {
  let a_elev = Grid.at(base, ax, ay).elevation;
  let b_elev = Grid.at(base, bx, by).elevation;
  A_star.distance_3d((ax, ay, a_elev), (bx, by, b_elev));
};

let edge_cost = (base: Grid.t(Base_overlay.tile), (ax, ay), (bx, by)) => {
  let a_elev = Grid.at(base, ax, ay).elevation;
  switch (Grid.at(base, bx, by)) {
  | {river: false, ocean: false, elevation: b_elev} =>
    let elev_diff = abs(a_elev - b_elev);
    switch (elev_diff) {
    | 0 => Some(1.)
    | 1 => Some(2.)
    | _ => None
    };
  | {river: true, _}
  | {ocean: true, _} => None
  };
};

let rec place_road = (roads, path) =>
  switch (path) {
  | [] => roads
  | [(x, y), ...path] =>
    let road_tile = {elevation: 0, niceness: Dirt}; /* TODO */
    let roads = Sparse_grid.put(roads, x, y, road_tile);
    place_road(roads, path);
  };

let prepare = (base: Grid.t(Base_overlay.tile), ()) => {
  /* Use a point cloud to get points of interest */
  print_endline("Making points of interest");
  let pois =
    Point_cloud.init(
      ~width=base.side, ~height=base.side, ~spacing=256, (_, _) =>
      ()
    ).
      points
    |> List.filter_map((Point_cloud.{x, y, _}) => {
         let x = int_of_float(x);
         let y = int_of_float(y);
         switch (Grid.at(base, x, y)) {
         | {ocean: false, river: false, _} => Some((x, y))
         | _ => None
         };
       });
  /* Run A* to go from each point to each other point */
  let roads = Sparse_grid.make(base.side);
  let poi_pairs = all_pairs(pois, []) |> Util.take(10, _);
  print_endline("Pathfinding roads");
  let roads =
    List.fold_left(
      (roads, (start, goal)) => {
        switch (
          A_star.run(
            ~grid_side=base.side,
            ~start,
            ~goal,
            ~edge_cost=edge_cost(base),
            ~heuristic=heuristic(base),
          )
        ) {
        | Some(path) =>
          print_endline("found a road");
          place_road(roads, path); /* Add the path to the grid */
        | None =>
          print_endline("couldn't find road");
          roads;
        }
      },
      roads,
      poi_pairs,
    );
  print_endline("Drawing");
  Draw.draw_sparse_grid(colorizer, "roads.ppm", roads);
  /* TODO: add "stairs" to large increases in elev */
  {pois, roads};
};

let apply_region = (_base, _state, _args) => ();

let overlay = base =>
  Overlay.make("roads", prepare(base), apply_region(base));