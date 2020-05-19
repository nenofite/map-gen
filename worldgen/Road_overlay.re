type niceness =
  | Dirt
  | Paved;

type road = {
  elevation: int,
  niceness,
  connections: (bool, bool, bool, bool),
  stairs: (bool, bool, bool, bool),
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

let rec enrich_path = (before, path, result) =>
  switch (path) {
  | [here, ...[after, ..._] as rest] =>
    let (bx, by, bel) = before;
    let (hx, hy, hel) = here;
    let (ax, ay, ael) = after;
    let n_conn = bx == hx && by == hy - 1 || ax == hx && ay == hy - 1;
    let e_conn = by == hy && bx == hx + 1 || ay == hy && ax == hx + 1;
    let s_conn = bx == hx && by == hy + 1 || ax == hx && ay == hy + 1;
    let w_conn = by == hy && bx == hx - 1 || ay == hy && ax == hx - 1;
    let connections = (n_conn, e_conn, s_conn, w_conn);
    let n_stair =
      bx == hx
      && by == hy
      - 1
      && bel == hel
      - 1
      || ax == hx
      && ay == hy
      - 1
      && ael == hel
      - 1;
    let s_stair =
      bx == hx
      && by == hy
      + 1
      && bel == hel
      - 1
      || ax == hx
      && ay == hy
      + 1
      && ael == hel
      - 1;
    let e_stair =
      by == hy
      && bx == hx
      + 1
      && bel == hel
      - 1
      || ay == hy
      && ax == hx
      + 1
      && ael == hel
      - 1;
    let w_stair =
      by == hy
      && bx == hx
      - 1
      && bel == hel
      - 1
      || ay == hy
      && ax == hx
      - 1
      && ael == hel
      - 1;
    let stairs = (n_stair, e_stair, s_stair, w_stair);
    let enriched_here = {
      elevation: hel,
      niceness: Paved, /* TODO */
      connections,
      stairs,
    };
    enrich_path(here, rest, [(hx, hy, enriched_here), ...result]);
  | [] => result
  | [_last] => result /* TODO we chop off the last one */
  };
let enrich_path = path =>
  switch (path) {
  | [first, ...rest] => enrich_path(first, rest, []) /* TODO we chop off the first one */
  | [] => []
  };

let rec add_road_to_grid = (roads, path) =>
  switch (path) {
  | [] => roads
  | [(x, y, road_tile), ...path] =>
    let roads = Sparse_grid.put(roads, x, y, road_tile);
    add_road_to_grid(roads, path);
  };

let place_road = (base: Grid.t(Base_overlay.tile), roads, path) => {
  /* Add elevations */
  let path =
    List.map(
      ((x, y)) => {
        let elevation = Grid.at(base, x, y).elevation;
        (x, y, elevation);
      },
      path,
    );
  /* TODO smooth steps? */
  /* Enrich */
  let path = enrich_path(path);
  /* Add to grid */
  add_road_to_grid(roads, path);
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
          place_road(base, roads, path); /* Add the path to the grid */
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

let place_road_block = (region, x, y, z) => {
  Minecraft.Block_tree.(
    switch (get_block_opt(region, x, y, z)) {
    | None => ()
    | Some(_) =>
      set_block(region, x, y, z, Minecraft.Block.Stone_slab);
      /* Clear space above unless it's also a path (cobblestone) */
      for (dy in 1 to 10) {
        switch (get_block_opt(region, x, y, z)) {
        | None
        | Some(Air)
        | Some(Cobblestone) => ()
        | Some(_) => set_block(region, x, y, z, Minecraft.Block.Air)
        };
      };
    }
  );
};

let place_step_block = (region, x, y, z) => {
  Minecraft.Block_tree.(
    switch (get_block_opt(region, x, y, z)) {
    | None => ()
    | Some(_) =>
      set_block(region, x, y, z, Minecraft.Block.Air);
      set_block(region, x, y - 1, z, Minecraft.Block.Stone_slab);
    }
  );
};

let apply_region =
    (
      base: Grid.t(Base_overlay.tile),
      state,
      args: Minecraft_converter.region_args,
    ) => {
  let region = args.region;
  Sparse_grid.iter(
    state.roads,
    ((gx, gy), road) => {
      let x = gx - args.gx_offset;
      let z = gy - args.gy_offset;
      if (0 <= x && x < args.gsize && 0 <= z && z < args.gsize) {
        open Minecraft.Block_tree;
        let {
          elevation: y,
          connections: (cn, ce, cs, cw),
          stairs: (sn, se, ss, sw),
        } = road;
        place_road_block(region, x, y, z);

        cn ? () : place_road_block(region, x, y, z - 1);
        cn || ce ? () : place_road_block(region, x + 1, y, z - 1);
        ce ? () : place_road_block(region, x + 1, y, z);
        cs || ce ? () : place_road_block(region, x + 1, y, z + 1);
        cs ? () : place_road_block(region, x, y, z + 1);
        cs || cw ? () : place_road_block(region, x - 1, y, z + 1);
        cw ? () : place_road_block(region, x - 1, y, z);
        cn || cw ? () : place_road_block(region, x - 1, y, z - 1);

        if (sn) {
          place_step_block(region, x - 1, y, z - 1);
          place_step_block(region, x, y, z - 1);
          place_step_block(region, x + 1, y, z - 1);
        };
        if (se) {
          place_step_block(region, x + 1, y, z - 1);
          place_step_block(region, x + 1, y, z);
          place_step_block(region, x + 1, y, z + 1);
        };
        if (ss) {
          place_step_block(region, x - 1, y, z + 1);
          place_step_block(region, x, y, z + 1);
          place_step_block(region, x + 1, y, z + 1);
        };
        if (sw) {
          place_step_block(region, x - 1, y, z - 1);
          place_step_block(region, x - 1, y, z);
          place_step_block(region, x - 1, y, z + 1);
        };
      };
    },
  );
};

let overlay = base =>
  Overlay.make("roads", prepare(base), apply_region(base));