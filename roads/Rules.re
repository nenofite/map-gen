open! Core_kernel;

module Coord = {
  module T = {
    [@deriving (eq, ord, hash, sexp, bin_io)]
    type dir2 =
      | Ns
      | Ew;

    /** the direction you must walk to go up the stairs */

    [@deriving (eq, ord, hash, sexp, bin_io)]
    type dir4 =
      | N
      | E
      | S
      | W;

    [@deriving (eq, ord, hash, sexp, bin_io)]
    type structure =
      | Road
      | Stair(dir4)
      | Bridge(dir4);

    [@deriving (eq, ord, hash, sexp, bin_io)]
    type t = {
      x: int,
      y: int,
      z: int,
      structure,
    };
  };

  include T;
  include Comparable.Make_binable(T);
  include Hashable.Make(T);

  let make_road = (~x, ~y, ~z) => {x, y, z, structure: Road};

  let translate = (~dx, ~dz, {x, y, z, structure}) => {
    x: x + dx,
    y,
    z: z + dz,
    structure,
  };
};

include Coord.T;

[@deriving (eq, bin_io)]
type bridge = {
  x: int,
  y: int,
  z: int,
  direction: dir4,
  length: int,
};

/* Costs */
let flat_ground_cost = 10;

let start_stair_cost = 100;

let continue_stair_cost = 10;

let bridge_cost = 100;

let add_margin = (get_obstacle, ~x, ~z) => {
  let margin = 2;
  Mg_util.Range.fold(z - margin, z + margin, Overlay.Canon.Clear, (obs, z) =>
    Mg_util.Range.fold(
      x - margin,
      x + margin,
      obs,
      (obs, x) => {
        let here_obs = get_obstacle(~x, ~z);
        Overlay.Canon.Obstacle.max(obs, here_obs);
      },
    )
  );
};

let get_canon_obstacle = (~x, ~z) => {
  let canon = Overlay.Canon.require();
  Grid.get(x, z, canon.obstacles);
};

let get_canon_elevation = (~x, ~z) => {
  let elevation = Overlay.Canon.require().elevation;
  Grid.get(x, z, elevation);
};

let two_neighbors_of_dir2 =
  fun
  | Ns => [(0, (-1)), (0, 1)]
  | Ew => [((-1), 0), (1, 0)];

let neighbor_of_dir4 =
  fun
  | N => (0, (-1))
  | E => (1, 0)
  | S => (0, 1)
  | W => ((-1), 0);

let two_neighbors_of_dir4 =
  fun
  | N => [(0, 1, (-1)), (0, 0, 1)]
  | E => [((-1), 0, 0), (1, 1, 0)]
  | S => [(0, 0, (-1)), (0, 1, 1)]
  | W => [((-1), 1, 0), (1, 0, 0)];

let opposite_of_dir4 =
  fun
  | N => S
  | E => W
  | S => N
  | W => E;

let make_dir2_exn = (x1, z1, x2, z2) =>
  if (x1 == x2 && z1 != z2) {
    Ns;
  } else if (x1 != x2 && z1 == z2) {
    Ew;
  } else {
    invalid_argf("cannot make dir2: (%d, %d) (%d, %d)", x1, z1, x2, z2, ());
  };

let make_dir4_exn = (x1, z1, x2, z2) =>
  switch (x2 - x1, z2 - z1) {
  | (0, (-1)) => N
  | (1, 0) => E
  | (0, 1) => S
  | ((-1), 0) => W
  | _ =>
    invalid_argf("cannot make dir4: (%d, %d) (%d, %d)", x1, z1, x2, z2, ())
  };

let neighbors = (~get_elevation, ~get_obstacle, {x, y, z, structure}) => {
  let get_obstacle_in_margin = add_margin(get_obstacle);
  let add_ground_neighbor = (~from_stair, ~y, ~nx, ~ny, ~nz, list) => {
    let stair_cost =
      if (from_stair) {continue_stair_cost} else {start_stair_cost};

    let dy = ny - y;
    if (dy == 0) {
      [({x: nx, y: ny, z: nz, structure: Road}, flat_ground_cost), ...list];
    } else if (dy == 1) {
      [
        /* stairs should only generate on the lower block of a slope, so
           forcefully lower the y */
        (
          {
            x: nx,
            y: ny - 1,
            z: nz,
            structure: Stair(make_dir4_exn(x, z, nx, nz)),
          },
          stair_cost,
        ),
        ...list,
      ];
    } else if (dy == (-1)) {
      [
        (
          {
            x: nx,
            y: ny,
            z: nz,
            structure: Stair(opposite_of_dir4(make_dir4_exn(x, z, nx, nz))),
          },
          stair_cost,
        ),
        ...list,
      ];
    } else {
      list;
    };
  };

  let add_bridge_start_neighbor = (~x, ~y, ~z, ~nx, ~ny, ~nz, list) =>
    if (ny <= y) {
      [
        (
          {
            x: nx,
            y: ny,
            z: nz,
            structure: Bridge(make_dir4_exn(x, z, nx, nz)),
          },
          bridge_cost,
        ),
        ...list,
      ];
    } else {
      list;
    };

  let ground_neighbors = (~x, ~y, ~z) =>
    List.fold(
      Grid.Griddable.Helpers.four_directions,
      ~init=[],
      ~f=(acc, (dx, dz)) => {
        let nx = x + dx;
        let nz = z + dz;
        switch (get_obstacle_in_margin(~x=nx, ~z=nz)) {
        | Overlay.Canon.Impassable => acc
        | Bridgeable =>
          let ny = get_elevation(~x=nx, ~z=nz);
          add_bridge_start_neighbor(~y, ~ny, ~x, ~z, ~nx, ~nz, acc);
        | Clear =>
          let ny = get_elevation(~x=nx, ~z=nz);
          add_ground_neighbor(~from_stair=false, ~y, ~ny, ~nx, ~nz, acc);
        };
      },
    );

  let stair_neighbors = (~x, ~y, ~z, stair_dir) =>
    List.fold(
      two_neighbors_of_dir4(stair_dir),
      ~init=[],
      ~f=(acc, (dx, dy, dz)) => {
        let y = y + dy;
        let nx = x + dx;
        let nz = z + dz;
        switch (get_obstacle_in_margin(~x=nx, ~z=nz)) {
        | Overlay.Canon.Impassable => acc
        | Bridgeable =>
          let ny = get_elevation(~x=nx, ~z=nz);
          add_bridge_start_neighbor(~y, ~ny, ~x, ~z, ~nx, ~nz, acc);
        | Clear =>
          let ny = get_elevation(~x=nx, ~z=nz);
          add_ground_neighbor(~from_stair=true, ~y, ~ny, ~nx, ~nz, acc);
        };
      },
    );

  let add_bridge_continue_neighbor = (bridge_dir, ~y, ~ny, ~nx, ~nz, list) =>
    if (ny <= y) {
      [
        ({x: nx, y, z: nz, structure: Bridge(bridge_dir)}, bridge_cost),
        ...list,
      ];
    } else {
      list;
    };

  let add_bridge_end_neighbor = (_bridge, ~y, ~ny, ~nx, ~nz, list) =>
    if (ny == y) {
      [({x: nx, y, z: nz, structure: Road}, flat_ground_cost), ...list];
    } else {
      list;
    };

  let bridge_neighbors = (~x, ~y, ~z, bridge_dir) => {
    let (dx, dz) = neighbor_of_dir4(bridge_dir);
    let nx = x + dx;
    let nz = z + dz;
    switch (get_obstacle_in_margin(~x=nx, ~z=nz)) {
    | Overlay.Canon.Impassable => []
    | Bridgeable =>
      let ny = get_elevation(~x=nx, ~z=nz);
      add_bridge_continue_neighbor(bridge_dir, ~y, ~ny, ~nx, ~nz, []);
    | Clear =>
      let ny = get_elevation(~x=nx, ~z=nz);
      add_bridge_continue_neighbor(bridge_dir, ~y, ~ny, ~nx, ~nz) @@
      add_bridge_end_neighbor(bridge_dir, ~y, ~ny, ~nx, ~nz, []);
    };
  };

  switch (structure) {
  | Road => ground_neighbors(~x, ~y, ~z)
  | Stair(dir) => stair_neighbors(~x, ~y, ~z, dir)
  | Bridge(dir) => bridge_neighbors(~x, ~y, ~z, dir)
  };
};

let put_roads_onto_sparse_grid = (roads: list(t), ~grid) =>
  List.fold(roads, ~init=grid, ~f=(grid, road) =>
    Sparse_grid.put(grid, road.x, road.z, road)
  );

/**
  Makes roads three blocks wide. Each block is the highest elevation of any road
  it touches.
 */
let widen_roads = (roads: Sparse_grid.t(t)) => {
  let roads_lo_to_hi =
    Sparse_grid.fold(
      roads,
      (coord, road, ls) => [(coord, road), ...ls],
      [],
    )
    |> List.stable_sort(~compare=((_, a), (_, b)) =>
         switch (compare_structure(a.structure, b.structure)) {
         | 0 => Int.compare(a.y, b.y)
         | i => i
         }
       );

  List.fold(
    roads_lo_to_hi,
    ~init=Sparse_grid.make(Sparse_grid.side(roads)),
    ~f=(g, ((x, z), coord)) =>
    switch (coord.structure) {
    | Road =>
      Mg_util.Range.fold(z - 1, z + 1, g, (g, z) =>
        Mg_util.Range.fold(x - 1, x + 1, g, (g, x) =>
          Sparse_grid.put(g, x, z, coord)
        )
      )
    | Stair(N | S) =>
      Mg_util.Range.fold(x - 1, x + 1, g, (g, x) =>
        Sparse_grid.put(g, x, z, coord)
      )
    | Stair(E | W) =>
      Mg_util.Range.fold(z - 1, z + 1, g, (g, z) =>
        Sparse_grid.put(g, x, z, coord)
      )
    | Bridge(_) => Sparse_grid.put(g, x, z, coord)
    }
  );
};

let widen_roads_list = (roads: list(t)) => {
  let side =
    List.fold(roads, ~init=0, ~f=(acc, coord) =>
      max(acc, max(coord.x, coord.z))
    )
    + 3;
  let grid =
    List.fold(roads, ~init=Sparse_grid.make(side), ~f=(grid, coord) =>
      Sparse_grid.put(grid, coord.x, coord.z, coord)
    );
  let widened = widen_roads(grid);
  Sparse_grid.fold(widened, (_, coord, ls) => [coord, ...ls], []);
};

let extract_bridges = paths => {
  /* direction always goes from min to max */
  let normalize_dir =
    fun
    | N
    | S => S
    | E
    | W => E;
  let rec go = (paths, cur_bridge, bridges) =>
    switch (paths) {
    | [{x, y, z, structure: Bridge(direction)}, ...paths] =>
      let direction = normalize_dir(direction);
      switch (cur_bridge) {
      | Some({x: cx, y: _, z: cz, length, direction: _}) =>
        /* TODO check same direction? */
        go(
          paths,
          Some({
            x: min(x, cx),
            y,
            z: min(z, cz),
            length: length + 1,
            direction,
          }),
          bridges,
        )
      | None => go(paths, Some({x, y, z, direction, length: 1}), bridges)
      };
    | [_, ...paths] =>
      switch (cur_bridge) {
      | Some(cb) => go(paths, None, [cb, ...bridges])
      | None => go(paths, None, bridges)
      }
    | [] =>
      switch (cur_bridge) {
      | Some(cb) => [cb, ...bridges]
      | None => bridges
      }
    };

  go(paths, None, []);
};
