open! Core_kernel;
module Pq = Priority_queue.Int;
module Path_coord = Rules.Coord;

type closest_path = {
  cost: int,
  parent: option(Path_coord.t),
};

let root_path = {cost: 0, parent: None};

type pathing_state = {
  paths: Path_coord.Hash_set.t,
  mutable paths_to_place: list(Path_coord.t),
  closest_paths: Path_coord.Table.t(closest_path),
};

let init_state = () => {
  paths: Path_coord.Hash_set.create(),
  paths_to_place: [],
  closest_paths: Path_coord.Table.create(),
};

let get_paths_list = state => state.paths_to_place;

let get_bridges_list = state => Rules.extract_bridges(state.paths_to_place);

/** Call this if obstacles have changed */

let clear_closest_paths = state =>
  Path_coord.Table.clear(state.closest_paths);

let add_paths = (~should_place, ~new_paths, state) => {
  List.iter(new_paths, ~f=p => Hash_set.add(state.paths, p));
  if (should_place) {
    state.paths_to_place = new_paths @ state.paths_to_place;
  };
  ();
};

let set_paths_as_roots = state =>
  Hash_set.iter(state.paths, ~f=coord =>
    Path_coord.Table.set(state.closest_paths, ~key=coord, ~data=root_path)
  );

let update_closest_paths = (~edges, state) => {
  let allowed_iters = 10_000_000;
  let rec go = (open_set, ~elapsed_iters) =>
    if (elapsed_iters > allowed_iters) {
      Tale.logf("Exhausted all %d iters", elapsed_iters);
      ();
    } else {
      switch (Pq.extract_with_priority(open_set)) {
      | (Some((here_cost, here)), open_set) =>
        let open_set =
          List.fold(
            edges(here),
            ~init=open_set,
            ~f=(open_set, (neighbor, edge_cost)) => {
              let neighbor_cost = here_cost + edge_cost;
              let should_add =
                switch (Path_coord.Table.find(state.closest_paths, neighbor)) {
                | Some({cost: old_cost, parent: _}) =>
                  neighbor_cost < old_cost
                | None => true
                };

              if (should_add) {
                Path_coord.Table.set(
                  state.closest_paths,
                  ~key=neighbor,
                  ~data={cost: neighbor_cost, parent: Some(here)},
                );
                Pq.insert(open_set, neighbor_cost, neighbor);
              } else {
                open_set;
              };
            },
          );

        go(open_set, ~elapsed_iters=elapsed_iters + 1);
      | (None, _) =>
        Tale.logf(
          "Finished after %d of %d iters",
          elapsed_iters,
          allowed_iters,
        );
        ();
      };
    };

  let open_set =
    Hash_set.fold(state.paths, ~init=Pq.empty, ~f=(pq, coord) =>
      Pq.insert(pq, 0, coord)
    );

  set_paths_as_roots(state);
  go(open_set, ~elapsed_iters=0);
  ();
};

let reconstruct_path = (start, ~state) => {
  let rec go = (ls, point) => {
    let ls = [point, ...ls];
    let cp = Path_coord.Table.find_exn(state.closest_paths, point);
    switch (cp.parent) {
    | Some(parent) => go(ls, parent)
    | None => List.rev(ls)
    };
  };

  go([], start);
};

let get_closest_path = (~from_paths, state) =>
  List.filter_map(from_paths, ~f=p =>
    Path_coord.Table.find(state.closest_paths, p)
  )
  |> List.min_elt(~compare=(a, b) => Int.compare(a.cost, b.cost))
  |> Option.bind(~f=cp => cp.parent)
  |> Option.map(~f=parent => reconstruct_path(parent, ~state));

let enroad_outlets =
    (~add_outlets=false, ~get_elevation, ~get_obstacle, ~outlets, state) => {
  let edges = Rules.neighbors(~get_elevation, ~get_obstacle);
  update_closest_paths(~edges, state);
  let outlet_paths =
    List.map(
      outlets,
      ~f=((x, z)) => {
        let y = get_elevation(~x, ~z);
        Path_coord.make_road(~x, ~y, ~z);
      },
    );

  let found_path =
    switch (get_closest_path(state, ~from_paths=outlet_paths)) {
    | Some(new_path) =>
      let new_path =
        Rules.widen_roads_list(
          if (add_outlets) {
            outlet_paths @ new_path;
          } else {
            new_path;
          },
        );
      add_paths(~should_place=true, ~new_paths=new_path, state);
      true;
    | None => false
    };

  found_path;
};

let enroad_roads = (~roads, state) => {
  let edges =
    Rules.(
      neighbors(
        ~get_elevation=get_canon_elevation,
        ~get_obstacle=get_canon_obstacle,
      )
    );

  update_closest_paths(~edges, state);
  let new_path =
    roads
    @ (
      switch (get_closest_path(state, ~from_paths=roads)) {
      | Some(path) =>
        Tale.log("Found path");
        Rules.widen_roads_list(path);
      | None =>
        Tale.log("No path");
        [];
      }
    );

  add_paths(~should_place=true, ~new_paths=new_path, state);
  List.iter(new_path, ~f=p => Hash_set.add(state.paths, p));
  ();
};
