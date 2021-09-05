open Town_overlay_i;

type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
};

type layout_state = {
  centers: list((int, int)),
  elevation: Grid.t(int),
  road_obstacles: Sparse_grid.t(unit),
  obstacles: Sparse_grid.t(unit),
  pathing_state: Roads.pathing_state,
};

let side = 128;
let max_elevation = 150;
let min_elevation = 0;
let base_elevation = 80;
let elevation_range = 10;
let min_population = 3;
let max_population = 19;
let block_center_spacing = 10;
let min_block_spacing = 2;
let min_house_side = 5;
let max_house_side = 10;
let min_farm_side = 18;
let max_farm_side = 24;
let random_grab_ahead = 5;
let pop_per_farm = 2;
let num_plazas = 3;

let all_blocks = t => {
  Core_kernel.([t.bell] @ t.farms @ List.map(t.houses, ~f=h => h.block));
};

let make_input = () => {
  let elevation =
    Grid.Int.init(~side=4, _xy =>
      Random.int(elevation_range) + base_elevation
    )
    |> Grid.Subdivide.subdivide
    |> Grid.Subdivide.subdivide
    |> Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.(line() **> avg))
    |> Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.(line() **> avg))
    |> Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.(line() **> avg));

  /* Roads to the center */
  let center_x = elevation.side / 2;
  let center_z = elevation.side / 2;
  /* Start with boring roads */
  let obstacles =
    Sparse_grid.make(elevation.side)
    |> Mg_util.Range.fold(0, elevation.side - 1, _, (roads, z) => {
         Sparse_grid.put(roads, center_x, z, ())
       })
    |> Mg_util.Range.fold(0, elevation.side - 1, _, (roads, x) => {
         Sparse_grid.put(roads, x, center_z, ())
       });

  {elevation, obstacles};
};

let draw_rect = (img, min_x, max_x, min_z, max_z, border_color) => {
  for (x in min_x to max_x) {
    img#set(x, min_z, border_color);
    img#set(x, max_z, border_color);
  };
  for (z in min_z to max_z) {
    img#set(min_x, z, border_color);
    img#set(max_x, z, border_color);
  };
};

let draw = (input: input, output: output, file) => {
  open Images;
  open OImages;

  let colorize_elevation = elev => {
    let v =
      Mg_util.Floats.(
        ~~(
          ~.(elev - min_elevation) /. ~.(max_elevation - min_elevation) *. 255.
        )
      );
    {r: v, g: v, b: v};
  };

  let side = input.elevation.side;
  let img = (new rgb24)(side, side);

  let draw_blocks = (color, blocks) => {
    List.iter(
      block => {
        let {xz: {min_x, max_x, min_z, max_z}, elevation: _} = block;
        draw_rect(img, min_x, max_x, min_z, max_z, color);
      },
      blocks,
    );
  };

  Grid.Compat.iter(input.elevation, (x, y, c) => {
    img#set(x, y, colorize_elevation(c))
  });
  let road_color = {r: 0, g: 0, b: 0};
  Sparse_grid.iter(input.obstacles, ((x, y), ()) => {
    img#set(x, y, road_color)
  });

  draw_blocks({r: 0, g: 255, b: 0}, output.farms);
  draw_blocks({r: 0, g: 0, b: 255}, output.houses |> List.map(h => h.block));

  img#save(file, Some(Png), []);
  ();
};

let random_worksite = () => {
  switch (Random.int(3)) {
  | 0 => Butcher
  | 1 => Fisherman
  | 2
  | _ => Shepherd
  };
};

let block_area = block => {
  let {min_x, max_x, min_z, max_z, _} = block;
  (max_x - min_x + 1) * (max_z - min_z + 1);
};

let block_center = block => {
  let {min_x, max_x, min_z, max_z} = block;
  let x = min_x + (max_x - min_x) / 2;
  let z = min_z + (max_z - min_z) / 2;
  (x, z);
};

let block_center' = (block: block) => {
  block_center(block.xz);
};

let distance_to_block_edge = (~x, ~z, block) => {
  let {min_x, max_x, min_z, max_z} = block;
  let x_dist = max(min_x - x, max(x - max_x, 0));
  let z_dist = max(min_z - z, max(z - max_z, 0));
  Mg_util.distance_int((0, 0), (x_dist, z_dist));
};

let rec grab_one = (index, list) => {
  switch (list) {
  | [] => raise(Invalid_argument("grab index is outside list"))
  | [grabbed, ...rest] when index == 0 => (grabbed, rest)
  | [passed, ...rest] =>
    let (grabbed, rest) = grab_one(index - 1, rest);
    (grabbed, [passed, ...rest]);
  };
};

let random_grab_one = blocks => {
  switch (blocks) {
  | [] => None
  | blocks =>
    let avail = min(random_grab_ahead, List.length(blocks));
    let grab_index = Random.int(avail);
    let (block, blocks) = grab_one(grab_index, blocks);
    Some((block, blocks));
  };
};

let rec random_grab = (amount, blocks, selected) =>
  if (amount <= 0) {
    (selected, blocks);
  } else {
    switch (random_grab_one(blocks)) {
    | None => (selected, blocks)
    | Some((block, blocks)) =>
      random_grab(amount - 1, blocks, [block, ...selected])
    };
  };
let random_grab = (amount, blocks) => random_grab(amount, blocks, []);

let calc_average_elevation = (elevation, min_x, max_x, min_z, max_z) => {
  let sum =
    Mg_util.Range.fold(min_z, max_z, 0, (cur, z) =>
      Mg_util.Range.fold(
        min_x,
        max_x,
        cur,
        (cur, x) => {
          let here = Grid.get(x, z, elevation);
          cur + here;
        },
      )
    );
  sum / ((max_z - min_z + 1) * (max_x - min_x + 1));
};

let flatten_block = (state, ~block) => {
  let {min_x, max_x, min_z, max_z} = block;
  let elevation =
    calc_average_elevation(state.elevation, min_x, max_x, min_z, max_z);
  {xz: block, elevation};
};

let set_block_into_elevation = (state, ~block) => {
  let {xz: {min_x, max_x, min_z, max_z}, elevation: block_elevation} = block;
  let elevation = state.elevation;
  let elevation =
    Mg_util.Range.(
      fold(min_z, max_z, elevation, (elevation, z) =>
        fold(min_x, max_x, elevation, (elevation, x) =>
          Grid.Int.set(x, z, block_elevation, elevation)
        )
      )
    );
  {...state, elevation};
};

let flatten_blocks = (state, ~blocks) => {
  Core_kernel.List.map(blocks, ~f=block => flatten_block(state, ~block));
};

let sort_by_distance_to_center = (center, block_centers) => {
  List.(
    block_centers
    |> map(((x, z)) => (Mg_util.distance_int((x, z), center), x, z))
    |> fast_sort(((a, _, _), (b, _, _)) => Int.compare(a, b))
    |> map(((_dist, x, z)) => (x, z))
  );
};

let blocks_collide = (a, b) => {
  (
    /* Top side of A within B */
    b.min_z <= a.min_z
    && a.min_z <= b.max_z
    /* Top side of B within A */
    || a.min_z <= b.min_z
    && b.min_z <= a.max_z
  )
  && (
    /* Left side of A within B */
    b.min_x <= a.min_x
    && a.min_x <= b.max_x
    /* Left side of B within A */
    || a.min_x <= b.min_x
    && b.min_x <= a.max_x
  );
};

let check_block_obstacles = (~obstacles, block) => {
  let {min_x, max_x, min_z, max_z} = block;
  /* within town */
  0 <= min_x
  && max_x < side
  && 0 <= min_z
  && max_z < side
  /* doesn't hit obstacle */
  && !
       Mg_util.Range.exists(min_x, max_x, x =>
         Mg_util.Range.exists(min_z, max_z, z =>
           Sparse_grid.at(obstacles, x, z) |> Option.is_some
         )
       );
};

let make_block_from_center = ((x, z), side_x, side_z) => {
  let min_x = x - side_x / 2;
  let min_z = z - side_z / 2;
  let max_x = min_x + side_x - 1;
  let max_z = min_z + side_z - 1;
  {min_x, max_x, min_z, max_z};
};

let pad_block = (block, amount) => {
  {
    min_x: block.min_x - amount,
    max_x: block.max_x + amount,
    min_z: block.min_z - amount,
    max_z: block.max_z + amount,
  };
};

/**
  place_block pulls randomly from near the front of the list of available
  block centers until it finds one where the block will fit. Rejected block
  centers are discarded from the list.
 */
let rec fit_block = (state, ~side_x, ~side_z) => {
  switch (random_grab_one(state.centers)) {
  | None => (state, None)
  | Some((center, centers)) =>
    let state = {...state, centers};
    let new_block = make_block_from_center(center, side_x, side_z);
    if (check_block_obstacles(
          ~obstacles=state.obstacles,
          pad_block(new_block, min_block_spacing),
        )) {
      (state, Some(new_block));
    } else {
      fit_block(state, ~side_x, ~side_z);
    };
  };
};

let outlets_of_block = block => {
  let {min_x, max_x, min_z, max_z} = block;
  let m = 3;
  let top_bottom =
    Mg_util.Range.map(min_x, max_x, x => [(x, min_z - m), (x, max_z + m)])
    |> List.concat;
  let left_right =
    Mg_util.Range.map(min_z, max_z, z => [(min_x - m, z), (max_x + m, z)])
    |> List.concat;
  left_right
  @ top_bottom
  |> List.filter(((x, z)) => 0 <= x && x < side && 0 <= z && z < side);
};

let outlets_of_bell = block => {
  let {min_x, max_x, min_z, max_z} = block;
  Mg_util.Range.map(min_x, max_x, x =>
    Mg_util.Range.map(min_z, max_z, z => (x, z))
  )
  |> List.concat;
};

let add_block_to_obstacles = (~block, obstacles) => {
  let {min_x, max_x, min_z, max_z} = block;
  Mg_util.Range.fold(min_z, max_z, obstacles, (obstacles, z) =>
    Mg_util.Range.fold(min_x, max_x, obstacles, (obstacles, x) =>
      Sparse_grid.put(obstacles, x, z, ())
    )
  );
};

let add_roads_to_obstacles = (~roads, obstacles) => {
  Core_kernel.(
    List.fold(
      roads,
      ~init=obstacles,
      ~f=(obstacles, road) => {
        let {x, z, _}: Roads.Rules.t = road;
        Mg_util.Range.fold(z - 1, z + 1, obstacles, (obstacles, z) =>
          Mg_util.Range.fold(x - 1, x + 1, obstacles, (obstacles, x) =>
            Sparse_grid.put(obstacles, x, z, ())
          )
        );
      },
    )
  );
};

let get_elevation_of_state = state => {
  let elevation = state.elevation;
  (~x, ~z) => Grid.get(x, z, elevation);
};

let get_road_obstacle_of_state = state => {
  let obstacles = state.road_obstacles;
  (~x, ~z) =>
    if (Sparse_grid.is_within'(obstacles, x, z)) {
      if (Sparse_grid.has(obstacles, x, z)) {
        Overlay.Canon.Obstacle.Impassable;
      } else {
        Overlay.Canon.Obstacle.Clear;
      };
    } else {
      Overlay.Canon.Obstacle.Impassable;
    };
};

let enroad = (state, ~block) => {
  Roads.clear_closest_paths(state.pathing_state);
  Roads.enroad_gen(
    ~get_elevation=get_elevation_of_state(state),
    ~get_obstacle=get_road_obstacle_of_state(state),
    ~outlets=outlets_of_block(block),
    state.pathing_state,
  );
};

let place_plaza = (state, ~side_x, ~side_z) => {
  switch (fit_block(state, ~side_x, ~side_z)) {
  | (state, Some(block)) =>
    let obstacles = add_block_to_obstacles(~block, state.obstacles);
    let state = {...state, obstacles};
    Some((state, block));
  | (_state, None) => None
  };
};

let place_block = (state, ~side_x, ~side_z) => {
  switch (fit_block(state, ~side_x, ~side_z)) {
  | (state, Some(block)) =>
    let obstacles = add_block_to_obstacles(~block, state.obstacles);
    let road_obstacles = add_block_to_obstacles(~block, state.road_obstacles);
    let state = {...state, obstacles, road_obstacles};
    Roads.clear_closest_paths(state.pathing_state);
    enroad(state, ~block);
    let obstacles =
      add_roads_to_obstacles(
        state.obstacles,
        ~roads=Roads.get_paths_list(state.pathing_state),
      );
    let state = {...state, obstacles};
    Some((state, block));
  | (_state, None) => None
  };
};

let place_blocks = (state, ~min_side, ~max_side, ~amount) => {
  open Core_kernel;
  let rec go = (state, ~amount, ~blocks) =>
    if (amount > 0) {
      let side_x = Random.int_incl(min_side, max_side);
      let side_z = Random.int_incl(min_side, max_side);
      switch (place_block(state, ~side_x, ~side_z)) {
      | Some((state, block)) =>
        go(state, ~amount=amount - 1, ~blocks=[block, ...blocks])
      | None => (state, blocks)
      };
    } else {
      (state, blocks);
    };
  go(state, ~amount, ~blocks=[]);
};
let run = (input': input): output => {
  open Core_kernel;
  let {obstacles, elevation}: input = input';
  let town_side = side;
  let town_center = (town_side / 2, town_side / 2);
  let target_population =
    min_population + Random.int(max_population - min_population);
  Tale.logf("Target population = %d", target_population);
  let num_farms = target_population / pop_per_farm;
  let num_houses = target_population;

  /* Use a point cloud for block centers */
  let centers =
    Point_cloud.make_list(~side=town_side, ~spacing=block_center_spacing, ())
    |> List.map(~f=((x, z)) => Mg_util.Floats.(~~x, ~~z))
    /* Sort block centers by how close they are to the center plaza */
    |> sort_by_distance_to_center(town_center);
  let state = {
    centers,
    elevation,
    obstacles,
    road_obstacles: obstacles,
    pathing_state: Roads.init_state(),
  };

  let bell_side = 9;
  let (state, bell) =
    Option.value_exn(
      place_plaza(state, ~side_x=bell_side, ~side_z=bell_side),
    );
  let bell = flatten_block(state, ~block=bell);
  let state = set_block_into_elevation(state, ~block=bell);
  let bell_paths =
    outlets_of_bell(bell.xz)
    |> List.map(~f=((x, z)) => {
         let y = Grid.get(x, z, state.elevation);
         Roads.Rules.Coord.make_road(~x, ~y, ~z);
       });
  Roads.add_paths(
    ~should_place=false,
    ~new_paths=bell_paths,
    state.pathing_state,
  );

  /* Grab houses */
  let (state, houses) =
    place_blocks(
      state,
      ~min_side=min_house_side,
      ~max_side=max_house_side,
      ~amount=num_houses,
    );

  /* Grab farms */
  let (state, farms) =
    place_blocks(
      state,
      ~min_side=min_farm_side,
      ~max_side=max_farm_side,
      ~amount=num_farms,
    );

  let houses = flatten_blocks(state, ~blocks=houses);
  let farms = flatten_blocks(state, ~blocks=farms);

  /* Assign jobs */
  let (farm_houses, prof_houses) =
    Mg_util.take_both(List.length(farms), houses);
  let houses =
    List.map(~f=block => {block, worksite: None}, farm_houses)
    @ List.map(
        ~f=block => {block, worksite: Some(random_worksite())},
        prof_houses,
      );

  let {elevation: _, obstacles, road_obstacles: _, centers: _, pathing_state} = state;
  // TODO
  ignore(obstacles);
  let roads = Roads.get_paths_list(pathing_state);
  {bell, farms, houses, roads};
};

let test = () => {
  Random.init(1248);
  for (i in 1 to 5) {
    let input = make_input();
    let output = run(input);
    let path = Printf.sprintf("town_proto_%d.png", i);
    draw(input, output, path);
  };
};
