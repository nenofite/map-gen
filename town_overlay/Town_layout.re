open! Core_kernel;
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

let is_within_town = (x, z) => 0 <= x && x < side && 0 <= z && z < side;

let all_blocks' = (~bell, ~farms, ~houses) => {
  [bell.xz]
  @ List.map(farms, ~f=f => f.xz)
  @ List.map(houses, ~f=h => h.building.block);
};

let all_blocks = t => {
  let {bell, farms, houses, roads: _, fences: _, obstacles: _} = t;
  all_blocks'(~bell, ~farms, ~houses);
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

  let obstacles = Sparse_grid.make(elevation.side);

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
      ~f=
        block => {
          let {min_x, max_x, min_z, max_z} = block;
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

  draw_blocks({r: 0, g: 255, b: 0}, output.farms |> List.map(~f=f => f.xz));
  draw_blocks(
    {r: 0, g: 0, b: 255},
    output.houses |> List.map(~f=h => h.building.block),
  );

  img#save(file, Some(Png), []);
  ();
};

let rotate_building_cw = (b: building, ~times: int): building => {
  template:
    Minecraft_template.rotate_90_cw(b.template, ~times)
    |> Minecraft_template.normalize_on_origin,
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
    |> map(~f=((x, z)) => (Mg_util.distance_int((x, z), center), x, z))
    |> Caml.List.fast_sort(((a, _, _), (b, _, _)) => Int.compare(a, b))
    |> map(~f=((_dist, x, z)) => (x, z))
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
  let cx = (max_x + min_x) / 2;
  let cz = (max_z + min_z) / 2;
  let m = 3;
  let top_bottom =
    Mg_util.Range.map(cx - 1, cx + 1, x => [(x, min_z - m), (x, max_z + m)])
    |> List.concat;
  let left_right =
    Mg_util.Range.map(cz - 1, cz + 1, z => [(min_x - m, z), (max_x + m, z)])
    |> List.concat;
  left_right
  @ top_bottom
  |> List.filter(~f=((x, z)) => is_within_town(x, z));
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

let enroad_building = (state, ~roads) => {
  Roads.clear_closest_paths(state.pathing_state);
  let found_path =
    Roads.enroad_outlets(
      ~get_elevation=get_elevation_of_state(state),
      ~get_obstacle=get_road_obstacle_of_state(state),
      ~outlets=roads,
      ~add_outlets=true,
      state.pathing_state,
    );
  if (found_path) {
    Tale.log("Found path to building 💃");
  } else {
    Tale.log("No path to building 😪");
  };
  found_path;
};

let enroad_block = (state, ~block) => {
  Roads.clear_closest_paths(state.pathing_state);
  Roads.enroad_outlets(
    ~get_elevation=get_elevation_of_state(state),
    ~get_obstacle=get_road_obstacle_of_state(state),
    ~outlets=outlets_of_block(block),
    state.pathing_state,
  );
};

/**
 Try to place a building in each possible rotation
 */
let fit_building =
    (state: layout_state, ~building: building)
    : (layout_state, option(fitted_building)) => {
  let original_building =
    rotate_building_cw(building, ~times=Random.int_incl(0, 3));
  let rec go = (building, remaining_rots, state, remaining_centers) => {
    let side_x = Minecraft_template.x_size_of(building.template);
    let side_z = Minecraft_template.z_size_of(building.template);
    switch (random_grab_one(remaining_centers)) {
    | None => (state, None)
    | Some((center, remaining_centers)) =>
      let new_block = make_block_from_center(center, side_x, side_z);
      let fits =
        check_block_obstacles(
          ~obstacles=state.obstacles,
          pad_block(new_block, min_block_spacing),
        );
      if (fits) {
        let fitted = {building, block: new_block};
        let state = {
          ...state,
          centers: List.filter(state.centers, ~f=c => Poly.(c != center)),
        };
        (state, Some(fitted));
      } else if (remaining_rots > 0) {
        go(
          rotate_building_cw(building, ~times=1),
          remaining_rots - 1,
          state,
          remaining_centers,
        );
      } else {
        // No rotation works at this center, so move on to the next one
        go(
          original_building,
          3,
          state,
          remaining_centers,
        );
      };
    };
  };

  go(original_building, 3, state, state.centers);
};

/**
  Flatten elevation under a fitted building, add it to obstacles, and enroad it
 */
let place_building =
    (state: layout_state, ~building: fitted_building): layout_state => {
  let {building: _, block} = building;
  let state =
    set_block_into_elevation(state, ~block=flatten_block(state, ~block));
  let obstacles = add_block_to_obstacles(~block, state.obstacles);
  let road_obstacles = add_block_to_obstacles(~block, state.road_obstacles);
  let state = {...state, obstacles, road_obstacles};
  Roads.clear_closest_paths(state.pathing_state);
  let found_path =
    switch (
      Minecraft_template.get_marks(building.building.template, ~mark=`Road)
    ) {
    | [] => enroad_block(state, ~block)
    | roads =>
      let roads =
        List.map(
          roads,
          ~f=((x, _y, z)) => {
            let x = block.min_x + x;
            let z = block.min_z + z;
            (x, z);
          },
        );
      enroad_building(state, ~roads);
    };
  ignore(found_path: bool);
  let obstacles =
    add_roads_to_obstacles(
      state.obstacles,
      ~roads=Roads.get_paths_list(state.pathing_state),
    );
  let state = {...state, obstacles};
  state;
};

/**
  Fit and place a random selection of buildings from the list, up to a given amount
 */
let place_buildings =
    (state: layout_state, ~buildings: list(building), ~amount: int)
    : (layout_state, list(fitted_building)) => {
  let buildings = Mg_util.shuffle(buildings);
  let rec go = (state, buildings, amount, result) => {
    switch (buildings) {
    | _ when amount <= 0 => (state, result)
    | [] => (state, result)
    | [building, ...buildings] =>
      switch (fit_building(state, ~building)) {
      | (state, Some(fitted_building)) =>
        let state = place_building(state, ~building=fitted_building);
        go(
          state,
          buildings @ [building],
          amount - 1,
          [fitted_building, ...result],
        );
      | (state, None) => go(state, buildings, amount, result)
      }
    };
  };
  go(state, buildings, amount, []);
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
    ignore(enroad_block(state, ~block): bool);
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

let init_state_and_centers = (input': input) => {
  let {obstacles, elevation}: input = input';
  let town_center = (side / 2, side / 2);
  /* Use a point cloud for block centers */
  let centers =
    Point_cloud.make_list(~side, ~spacing=block_center_spacing, ())
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
  state;
};

let prepare_bell = (state: layout_state) => {
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
  (state, bell);
};

let prepare_houses = (num_houses: int, state: layout_state) =>
  place_buildings(
    state,
    ~buildings=Town_templates.houses,
    ~amount=num_houses,
  );

let prepare_farms = (num_farms: int, state: layout_state) => {
  let (state, farms) =
    place_blocks(
      state,
      ~min_side=min_farm_side,
      ~max_side=max_farm_side,
      ~amount=num_farms,
    );
  let farms = flatten_blocks(state, ~blocks=farms);
  (state, farms);
};

let prepare_fences = (~blocks: list(block_no_elevation), state: layout_state) => {
  let margin = 3;
  let min_x =
    (List.map(blocks, ~f=b => b.min_x) |> List.reduce_exn(~f=min)) - margin;
  let max_x =
    (List.map(blocks, ~f=b => b.max_x) |> List.reduce_exn(~f=max)) + margin;
  let min_z =
    (List.map(blocks, ~f=b => b.min_z) |> List.reduce_exn(~f=min)) - margin;
  let max_z =
    (List.map(blocks, ~f=b => b.max_z) |> List.reduce_exn(~f=max)) + margin;
  let fences =
    Mg_util.Range.map(min_x + 1, max_x, x => (x, min_z))
    @ Mg_util.Range.map(min_x, max_x - 1, x => (x, max_z))
    @ Mg_util.Range.map(min_z, max_z - 1, z => (min_x, z))
    @ Mg_util.Range.map(min_z + 1, max_z, z => (max_x, z))
    |> List.filter(~f=((x, z)) => is_within_town(x, z));

  let obstacles =
    List.fold(fences, ~init=state.obstacles, ~f=(obs, (x, z)) =>
      Sparse_grid.put(obs, x, z, ())
    );
  let road_obstacles =
    List.fold(fences, ~init=state.road_obstacles, ~f=(obs, (x, z)) =>
      Sparse_grid.put(obs, x, z, ())
    );
  let state = {...state, obstacles, road_obstacles};

  (state, fences);
};

let assign_jobs_to_houses = (houses, ~num_farms) => {
  let (farm_houses, prof_houses) = Mg_util.take_both(num_farms, houses);
  let houses =
    List.map(~f=building => {building, worksite: None}, farm_houses)
    @ List.map(
        ~f=building => {building, worksite: Some(random_worksite())},
        prof_houses,
      );
  houses;
};

let run = (input': input): output => {
  let target_population =
    min_population + Random.int(max_population - min_population);
  Tale.logf("Target population = %d", target_population);
  let num_farms = target_population / pop_per_farm;
  let num_houses = target_population;

  let state = init_state_and_centers(input');
  let (state, bell) = prepare_bell(state);
  let (state, houses) = prepare_houses(num_houses, state);
  let (state, farms) = prepare_farms(num_farms, state);
  // Fix-up number of farms based on how many plots we could actually place,
  // rather than the target
  let num_farms = List.length(farms);
  let houses = assign_jobs_to_houses(houses, ~num_farms);

  let (state, fences) =
    prepare_fences(~blocks=all_blocks'(~bell, ~houses, ~farms), state);

  let {elevation: _, obstacles, road_obstacles: _, centers: _, pathing_state} = state;
  let roads = Roads.get_paths_list(pathing_state);
  {bell, farms, houses, roads, fences, obstacles};
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

module Test_helpers = {
  include Test_helpers;

  let show_layout_state =
      (~show_centers=false, state: layout_state): text_grid => {
    let show_at = (x, z) =>
      if (Sparse_grid.has(state.road_obstacles, x, z)) {
        "X";
      } else if (Sparse_grid.has(state.obstacles, x, z)) {
        "O";
      } else if (show_centers
                 && List.mem(~equal=Poly.equal, state.centers, (x, z))) {
        "#";
      } else {
        " ";
      };
    show_grid(
      ~side=Grid.side(state.elevation),
      ~get=show_at,
      ~show_cell=s => s,
      (),
    );
  };

  let show_elevation = (~radius=?, ~center=?, elevation: Grid.t(int)) => {
    show_grid(
      ~radius?,
      ~center?,
      ~side=Grid.side(elevation),
      ~get=(x, z) => Grid.get(x, z, elevation),
      ~show_cell=Int.to_string,
      (),
    );
  };

  let show_obstacles = (~radius=?, ~center=?, obstacles: Sparse_grid.t(unit)) => {
    let show_cell = o =>
      switch (o) {
      | None => " "
      | Some () => "X"
      };
    show_grid(
      ~radius?,
      ~center?,
      ~side=Sparse_grid.side(obstacles),
      ~get=(x, z) => Sparse_grid.at(obstacles, x, z),
      ~show_cell,
      (),
    );
  };
};

let%expect_test "creates a town from input" = {
  open Test_helpers;
  Random.init(1234);
  Caml.Random.init(1234);
  let input = make_input();
  let output = run(input);
  ignore([%expect.output]);
  show_obstacles(output.obstacles) |> print_grid;
  %expect
  "
    X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
    X                                                                                                                                                                                                                                 X
    X                                                                                                                                                                                                                                 X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                                                                                                               X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X                                                 X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X     X X X                                       X X X X X X X X X X X X X X X X X X X X X                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X     X X X X X                                                                                                     X
    X                                                                       X X X X X X X X X X X X X X X X X X X X     X X X X X                       X X X X X X X X X X X X X X X X                                               X
    X                                                                       X X X X X X X X X X X X X X X X X X X X     X X X X X                   X X X X X X X X X X X X X X X X X X                                               X
    X                                                                       X X X X X X X X X X X X X X X X X X X X         X X X X                 X X X X X X X X X X X X X X X X X X                                               X
    X                                                                       X X X X X X X X X X X X X X X X X X X X         X X X X                 X X X X X                                                                         X
    X                                                                       X X X X X X X X X X X X X X X X X X X X         X X X X                 X X X                                                                             X
    X                                                                       X X X X X X X X X X X X X X X X X X X X           X X X               X X X X               X X X X X X X                                                 X
    X                                                                       X X X X X X X X X X X X X X X X X X X X           X X X               X X X X               X X X X X X X                                                 X
    X                                                                       X X X X X X X X X X X X X X X X X X X X           X X X               X X X X               X X X X X X X                                                 X
    X                                                                       X X X X X X X X X X X X X X X X X X X X           X X X               X X X                 X X X X X X X                                                 X
    X                                                                                                                         X X X               X X X                 X X X X X X X                                                 X
    X                                                                                                                         X X X X X X X X X X X X X X X X X X       X X X X X X X                                                 X
    X                                                                                                                       X X X X X X X X X X X X X X X X X X X       X X X X X X X                                                 X
    X                                                                                                                       X X X X X X X X X X X X X X X X X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X                     X X X X X                         X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X                   X X X X X           X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X                   X X X X X           X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X     X X X X       X X X X             X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X     X X X X X     X X X               X X X X X X X   X X X X     X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X   X X X X     X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                               X X X X X X X X X X X X X X X X X X X X X X X X X       X X X X X X X X X X X X X X X X X X X X X X X   X X X X     X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                                                                         X X X X X X X X X X X X X X X X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                                                                                                       X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                                               X X X X X X X X X X X X X X X X X X X X X X X X X       X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                                               X X X X X X X X X X X X X X X X X X X X X X X X X       X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X X       X X X X X X X   X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X X                       X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X X                       X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X X                       X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X     X X X X X X X X X X X X X X X X X X X X X X X X X         X X X X X X X X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                                                               X X X X X X X X X X       X X X X X X X         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                           X X X X X X           X X X X X X X X X X X X X X X X X                             X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                           X X X X X X X X X X X X X X X X X X X X X         X X X                             X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                           X X X X X X X X X X X X X X X X X X X X X     X X X X X X X                         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                           X X X   X X X X X X X X X                     X X X X X X X                         X X X X X X X X X X X X X X X X X X     X
    X                                                             X X X X X X X                           X X X   X X X X X X X X X                     X X X X X X X                                                                 X
    X                                                             X X X X X X X                           X X X   X X X X X X X X X                     X X X X X X X                                                                 X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X   X X X X X X X X X                     X X X X X X X                                     X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X   X X X X X X X X X                     X X X X X X X                                     X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X   X X X X X X X X X X                   X X X X X X X                                     X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X   X X X X X X X X X X                                                                   X X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X                 X X X                                                                   X X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X                 X X X                                                                 X X X X X                       X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X                 X X X   X X X X X X X                                                 X X X X                         X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                           X X X                 X X X   X X X X X X X                                                 X X X X                         X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                     X X X X X X                 X X X   X X X X X X X             X X X X X X X X X X X X X X X X X X X X X                           X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                     X X X X X X                 X X X   X X X X X X X             X X X X X X X X X X X X X X X X X X X X X                           X
    X     X X X X X X X X X X X X X X X X X X X X X X X           X X X X X X X                     X X X X X X                 X X X X X X X X X X X             X X X X X X X X X X X X X X X X X X X X X                           X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X   X X X X X X X                   X X X X                       X X X X X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X   X X X X X X X                   X X X X                       X X X X X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X                           X X X X X X X X                       X X X   X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X X X X X X X X X X X X X X X X X X X X X                         X X X   X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X X X X X X X X X X X X X X X X X X X X X       X X X X X X X     X X X   X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X   X X X X X X X X X X X X X X X X X X X   X X X X     X X X X X X X     X X X   X X X X X X X             X X X                                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X                                           X X X X     X X X X X X X X X X X X                             X X X   X X X X X X X                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X                                           X X X X     X X X X X X X X X X X X X X X                       X X X   X X X X X X X                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X                                             X X X     X X X X X X X X X X X X X X X                       X X X   X X X X X X X                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X                                             X X X     X X X X X X X   X X X X X X X                       X X X   X X X X X X X                                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X     X X X X X X X X X X X X X X X X X X     X X X     X X X X X X X           X X X                     X X X X X X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X                             X X X X X X X X X X X X X X X X X X X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X X X X X X X                 X X X X X X X X X X X X X X X X X X X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X X X X X X X                 X X X X X X X X X X X X X X X X     X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X X X X X X X                   X X X                             X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X       X X X           X X X X X X X X X X X                     X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X   X X X X X X X       X X X X X X X X X X X                     X X X X X X X                                               X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X   X X X X X X X       X X X X X X X X X X X                                                                                 X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X   X X X X X X X       X X X X X X X X X X X                                                                                 X
    X                                                       X X X X X X X X X X X X X X X X X X     X X X   X X X X X X X       X X X X X X X X X X X                                                                                 X
    X                                                       X X X X X X X X X X X X X X X X X X             X X X X X X X       X X X X X X X X X X X                                                                                 X
    X                                                       X X X X X X X X X X X X X X X X X X             X X X X X X X       X X X X X X X X X X X                                                                                 X
    X                                                       X X X X X X X X X X X X X X X X X X             X X X X X X X                                                                                                             X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                       X X X X X X X X X X X X X X X X X X                                                                                                                                       X
    X                                                                                                                                                                                                                                 X
    X                                                                                                                                                                                                                                 X
    X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
  ";
};

let%expect_test "builds up" = {
  open Test_helpers;
  Random.init(789);
  Caml.Random.init(789);
  let diff = make_running_diff();
  let input = make_input();
  let state = init_state_and_centers(input);
  show_layout_state(~show_centers=true, state) |> print_grid(~radius=10);
  // Should have scattered centers
  %expect
  "
                #




    #                         #

                #










          #
  ";
  let (state, bell) = prepare_bell(state);
  diff(show_layout_state(state)) |> print_grid;
  // Should create a bell square that blocks roads
  %expect
  "
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
    O O O O O O O O O
  ";
  let (state, _) = prepare_houses(1, state);
  ignore([%expect.output]);
  diff(show_layout_state(state)) |> print_grid;
  // Should create one house and a road touching it
  %expect
  "
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
                    O O O
    X X X X X X X   O O O
    X X X X X X X   O O O
    X X X X X X X   O O O
    X X X X X X X   O O O
    X X X X X X X O O O O
    X X X X X X X O O O O
    X X X X X X X O O O O
    X X X X X X X
    X X X X X X X
    X X X X X X X
    X X X X X X X
  ";
  let (state, houses) = prepare_houses(3, state);
  ignore([%expect.output]);
  diff(show_layout_state(state)) |> print_grid;
  // Should create three houses and roads
  %expect
  "
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
                                                      X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X O O O O O O O                       X X X X X X X
    X X X X X X X O O O O O O O                       X X X X X X X
    X X X X X X X O O O O O O O                       X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
    X X X X X X X                                     X X X X X X X
                                                      X X X X X X X





                                                O O O O O O O O O O O
                                                O O O O O O O O O O O
                                                O O O O O O O O O O O
                                                                O O O
                                                            X X X X X X X
                                                            X X X X X X X
                                                            X X X X X X X
                                                            X X X X X X X
                                                            X X X X X X X
                                                            X X X X X X X
                                                            X X X X X X X
  ";

  let (state, farm_1) = prepare_farms(1, state);
  ignore([%expect.output]);
  diff(show_layout_state(state)) |> print_grid;
  // Should create one farm and a road leading to the center of any side
  %expect
  "
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
    X X X X X X X X X X X X X X X X X X   O O O
                                          O O O
                                          O O O
  ";
  let (state, farms) = prepare_farms(3, state);
  ignore([%expect.output]);
  diff(show_layout_state(state)) |> print_grid;
  // Should create three farms and roads
  %expect
  "
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X
                                                                                                                    X X X X X X X X X X X X X X X X X X X X X X X

                                                                                                                                    O O O
                                                                                                                                    O O O
                                                                                                                                    O O O
                                                                                                                                    O O O
                                                                                                                                  O O O O
                                                                                                                                  O O O O
                                                                                                                                  O O O O
                                                                                                                O O O O O O O O O O O O
                                                                                                              O O O O O O O O O O O O O
                                                                                                          O O O O O O O O O O O O O O O
    X X X X X X X X X X X X X X X X X X X X X X X X                                                       O O O O O O
    X X X X X X X X X X X X X X X X X X X X X X X X                                                       O O O O O     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                     O O O O         X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                           O         X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                           O         X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X                                                                     X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
    X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
                                                          O O O                   O O O                                 X X X X X X X X X X X X X X X X X X
                                                          O O O                 O O O O                                 X X X X X X X X X X X X X X X X X X
                                                          O O O             O O O O O O                                 X X X X X X X X X X X X X X X X X X
                                                          O O O O O O O O O O O O O O O
                                                          O O O O O O O O O O O O O O
                                                          O O O O O O O O O O O O
  ";

  let houses = assign_jobs_to_houses(houses, ~num_farms=1);
  print_s(
    [%sexp_of: list(option(worksite))](
      List.map(houses, ~f=h => h.worksite),
    ),
  );
  // Should have one jobless and two with jobs
  %expect
  "(() (Shepherd) (Butcher))";

  let (state, fences) =
    prepare_fences(
      ~blocks=all_blocks'(~bell, ~houses, ~farms=farm_1 @ farms),
      state,
    );
  ignore([%expect.output]);
  print_s(
    [%sexp_of: bool](
      List.contains_dup(~compare=[%derive.ord: (int, int)], fences),
    ),
  );
  %expect
  "false";
  show_layout_state(state) |> print_grid;
  // Should surround the village with a fence
  %expect
  "
    X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
    X                                                                                                                                                                       X
    X                                                                                                                                                                       X
    X                                     X X X X X X X X X X X X X X X X X X                                                                                               X
    X                                     X X X X X X X X X X X X X X X X X X                                                                                               X
    X                                     X X X X X X X X X X X X X X X X X X                                                                                               X
    X                                     X X X X X X X X X X X X X X X X X X                                                                                               X
    X                                     X X X X X X X X X X X X X X X X X X                                                                                               X
    X                                     X X X X X X X X X X X X X X X X X X                                             X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X                                             X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X                                             X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X                                             X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                     X X X X X X X X X X X X X X X X X X   O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                                                           O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                                                           O O O               X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                                                         O O O O O O O O O     X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                                                         O O O O O O O O O     X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                             X X X X X X X               O O O O O O O O O     X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                             X X X X X X X               O O O O O O O O O     X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                             X X X X X X X               O O O O O O O O O     X X X X X X X         X X X X X X X X X X X X X X X X X X X X X X X     X
    X                                             X X X X X X X               O O O O O O O O O     X X X X X X X                                                           X
    X                                             X X X X X X X O O O O O O O O O O O O O O O O     X X X X X X X                         O O O                             X
    X                                             X X X X X X X O O O O O O O O O O O O O O O O     X X X X X X X                         O O O                             X
    X                                             X X X X X X X O O O O O O O O O O O O O O O O     X X X X X X X                         O O O                             X
    X                                             X X X X X X X                         O O O       X X X X X X X                         O O O                             X
    X                                             X X X X X X X                         O O O       X X X X X X X                       O O O O                             X
    X                                             X X X X X X X                         O O O       X X X X X X X                       O O O O                             X
    X                                             X X X X X X X                         O O O       X X X X X X X                       O O O O                             X
    X                                                                                   O O O       X X X X X X X     O O O O O O O O O O O O                               X
    X                                                                                   O O O                       O O O O O O O O O O O O O                               X
    X                                                                                   O O O                   O O O O O O O O O O O O O O O                               X
    X     X X X X X X X X X X X X X X X X X X X X X X X X                               O O O                   O O O O O O                                                 X
    X     X X X X X X X X X X X X X X X X X X X X X X X X                               O O O                   O O O O O     X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X                               O O O                 O O O O         X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X                               O O O O O O O O O O O O O O O         X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X   O O O O O O O O O O O O O O O         X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X   O O O O O O O O O O O O O O           X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X   O O O                 O O O           X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X   O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X O O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X O O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X               X X X X X X X O O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O   X X X X X X X   O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O   X X X X X X X   O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X   O O O O O   X X X X X X X   O O O             X X X X X X X       X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O   X X X X X X X   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X     X X X X X X X X X X X X X X X X X X X X X X X X       O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X                                                           O O O                   O O O                                 X X X X X X X X X X X X X X X X X X           X
    X                                                           O O O                 O O O O                                 X X X X X X X X X X X X X X X X X X           X
    X                                                           O O O             O O O O O O                                 X X X X X X X X X X X X X X X X X X           X
    X                                                           O O O O O O O O O O O O O O O                                                                               X
    X                                                           O O O O O O O O O O O O O O                                                                                 X
    X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X
  ";
};