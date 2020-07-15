type input = {
  elevation: Grid.t(int),
  roads: Sparse_grid.t(unit),
};

type block = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
  elevation: int,
};

type block_no_elevation = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
};

type output = {
  farms: list(block),
  houses: list(block),
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
let min_farm_side = 8;
let max_farm_side = 12;
let random_grab_ahead = 5;
let pop_per_farm = 2;
let num_plazas = 3;

let make_input = () => {
  let elevation =
    Phase_chain.(
      run_all(
        phase("init", () =>
          Grid.init(4, (_x, _y) =>
            Random.int(elevation_range) + base_elevation
          )
        )
        @> phase_repeat(2, "subdivide", Subdivide.subdivide)
        @> phase_repeat(
             3,
             "line subdivide",
             Subdivide.subdivide_with_fill(_, Fill.(line() **> avg)),
           ),
      )
    );
  /* Roads to the center */
  let roads = Sparse_grid.make(elevation.side);
  let center_x = elevation.side / 2;
  let center_z = elevation.side / 2;
  /* Start with boring roads */
  let roads =
    Range.fold(0, elevation.side - 1, roads, (roads, z) => {
      Sparse_grid.put(roads, center_x, z, ())
    });
  let roads =
    Range.fold(0, elevation.side - 1, roads, (roads, x) => {
      Sparse_grid.put(roads, x, center_z, ())
    });

  {elevation, roads};
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
        let {min_x, max_x, min_z, max_z, elevation: _} = block;
        draw_rect(img, min_x, max_x, min_z, max_z, color);
      },
      blocks,
    );
  };

  Grid.iter(input.elevation, (x, y, c) => {
    img#set(x, y, colorize_elevation(c))
  });
  let road_color = {r: 0, g: 0, b: 0};
  Sparse_grid.iter(input.roads, ((x, y), ()) => {
    img#set(x, y, road_color)
  });

  draw_blocks({r: 0, g: 255, b: 0}, output.farms);
  draw_blocks({r: 0, g: 0, b: 255}, output.houses);

  img#save(file, Some(Png), []);
  ();
};

let block_area = block => {
  let {min_x, max_x, min_z, max_z, _} = block;
  (max_x - min_x + 1) * (max_z - min_z + 1);
};

let block_center = block => {
  let {min_x, max_x, min_z, max_z, _} = block;
  let x = min_x + (max_x - min_x) / 2;
  let z = min_z + (max_z - min_z) / 2;
  (x, z);
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
  | [] => (None, [])
  | blocks =>
    let avail = min(random_grab_ahead, List.length(blocks));
    let grab_index = Random.int(avail);
    let (block, blocks) = grab_one(grab_index, blocks);
    (Some(block), blocks);
  };
};

let rec random_grab = (amount, blocks, selected) =>
  if (amount <= 0) {
    (selected, blocks);
  } else {
    switch (random_grab_one(blocks)) {
    | (None, blocks) => (selected, blocks)
    | (Some(block), blocks) =>
      random_grab(amount - 1, blocks, [block, ...selected])
    };
  };
let random_grab = (amount, blocks) => random_grab(amount, blocks, []);

let flatten_blocks = (input: input, blocks) => {
  let calc_average_elevation = (min_x, max_x, min_z, max_z) => {
    let sum =
      Range.fold(min_z, max_z, 0, (cur, z) =>
        Range.fold(
          min_x,
          max_x,
          cur,
          (cur, x) => {
            let here = Grid.at(input.elevation, x, z);
            cur + here;
          },
        )
      );
    sum / ((max_z - min_z + 1) * (max_x - min_x + 1));
  };

  let flatten_block = block => {
    let {min_x, max_x, min_z, max_z} = block;
    let elevation = calc_average_elevation(min_x, max_x, min_z, max_z);
    {min_x, max_x, min_z, max_z, elevation};
  };

  List.map(flatten_block, blocks);
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

let check_block_obstacles = (obstacles, other_blocks, block) => {
  let {min_x, max_x, min_z, max_z} = block;
  let hit_other_block =
    List.exists(
      other_block => blocks_collide(other_block, block),
      other_blocks,
    );
  let hit_obstacle =
    Range.exists(min_x, max_x, x =>
      Range.exists(min_z, max_z, z =>
        Sparse_grid.at(obstacles, x, z) |> Option.is_some
      )
    );
  !hit_other_block && !hit_obstacle;
};

let make_block_from_center = ((x, z), side_x, side_z) => {
  let min_x = x - side_x / 2;
  let min_z = z - side_z / 2;
  let max_x = min_x + side_x;
  let max_z = min_z + side_z;
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
let rec place_block = (obstacles, other_blocks, block_centers, side_x, side_z) => {
  switch (random_grab_one(block_centers)) {
  | (None, block_centers) => (block_centers, None)
  | (Some(center), block_centers) =>
    let new_block = make_block_from_center(center, side_x, side_z);
    if (check_block_obstacles(
          obstacles,
          other_blocks,
          pad_block(new_block, min_block_spacing),
        )) {
      (block_centers, Some(new_block));
    } else {
      place_block(obstacles, other_blocks, block_centers, side_x, side_z);
    };
  };
};

let rec make_blocks =
        (
          obstacles,
          other_blocks,
          block_centers,
          new_blocks,
          min_side,
          max_side,
          amount,
        ) =>
  if (amount <= 0) {
    (other_blocks, block_centers, new_blocks);
  } else {
    let side_x = Random.int(max_side - min_side) + min_side;
    let side_z = Random.int(max_side - min_side) + min_side;
    switch (
      place_block(obstacles, other_blocks, block_centers, side_x, side_z)
    ) {
    | (block_centers, None) =>
      /* Couldn't place the block, so quit */
      (other_blocks, block_centers, new_blocks)
    | (block_centers, Some(new_block)) =>
      let new_blocks = [new_block, ...new_blocks];
      let other_blocks = [new_block, ...other_blocks];
      make_blocks(
        obstacles,
        other_blocks,
        block_centers,
        new_blocks,
        min_side,
        max_side,
        amount - 1,
      );
    };
  };
let make_blocks =
    (obstacles, other_blocks, block_centers, min_side, max_side, amount) =>
  make_blocks(
    obstacles,
    other_blocks,
    block_centers,
    [],
    min_side,
    max_side,
    amount,
  );

let run = (input: input): output => {
  let town_side = input.elevation.side;
  let town_center = (town_side / 2, town_side / 2);
  let target_population =
    min_population + Random.int(max_population - min_population);
  Printf.printf("Target population = %d\n", target_population);
  let num_farms = target_population / pop_per_farm;
  let num_houses = target_population;

  /* Use a point cloud for block centers */
  let centers =
    Point_cloud.make_list(
      ~width=town_side,
      ~height=town_side,
      ~spacing=block_center_spacing,
      (),
    )
    |> List.map(((x, z)) => Mg_util.Floats.(~~x, ~~z))
    /* Sort block centers by how close they are to the center plaza */
    |> sort_by_distance_to_center(town_center);
  /* Grab houses first */
  let other_blocks = [];
  let (other_blocks, centers, houses) =
    make_blocks(
      input.roads,
      other_blocks,
      centers,
      min_house_side,
      max_house_side,
      num_houses,
    );
  /* Grab farms */
  let (_other_blocks, _centers, farms) =
    make_blocks(
      input.roads,
      other_blocks,
      centers,
      min_farm_side,
      max_farm_side,
      num_farms,
    );

  let houses = flatten_blocks(input, houses);
  let farms = flatten_blocks(input, farms);

  {farms, houses};
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