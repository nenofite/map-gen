type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
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
  plazas: list(block),
  houses: list(block),
};

let side = 128;
let max_elevation = 150;
let min_elevation = 0;
let base_elevation = 80;
let elevation_range = 10;
let min_population = 3;
let max_population = 19;
let min_cardinal_road_off = side / 3;
let max_cardinal_road_off = side * 2 / 3;
let max_block_area = 9 * 8;
let block_split_randomization = 4;
let min_block_side = 4;
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

  draw_blocks({r: 0, g: 255, b: 0}, output.farms);
  draw_blocks({r: 0, g: 0, b: 0}, output.plazas);
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

let lay_cardinal_roads = (input: input) => {
  /* Find the line within middle 1/3 that has the flattest elevation */
  let calc_elevation_range = (min_x, max_x, min_z, max_z) => {
    let start = Grid.at(input.elevation, min_x, min_z);
    let (min_elev, max_elev) =
      Range.fold(min_z, max_z, (start, start), (cur, z) =>
        Range.fold(
          min_x,
          max_x,
          cur,
          ((cur_min, cur_max), x) => {
            let here = Grid.at(input.elevation, x, z);
            (min(cur_min, here), max(cur_max, here));
          },
        )
      );
    max_elev - min_elev;
  };

  let (road_x, _) =
    Range.fold(
      min_cardinal_road_off,
      max_cardinal_road_off,
      ((-1), Int.max_int),
      ((_, cur_range) as cur, x) => {
        let here_range = calc_elevation_range(x - 1, x, 0, side - 1);
        if (here_range < cur_range) {
          (x, here_range);
        } else {
          cur;
        };
      },
    );

  let (road_z, _) =
    Range.fold(
      min_cardinal_road_off,
      max_cardinal_road_off,
      ((-1), Int.max_int),
      ((_, cur_range) as cur, z) => {
        let here_range = calc_elevation_range(0, side - 1, z - 1, z);
        if (here_range < cur_range) {
          (z, here_range);
        } else {
          cur;
        };
      },
    );

  /* Create blocks based on these cardinal roads */
  let nw = {min_x: 0, max_x: road_x - 1 - 1, min_z: 0, max_z: road_z - 1 - 1};
  let ne = {
    min_x: road_x + 1,
    max_x: side - 1,
    min_z: 0,
    max_z: road_z - 1 - 1,
  };
  let sw = {
    min_x: 0,
    max_x: road_x - 1 - 1,
    min_z: road_z + 1,
    max_z: side - 1,
  };
  let se = {
    min_x: road_x + 1,
    max_x: side - 1,
    min_z: road_z + 1,
    max_z: side - 1,
  };
  let blocks = [nw, ne, sw, se];

  (road_x, road_z, blocks);
};

let split_block = block => {
  /* Split along whichever dimension the block is longer */
  let {min_x, max_x, min_z, max_z} = block;
  let split_along_z = max_x - min_x > max_z - min_z;
  if (split_along_z) {
    let street_x =
      min_x
      + (max_x - min_x)
      / 2
      + Random.int(2 * block_split_randomization)
      - block_split_randomization;
    let w = {min_x, max_x: street_x - 1, min_z, max_z};
    let e = {min_x: street_x + 1, max_x, min_z, max_z};
    (w, e);
  } else {
    let street_z =
      min_z
      + (max_z - min_z)
      / 2
      + Random.int(2 * block_split_randomization)
      - block_split_randomization;
    let n = {min_x, max_x, min_z, max_z: street_z - 1};
    let s = {min_x, max_x, min_z: street_z + 1, max_z};
    (n, s);
  };
};

let rec subdivide_blocks = (blocks, finished_blocks) => {
  /* Subdivide any blocks over the desired size, until all are within that size */
  switch (blocks) {
  | [] => finished_blocks
  | [block, ...blocks] when block_area(block) <= max_block_area =>
    subdivide_blocks(blocks, [block, ...finished_blocks])
  | [block, ...blocks] =>
    let (a, b) = split_block(block);
    subdivide_blocks([a, b, ...blocks], finished_blocks);
  };
};
let subdivide_blocks = blocks => subdivide_blocks(blocks, []);

let filter_sliver_blocks = blocks => {
  List.filter(
    b =>
      b.max_x
      - b.min_x >= min_block_side
      && b.max_z
      - b.min_z >= min_block_side,
    blocks,
  );
};

let order_blocks_by_dist = (road_x, road_z, blocks) => {
  let dist = block => {
    Mg_util.distance_int((road_x, road_z), block_center(block));
  };
  List.(
    map(b => (dist(b), b), blocks)
    |> fast_sort(((a, _), (b, _)) => - compare(a, b))
    |> map(((_dist, b)) => b)
  );
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

let rec grab = (amount, blocks, selected) => {
  switch (blocks) {
  | [] => (selected, [])
  | blocks when amount <= 0 => (selected, blocks)
  | [block, ...blocks] => grab(amount - 1, blocks, [block, ...selected])
  };
};
let grab = (amount, blocks) => grab(amount, blocks, []);

let rec random_grab = (amount, blocks, selected) => {
  switch (blocks) {
  | [] => (selected, [])
  | blocks when amount <= 0 => (selected, blocks)
  | blocks =>
    let avail = min(random_grab_ahead, List.length(blocks));
    let grab_index = Random.int(avail);
    let (block, blocks) = grab_one(grab_index, blocks);
    random_grab(amount - 1, blocks, [block, ...selected]);
  };
};
let random_grab = (amount, blocks) => random_grab(amount, blocks, []);

let warn_if_mismatch = (expected, actual, label) =>
  if (expected != actual) {
    Printf.printf(
      "warning: %s should be %d, but is %d\n",
      label,
      expected,
      actual,
    );
  };

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

let run = (input: input): output => {
  let target_population =
    min_population + Random.int(max_population - min_population);
  Printf.printf("Target population = %d\n", target_population);
  let (road_x, road_z, blocks) = lay_cardinal_roads(input);
  let blocks =
    blocks
    |> subdivide_blocks
    |> filter_sliver_blocks
    |> order_blocks_by_dist(road_x, road_z);
  let total_blocks = List.length(blocks);
  let num_farms = target_population / pop_per_farm;
  let num_houses = target_population;
  let needed_blocks = num_farms + num_houses + num_plazas;
  /* TODO deal with this more gracefully */
  assert(needed_blocks <= total_blocks);
  let blocks_to_discard = total_blocks - needed_blocks;
  let (_discards, blocks) = random_grab(blocks_to_discard, blocks);
  let blocks = flatten_blocks(input, blocks);
  /* Take from the closest blocks for plaza */
  let (plazas, blocks) = grab(num_plazas, List.rev(blocks));
  warn_if_mismatch(num_plazas, List.length(plazas), "num plazas");
  let blocks = List.rev(blocks);
  let (farms, blocks) = random_grab(num_farms, blocks);
  warn_if_mismatch(num_farms, List.length(farms), "num farms");
  let houses = blocks;
  warn_if_mismatch(num_houses, List.length(houses), "num houses");

  {farms, houses, plazas};
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