type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
};

type block = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
  s_of_center: bool,
  e_of_center: bool,
  can_subdivide: bool,
};

type output = {
  add_elevation: Grid.t(int),
  blocks: list(block),
};

let side = 128;
let max_elevation = 150;
let min_elevation = 0;
let base_elevation = 80;
let elevation_range = 10;
let min_cardinal_road_off = side / 3;
let max_cardinal_road_off = side * 2 / 3;
let max_block_area = 20 * 20;

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

  let colorize_elevation = ((elev, add)) => {
    let total = elev + add;
    let v =
      Mg_util.Floats.(
        ~~(
          ~.(total - min_elevation)
          /. ~.(max_elevation - min_elevation)
          *. 255.
        )
      );
    if (add > 0) {
      {r: 0, g: v, b: 0};
    } else {
      {r: v, g: v, b: v};
    };
  };

  let combined = Grid.zip(input.elevation, output.add_elevation);
  let side = combined.side;
  let img = (new rgb24)(side, side);

  Grid.iter(combined, (x, y, c) => {img#set(x, y, colorize_elevation(c))});

  List.iter(
    block => {
      let {min_x, max_x, min_z, max_z, _} = block;
      draw_rect(img, min_x, max_x, min_z, max_z, {r: 0, g: 0, b: 255});
    },
    output.blocks,
  );

  img#save(file, Some(Png), []);
  ();
};

let block_area = block => {
  let {min_x, max_x, min_z, max_z, _} = block;
  (max_x - min_x) * (max_z - min_z);
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

  let (road_along_z, _) =
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

  let (road_along_x, _) =
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
  let nw = {
    min_x: 0,
    max_x: road_along_z - 1 - 1,
    min_z: 0,
    max_z: road_along_x - 1 - 1,
    s_of_center: false,
    e_of_center: false,
    can_subdivide: true,
  };
  let ne = {
    min_x: road_along_z + 1,
    max_x: side - 1,
    min_z: 0,
    max_z: road_along_x - 1 - 1,
    s_of_center: false,
    e_of_center: true,
    can_subdivide: true,
  };
  let sw = {
    min_x: 0,
    max_x: road_along_z - 1 - 1,
    min_z: road_along_x + 1,
    max_z: side - 1,
    s_of_center: true,
    e_of_center: false,
    can_subdivide: true,
  };
  let se = {
    min_x: road_along_z + 1,
    max_x: side - 1,
    min_z: road_along_x + 1,
    max_z: side - 1,
    s_of_center: true,
    e_of_center: true,
    can_subdivide: true,
  };

  [nw, ne, sw, se];
};

let split_block = block => {
  /* Split along whichever dimension the block is longer */
  let {min_x, max_x, min_z, max_z, s_of_center, e_of_center} = block;
  let split_along_z = max_x - min_x > max_z - min_z;
  if (split_along_z) {
    let street_x = min_x + (max_x - min_x) / 2;
    let w = {
      min_x,
      max_x: street_x - 1,
      min_z,
      max_z,
      s_of_center,
      e_of_center,
      can_subdivide: e_of_center,
    };
    let e = {
      min_x: street_x + 1,
      max_x,
      min_z,
      max_z,
      s_of_center,
      e_of_center,
      can_subdivide: !e_of_center,
    };
    (w, e);
  } else {
    let street_z = min_z + (max_z - min_z) / 2;
    let n = {
      min_x,
      max_x,
      min_z,
      max_z: street_z - 1,
      s_of_center,
      e_of_center,
      can_subdivide: s_of_center,
    };
    let s = {
      min_x,
      max_x,
      min_z: street_z + 1,
      max_z,
      s_of_center,
      e_of_center,
      can_subdivide: !s_of_center,
    };
    (n, s);
  };
};

let rec subdivide_blocks = (blocks, finished_blocks) => {
  /* Subdivide any blocks over the desired size, until all are within that size */
  switch (blocks) {
  | [] => finished_blocks
  | [block, ...blocks]
      when !block.can_subdivide || block_area(block) <= max_block_area =>
    subdivide_blocks(blocks, [block, ...finished_blocks])
  | [block, ...blocks] =>
    let (a, b) = split_block(block);
    subdivide_blocks([a, b, ...blocks], finished_blocks);
  };
};
let subdivide_blocks = blocks => subdivide_blocks(blocks, []);

let run = (input: input): output => {
  let add_elevation = Grid.init(input.elevation.side, (_x, _y) => 0);
  let blocks = lay_cardinal_roads(input) |> subdivide_blocks;
  {add_elevation, blocks};
};

let test = () => {
  Random.init(1248);
  let input = make_input();
  let output = run(input);
  draw(input, output, "town_proto.png");
};