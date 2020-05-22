type depth =
  | From_surface(int, int)
  | From_bedrock(int, int);

type ore_layer = {
  ore: Minecraft.Block.material,
  densities: Point_cloud.t(float),
  depth,
  min_deposit_size: int,
  max_deposit_size: int,
};

type t = list(ore_layer);

let cardinal_directions_3d = [
  (0, 1, 0),
  (0, 0, 1),
  (1, 0, 0),
  (0, 0, (-1)),
  ((-1), 0, 0),
  (0, (-1), 0),
];

let random_ore = () => Minecraft.Block.Iron_ore; /* TODO other ores */

/** can_place_ore determines whether ore can be inserted into the given material */
let can_place_ore = (block: Minecraft.Block.material) =>
  switch (block) {
  | Grass
  | Dirt
  | Stone => true
  | _ => false
  };

let make_layer =
    (
      base: Grid.t(Base_overlay.tile),
      ~min_density,
      ~max_density,
      ~depth,
      ~min_deposit_size,
      ~max_deposit_size,
      ~ore,
    ) => {
  let densities =
    Point_cloud.init(
      ~width=base.side, ~height=base.side, ~spacing=128, (_, _) =>
      min_density +. Random.float(max_density -. min_density)
    );
  {ore, densities, depth, min_deposit_size, max_deposit_size};
};

let prepare = (base, ()) => {
  /* let iron_surface =
     make_layer(
       base,
       ~ore=Minecraft.Block.Iron_ore,
       ~min_density=0.1,
       ~max_density=0.1,
       ~depth=From_surface(0, 2),
       ~min_deposit_size=3,
       ~max_deposit_size=10,
     ); */
  let iron_low_density =
    [40, 60, 80, 100, 120, 140, 160]
    |> List.map(
         elev =>
           make_layer(
             base,
             ~ore=Minecraft.Block.Iron_ore,
             ~min_density=0.03,
             ~max_density=0.06,
             ~depth=From_bedrock(elev, elev + 20),
             ~min_deposit_size=1,
             ~max_deposit_size=14,
           ),
         _,
       );
  let iron_high_density =
    [10, 20, 30]
    |> List.map(
         elev =>
           make_layer(
             base,
             ~ore=Minecraft.Block.Iron_ore,
             ~min_density=0.1,
             ~max_density=0.2,
             ~depth=From_bedrock(elev, elev + 10),
             ~min_deposit_size=1,
             ~max_deposit_size=14,
           ),
         _,
       );
  let diamond =
    make_layer(
      base,
      ~ore=Minecraft.Block.Diamond_ore,
      ~min_density=0.05,
      ~max_density=0.15,
      ~depth=From_bedrock(1, 15),
      ~min_deposit_size=15,
      ~max_deposit_size=30,
    );
  iron_low_density @ iron_high_density @ [diamond];
};

let rec remove_i = (i, list) =>
  switch (list) {
  | [] => raise(Invalid_argument("index out of bounds"))
  | [x, ...rest] when i == 0 => (x, rest)
  | [not_x, ...rest] =>
    let (x, rest) = remove_i(i - 1, rest);
    (x, [not_x, ...rest]);
  };

let%expect_test "remove_i" = {
  let l = [0, 10, 20, 30];
  let (out_x, out_list) = remove_i(2, l);
  Printf.printf("%d\n", out_x);
  %expect
  "20";
  List.iter(n => Printf.printf("%d\n", n), out_list);
  %expect
  {|
    0
    10
    30
  |};
};

let random_sample = list => {
  let length = List.length(list);
  let i = Random.int(length);
  remove_i(i, list);
};

let rec place_deposit = (~region, ~ore, ~deposit_size, ~available, ~touched) =>
  switch (available) {
  | [] => ()
  | _ when deposit_size <= 0 => ()
  | available =>
    let ((x, y, z), available) = random_sample(available);
    switch (Minecraft.Block_tree.get_block_opt(region, x, y, z)) {
    | Some(block) when can_place_ore(block) =>
      Minecraft.Block_tree.set_block(region, x, y, z, ore);
      let deposit_size = deposit_size - 1;
      let (available, touched) =
        cardinal_directions_3d
        |> List.fold_left(
             ((available, touched) as no_change, (dx, dy, dz)) => {
               let coord = (x + dx, y + dy, z + dz);
               if (!List.mem(coord, touched)) {
                 ([coord, ...available], [coord, ...touched]);
               } else {
                 no_change;
               };
             },
             (available, touched),
             _,
           );
      place_deposit(~region, ~ore, ~deposit_size, ~available, ~touched);

    | Some(_)
    | None =>
      place_deposit(~region, ~ore, ~deposit_size, ~available, ~touched)
    };
  };
let place_deposit = (~region, ~ore, ~deposit_size, x, y, z): unit => {
  let coords = [(x, y, z)];
  place_deposit(
    ~region,
    ~ore,
    ~deposit_size,
    ~available=coords,
    ~touched=coords,
  );
};

let find_depth = (depth, args, x, z) => {
  let Minecraft_converter.{region, _} = args;
  switch (depth) {
  | From_surface(min_depth, max_depth) =>
    switch (
      Minecraft.Block_tree.highest_such_block(region, x, z, can_place_ore)
    ) {
    | None => None
    | Some(stone_elev) =>
      let depth = min_depth + Random.int(max_depth - min_depth);
      let y = stone_elev - depth;
      Some(y);
    }
  | From_bedrock(min_elev, max_elev) =>
    let y = min_elev + Random.int(max_elev - min_elev);
    Some(y);
  };
};

let apply_layer = (layer, args) => {
  let Minecraft_converter.{region, rx: _, rz: _, gx_offset, gy_offset, gsize} = args;
  let {ore, densities, depth, min_deposit_size, max_deposit_size} = layer;
  let deposits =
    Point_cloud.init(
      ~width=gsize,
      ~height=gsize,
      ~spacing=8,
      (x, z) => {
        let x = x + gx_offset;
        let z = z + gy_offset;
        let prob =
          Point_cloud.interpolate(
            densities,
            float_of_int(x),
            float_of_int(z),
          );
        Random.float(1.) < prob;
      },
    );
  List.iter(
    (Point_cloud.{x, y: z, value}) =>
      if (value) {
        let deposit_size =
          min_deposit_size + Random.int(max_deposit_size - min_deposit_size);
        let x = int_of_float(x);
        let z = int_of_float(z);
        switch (find_depth(depth, args, x, z)) {
        | Some(y) => place_deposit(~region, ~ore, ~deposit_size, x, y, z)
        | None => ()
        };
      },
    deposits.points,
  );
};

let apply_region = (_base: Grid.t(Base_overlay.tile), state, args): unit => {
  List.iter(layer => apply_layer(layer, args), state);
};

let overlay = base =>
  Overlay.make("ores", prepare(base), apply_region(base));