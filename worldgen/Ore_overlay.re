open Bin_prot.Std;

[@deriving bin_io]
type depth =
  | From_surface(int, int)
  | From_bedrock(int, int);

[@deriving bin_io]
type ore_layer = {
  ore: Minecraft.Block.material,
  densities: Point_cloud.t(float),
  depth,
  min_deposit_size: int,
  max_deposit_size: int,
};

[@deriving bin_io]
type t = list(ore_layer);

let cardinal_directions_3d = [
  (0, 1, 0),
  (0, 0, 1),
  (1, 0, 0),
  (0, 0, (-1)),
  ((-1), 0, 0),
  (0, (-1), 0),
];

/** can_place_ore determines whether ore can be inserted into the given material */
let can_place_ore = (block: Minecraft.Block.material) =>
  switch (block) {
  | Grass_block
  | Dirt
  | Stone => true
  | _ => false
  };

let make_layer =
    (
      base,
      ~min_density,
      ~max_density,
      ~depth,
      ~min_deposit_size,
      ~max_deposit_size,
      ~ore,
    ) => {
  let densities =
    Point_cloud.init(~side=Base_overlay.side(base), ~spacing=128, (_, _) =>
      min_density +. Random.float(max_density -. min_density)
    );
  {ore, densities, depth, min_deposit_size, max_deposit_size};
};

let prepare = () => {
  let (base, _) = Base_overlay.require();
  let emerald =
    make_layer(
      base,
      ~ore=Emerald_ore,
      ~min_density=0.,
      ~max_density=1.,
      ~depth=From_bedrock(1, 15),
      ~min_deposit_size=1,
      ~max_deposit_size=2,
    );
  let diamond =
    make_layer(
      base,
      ~ore=Diamond_ore,
      ~min_density=0.05,
      ~max_density=0.15,
      ~depth=From_bedrock(1, 15),
      ~min_deposit_size=15,
      ~max_deposit_size=30,
    );
  let gold =
    make_layer(
      base,
      ~ore=Gold_ore,
      ~min_density=0.5,
      ~max_density=1.,
      ~depth=From_bedrock(5, 31),
      ~min_deposit_size=1,
      ~max_deposit_size=9,
    );
  let iron_surface =
    make_layer(
      base,
      ~ore=Iron_ore,
      ~min_density=0.01,
      ~max_density=0.025,
      ~depth=From_surface(0, 1),
      ~min_deposit_size=2,
      ~max_deposit_size=6,
    );
  let iron_deep =
    make_layer(
      base,
      ~ore=Iron_ore,
      ~min_density=1.,
      ~max_density=1.,
      ~depth=From_bedrock(30, 50),
      ~min_deposit_size=14,
      ~max_deposit_size=28,
    );
  let lapis_lazuli =
    make_layer(
      base,
      ~ore=Lapis_ore,
      ~min_density=0.01,
      ~max_density=0.1,
      ~depth=From_bedrock(5, 30),
      ~min_deposit_size=10,
      ~max_deposit_size=100,
    );
  let redstone =
    make_layer(
      base,
      ~ore=Redstone_ore,
      ~min_density=0.01,
      ~max_density=0.1,
      ~depth=From_bedrock(5, 30),
      ~min_deposit_size=10,
      ~max_deposit_size=100,
    );
  let coal =
    make_layer(
      base,
      ~ore=Minecraft.Block.Coal_ore,
      ~min_density=0.5,
      ~max_density=1.,
      ~depth=From_bedrock(5, 120),
      ~min_deposit_size=1,
      ~max_deposit_size=17,
    );
  let granite =
    make_layer(
      base,
      ~ore=Granite,
      ~min_density=0.,
      ~max_density=0.1,
      ~depth=From_surface(3, 40),
      ~min_deposit_size=50,
      ~max_deposit_size=300,
    );
  let diorite =
    make_layer(
      base,
      ~ore=Diorite,
      ~min_density=0.,
      ~max_density=0.1,
      /* ~depth=From_bedrock(5, 79),
         ~min_deposit_size=1,
         ~max_deposit_size=33, */
      ~depth=From_surface(3, 40),
      ~min_deposit_size=50,
      ~max_deposit_size=300,
    );
  let andesite =
    make_layer(
      base,
      ~ore=Andesite,
      ~min_density=0.,
      ~max_density=0.1,
      ~depth=From_surface(3, 40),
      ~min_deposit_size=50,
      ~max_deposit_size=300,
    );
  let gravel =
    make_layer(
      base,
      ~ore=Minecraft.Block.Gravel,
      ~min_density=0.05,
      ~max_density=0.15,
      ~depth=From_surface(3, 40),
      ~min_deposit_size=1,
      ~max_deposit_size=33,
    );
  let dirt =
    make_layer(
      base,
      ~ore=Dirt,
      ~min_density=0.1,
      ~max_density=1.,
      ~depth=From_surface(3, 40),
      ~min_deposit_size=1,
      ~max_deposit_size=33,
    );
  /*
   * generate the most rare elements first to reduce the risk of failed
   * placement
   */
  [
    emerald,
    diamond,
    gold,
    lapis_lazuli,
    redstone,
    iron_surface,
    iron_deep,
    coal,
    granite,
    diorite,
    andesite,
    gravel,
    dirt,
  ];
};

let remove_i = (i, list) => {
  open Core;
  let rec go = (passed, i, rest) =>
    switch (rest) {
    | [] => raise(Invalid_argument("index out of bounds"))
    | [x, ...rest] when i == 0 => (x, List.rev_append(passed, rest))
    | [not_x, ...rest] => go([not_x, ...passed], i - 1, rest)
    };
  go([], i, list);
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
    switch (Minecraft.Region.get_block_opt(region, ~x, ~y, ~z)) {
    | Some(block) when can_place_ore(block) =>
      Minecraft.Region.set_block(~x, ~y, ~z, ore, region);
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

let find_depth = (depth, region, x, z) => {
  switch (depth) {
  | From_surface(min_depth, max_depth) =>
    switch (
      Minecraft.Region.highest_such_block(region, ~x, ~z, can_place_ore)
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

let apply_layer = (layer, region) => {
  let {ore, densities, depth, min_deposit_size, max_deposit_size} = layer;
  let (rx, rz) = Minecraft.Region.region_offset(region);
  let deposits =
    Point_cloud.init(
      ~side=Minecraft.Region.block_per_region_side,
      ~spacing=8,
      (x, z) => {
        let x = x + rx;
        let z = z + rz;
        let prob =
          Point_cloud.interpolate(
            densities,
            float_of_int(x),
            float_of_int(z),
          );
        Random.float(1.) < prob;
      },
    );
  Point_cloud.iter(deposits, ~f=(~x, ~y as z, value) =>
    if (value) {
      let deposit_size =
        min_deposit_size + Random.int(max_deposit_size - min_deposit_size);
      let x = int_of_float(x) + rx;
      let z = int_of_float(z) + rz;
      switch (find_depth(depth, region, x, z)) {
      | Some(y) => place_deposit(~region, ~ore, ~deposit_size, x, y, z)
      | None => ()
      };
    }
  );
};

let apply_region = (state, region): unit => {
  List.iter(layer => apply_layer(layer, region), state);
};

let (require, prepare, apply) =
  Overlay.make_no_canon(
    "ores",
    prepare,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );
