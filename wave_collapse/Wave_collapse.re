open! Core;

let item_at_exn =
    (eval: Evaluator.wave_evaluator('a), ~x: int, ~y: int, ~z: int) => {
  let ts = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (ts - 1));
  let ty = max(0, (y - 1) / (ts - 1));
  let tz = max(0, (z - 1) / (ts - 1));
  let subx = x - tx * (ts - 1);
  let suby = y - ty * (ts - 1);
  let subz = z - tz * (ts - 1);

  let t = Evaluator.observe_at_exn(eval, ~x=tx, ~y=ty, ~z=tz);
  eval.tileset.tiles[t].items[subx][suby][subz];
};

let item_at =
    (eval: Evaluator.wave_evaluator('a), ~x: int, ~y: int, ~z: int, ~default) => {
  let ts = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (ts - 1));
  let ty = max(0, (y - 1) / (ts - 1));
  let tz = max(0, (z - 1) / (ts - 1));
  let subx = x - tx * (ts - 1);
  let suby = y - ty * (ts - 1);
  let subz = z - tz * (ts - 1);

  switch (Evaluator.observe_at(eval, ~x=tx, ~y=ty, ~z=tz)) {
  | Some(t) => eval.tileset.tiles[t].items[subx][suby][subz]
  | None => default
  };
};

include Tileset;
include Evaluator;

module Test_helpers = {
  include Evaluator.Test_helpers;

  let print_items = (eval: Evaluator.wave_evaluator('a)) => {
    let ts = eval.tileset.tilesize;
    let xs = eval.xs * (ts - 1) + 1;
    let ys = eval.ys * (ts - 1) + 1;
    let zs = eval.zs * (ts - 1) + 1;
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          let t = item_at_exn(eval, ~x, ~y, ~z);
          Printf.printf("%s ", t);
        };
        Out_channel.newline(stdout);
      };
      Out_channel.newline(stdout);
    };
  };
};

let%expect_test "getting items after collapse" = {
  let eval =
    Evaluator.make_blank_wave(Test_helpers.tileset, ~xs=3, ~ys=1, ~zs=3);
  Evaluator.force_and_propagate(eval, ~x=2, ~y=0, ~z=2, 2);
  while (Evaluator.try_collapse_next_lowest_entropy(eval)) {
    ();
  };

  Test_helpers.print_items(eval);
  %expect
  {|
    x | x | x | x
    x 1 - 2 x 1 -
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x

    x | x | x | x
    x 1 - 2 x 1 -
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x

    x | x | x | x
    x 1 - 2 x 1 -
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
  |};
};