open! Core;

let item_at_exn =
    (eval: Evaluator.wave_evaluator('a, 'tag), ~x: int, ~y: int, ~z: int) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  let t = Evaluator.observe_at_exn(eval, ~x=tx, ~y=ty, ~z=tz);
  eval.tileset.tiles[t].items[subx][suby][subz];
};

let item_at =
    (
      eval: Evaluator.wave_evaluator('a, 'tag),
      ~x: int,
      ~y: int,
      ~z: int,
      ~default,
    ) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  switch (Evaluator.observe_at(eval, ~x=tx, ~y=ty, ~z=tz)) {
  | Result.Ok(t) => eval.tileset.tiles[t].items[subx][suby][subz]
  | Result.Error(_) => default
  };
};
let item_or_entropy_at =
    (eval: Evaluator.wave_evaluator('a, 'tag), ~x: int, ~y: int, ~z: int) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  switch (Evaluator.observe_at(eval, ~x=tx, ~y=ty, ~z=tz)) {
  | Result.Ok(t) => Result.Ok(eval.tileset.tiles[t].items[subx][suby][subz])
  | Result.Error(_) as e => e
  };
};

let item_dims = (eval: Evaluator.wave_evaluator('a, 'tag)) => {
  let tsz = eval.tileset.tilesize;
  let Evaluator.{xs, ys, zs, _} = eval;
  (xs * (tsz - 1) + 1, ys * (tsz - 1) + 1, zs * (tsz - 1) + 1);
};

let force_edges =
    (
      ~transition_margin=0,
      ~x0=?,
      ~x1=?,
      ~y0=?,
      ~y1=?,
      ~z0=?,
      ~z1=?,
      eval: Evaluator.wave_evaluator('a, 'tag),
    ) =>
  Evaluator.wrap_contradiction_error(() => {
    let Evaluator.{xs, ys, zs, tileset, _} = eval;

    // If allowing transitions, don't force corners
    let (mino, maxo) = (transition_margin, - (1 + transition_margin));

    switch (x0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (y in mino to ys + maxo) {
        for (z in mino to zs + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x=0, ~y, ~z, bans);
        };
      };
    | None => ()
    };
    switch (x1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (y in mino to ys + maxo) {
        for (z in mino to zs + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x=xs - 1, ~y, ~z, bans);
        };
      };
    | None => ()
    };

    switch (z0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (y in mino to ys + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z=0, bans);
        };
      };
    | None => ()
    };
    switch (z1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (y in mino to ys + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z=zs - 1, bans);
        };
      };
    | None => ()
    };

    // Top and bottom take priority
    switch (y0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (z in mino to zs + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y=0, ~z, bans);
        };
      };
    | None => ()
    };
    switch (y1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (z in mino to zs + maxo) {
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y=ys - 1, ~z, bans);
        };
      };
    | None => ()
    };

    Evaluator.finish_propagating(eval);
  });

include Tileset;
include Evaluator;

module Test_helpers = {
  include Tileset.Test_helpers;
  include Evaluator.Test_helpers;

  let print_items = (~show_item, eval: Evaluator.wave_evaluator('a, 'tag)) => {
    let (xs, ys, zs) = item_dims(eval);
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          let t =
            switch (item_or_entropy_at(eval, ~x, ~y, ~z)) {
            | Ok(item) => show_item(item)
            | Error(_) => "?"
            };
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

  Test_helpers.print_items(~show_item=Fn.id, eval);
  %expect
  {|
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x

    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x

    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
    - 2 x 1 - 2 x
    x | x | x | x
  |};
};