open! Core;

let item_dims = (eval: Evaluator.wave_evaluator('a, 'tag)) => {
  let tsz = eval.tileset.tilesize;
  let Evaluator.{xs, ys, zs, _} = eval;
  (xs * (tsz - 1) + 1, ys * (tsz - 1) + 1, zs * (tsz - 1) + 1);
};

let force_at =
    (
      ~walkability=?,
      ~x,
      ~y,
      ~z,
      tag,
      eval: Evaluator.wave_evaluator('a, 'tag),
    ) =>
  Evaluator.wrap_contradiction_error(() => {
    switch (walkability) {
    | Some(w) => Evaluator.mark_walkability(eval, ~x, ~y, ~z, w)
    | None => ()
    };
    let bans = Tileset.lookup_tag_inv(tag, eval.tileset);
    Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
    Evaluator.finish_propagating(eval);
  });

let force_edges =
    (
      ~transition_margin=0,
      ~walkable=false,
      ~unwalkable=false,
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

    let set_walk_at = (~x, ~y, ~z) => {
      if (walkable) {
        Evaluator.mark_walkability(eval, ~x, ~y, ~z, Walkable);
      };
      if (unwalkable) {
        Evaluator.mark_walkability(eval, ~x, ~y, ~z, Unwalkable);
      };
    };

    switch (x0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (y in mino to ys + maxo) {
        for (z in mino to zs + maxo) {
          let x = 0;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
        };
      };
    | None => ()
    };
    switch (x1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (y in mino to ys + maxo) {
        for (z in mino to zs + maxo) {
          let x = xs - 1;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
        };
      };
    | None => ()
    };

    switch (z0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (y in mino to ys + maxo) {
          let z = 0;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
        };
      };
    | None => ()
    };
    switch (z1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (y in mino to ys + maxo) {
          let z = zs - 1;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
        };
      };
    | None => ()
    };

    switch (y0) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (z in mino to zs + maxo) {
          let y = 0;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
        };
      };
    | None => ()
    };
    switch (y1) {
    | Some(t) =>
      let bans = Tileset.lookup_tag_inv(t, tileset);
      for (x in mino to xs + maxo) {
        for (z in mino to zs + maxo) {
          let y = ys - 1;
          set_walk_at(~x, ~y, ~z);
          Evaluator.ban_multi_no_propagate(eval, ~x, ~y, ~z, bans);
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
    for (y in ys - 1 downto 0) {
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
  let eval = make_blank_wave(Test_helpers.tileset, ~xs=3, ~ys=1, ~zs=3);
  force_at(~walkability=Unwalkable, ~x=2, ~y=0, ~z=2, Test_helpers.Two, eval);
  while (Evaluator.try_collapse_next_lowest_entropy(eval)) {
    ();
  };

  Test_helpers.print_items(~show_item=Fn.id, eval);
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