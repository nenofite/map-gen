open! Core_kernel;

type wave('a) = {
  tileset: Tileset.tileset('a),
  /* x y z kind */
  possibilities: array(array(array(array(bool)))),
  xs: int,
  ys: int,
  zs: int,
};

type wave_evaluator('a) = {
  initial: wave('a),
  /* x y z kind */
  mutable possibilities: array(array(array(array(bool)))),
  mutable entropy_queue: Priority_queue.t((int, int, int)),
};

let make_blank_possibilities = (~numtiles: int, xs: int, ys: int, zs: int) =>
  Array.init(xs, ~f=_ =>
    Array.init(ys, ~f=_ =>
      Array.init(zs, ~f=_ => Array.create(~len=numtiles, true))
    )
  );

let force_no_propagate = (wave, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let numtiles = Tileset.numtiles(wave.tileset);
  for (t in 0 to numtiles - 1) {
    wave.possibilities[x][y][z][t] = t == tile_id;
  };
};

let make_blank_wave = (tileset, ~xs, ~ys, ~zs) => {
  let initial = {
    tileset,
    possibilities: [||], /* TODO */
    xs,
    ys,
    zs,
  };
  let numtiles = Tileset.numtiles(tileset);
  {
    initial,
    possibilities: make_blank_possibilities(~numtiles, xs, ys, zs),
    entropy_queue: Priority_queue.empty,
  };
};

let tile_fits_at = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let numtiles = Tileset.numtiles(eval.initial.tileset);

  let x0_fits =
    if (x > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x - 1][y][z][t]
        && eval.initial.tileset.x_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let x1_fits =
    if (x < eval.initial.xs - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x + 1][y][z][t]
        && eval.initial.tileset.x_pairs[tile_id][t]
      );
    } else {
      true;
    };

  let y0_fits =
    if (y > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y - 1][z][t]
        && eval.initial.tileset.y_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let y1_fits =
    if (y < eval.initial.ys - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y + 1][z][t]
        && eval.initial.tileset.y_pairs[tile_id][t]
      );
    } else {
      true;
    };

  let z0_fits =
    if (z > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y][z - 1][t]
        && eval.initial.tileset.z_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let z1_fits =
    if (x < eval.initial.zs - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y][z + 1][t]
        && eval.initial.tileset.z_pairs[tile_id][t]
      );
    } else {
      true;
    };

  x0_fits && x1_fits && y0_fits && y1_fits && z0_fits && z1_fits;
};

let push_neighbor_coords = (ls, ~xs, ~ys, ~zs, ~x, ~y, ~z) => {
  if (x > 0) {
    ls := [(x - 1, y, z), ...ls^];
  };
  if (x < xs - 1) {
    ls := [(x + 1, y, z), ...ls^];
  };

  if (y > 0) {
    ls := [(x, y - 1, z), ...ls^];
  };
  if (y < ys - 1) {
    ls := [(x, y + 1, z), ...ls^];
  };

  if (z > 0) {
    ls := [(x, y, z - 1), ...ls^];
  };
  if (z < zs - 1) {
    ls := [(x, y, z + 1), ...ls^];
  };
};

let propagate_at = (eval, needs_propagate, ~x, ~y, ~z) => {
  let {xs, ys, zs, _} = eval.initial;
  let numtiles = Tileset.numtiles(eval.initial.tileset);
  let changed = ref(false);
  for (t in 0 to numtiles - 1) {
    if (eval.possibilities[x][y][z][t] && !tile_fits_at(eval, ~x, ~y, ~z, t)) {
      changed := true;
      eval.possibilities[x][y][z][t] = false;
    };
  };
  if (changed^) {
    push_neighbor_coords(needs_propagate, ~xs, ~ys, ~zs, ~x, ~y, ~z);
  };
};

let propagate_all = eval => {
  let needs_propagate = ref([]);
  let {xs, ys, zs, _} = eval.initial;

  for (x in 0 to xs - 1) {
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        propagate_at(eval, needs_propagate, ~x, ~y, ~z);
      };
    };
  };

  while (!List.is_empty(needs_propagate^)) {
    let np = needs_propagate^;
    needs_propagate := [];
    List.iter(np, ~f=((x, y, z)) => {
      propagate_at(eval, needs_propagate, ~x, ~y, ~z)
    });
  };
};

let entropy_at = (eval, ~x: int, ~y: int, ~z: int) => {
  Array.sum((module Int), eval.possibilities[x][y][z], ~f=Bool.to_int) - 1;
};

module Test_helpers = {
  let print_entropy = eval => {
    let {xs, ys, zs, _} = eval.initial;
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          let e = entropy_at(eval, ~x, ~y, ~z);
          Printf.printf("%d ", e);
        };
        Out_channel.newline(stdout);
      };
      Out_channel.newline(stdout);
    };
  };
};

let%expect_test "initial propagation" = {
  let ts =
    Tileset.(
      create_tileset(
        ~tilesize=3,
        [
          tile(
            ~weight=1.0,
            [|
              [|
                [|"a", "b", "c"|], /* */
                [|"d", "0", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
              [|
                [|"a", "b", "c"|], /* */
                [|"d", "0", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
              [|
                [|"a", "b", "c"|], /* */
                [|"d", "0", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
            |],
          ),
          tile(
            ~weight=1.0,
            [|
              [|
                [|"g", "h", "i"|], /* */
                [|"d", "1", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
              [|
                [|"g", "h", "i"|], /* */
                [|"d", "1", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
              [|
                [|"g", "h", "i"|], /* */
                [|"d", "1", "f"|], /* */
                [|"g", "h", "i"|] /* */
              |],
            |],
          ),
          tile(
            ~weight=1.0,
            [|
              [|
                [|"a", "b", "a"|], /* */
                [|"d", "2", "d"|], /* */
                [|"g", "h", "g"|] /* */
              |],
              [|
                [|"a", "b", "a"|], /* */
                [|"d", "2", "d"|], /* */
                [|"g", "h", "g"|] /* */
              |],
              [|
                [|"a", "b", "a"|], /* */
                [|"d", "2", "d"|], /* */
                [|"g", "h", "g"|] /* */
              |],
            |],
          ),
        ],
      )
    );

  let eval = make_blank_wave(ts, ~xs=7, ~ys=7, ~zs=7);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2

    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
    2 2 2 2 2 2 2
  |};
  ();
};