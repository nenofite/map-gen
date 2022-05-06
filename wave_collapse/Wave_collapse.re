open! Core_kernel;

type wave('a) = {
  /* TODO put this to use */
  tileset: Tileset.tileset('a),
  xs: int,
  ys: int,
  zs: int,
  /* x y z kind */
  possibilities: array(array(array(array(bool)))),
};

type wave_evaluator('a) = {
  tileset: Tileset.tileset('a),
  xs: int,
  ys: int,
  zs: int,
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

let force_no_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let numtiles = Tileset.numtiles(eval.tileset);
  for (t in 0 to numtiles - 1) {
    eval.possibilities[x][y][z][t] = t == tile_id;
  };
};

let make_blank_wave = (tileset, ~xs, ~ys, ~zs) => {
  let numtiles = Tileset.numtiles(tileset);
  {
    tileset,
    xs,
    ys,
    zs,
    possibilities: make_blank_possibilities(~numtiles, xs, ys, zs),
    entropy_queue: Priority_queue.empty,
  };
};

let tile_fits_at = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let numtiles = Tileset.numtiles(eval.tileset);

  let x0_fits =
    if (x > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x - 1][y][z][t] && eval.tileset.x_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let x1_fits =
    if (x < eval.xs - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x + 1][y][z][t] && eval.tileset.x_pairs[tile_id][t]
      );
    } else {
      true;
    };

  let y0_fits =
    if (y > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y - 1][z][t] && eval.tileset.y_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let y1_fits =
    if (y < eval.ys - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y + 1][z][t] && eval.tileset.y_pairs[tile_id][t]
      );
    } else {
      true;
    };

  let z0_fits =
    if (z > 0) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y][z - 1][t] && eval.tileset.z_pairs[t][tile_id]
      );
    } else {
      true;
    };
  let z1_fits =
    if (z < eval.zs - 1) {
      Mg_util.Range.exists(0, numtiles - 1, t =>
        eval.possibilities[x][y][z + 1][t] && eval.tileset.z_pairs[tile_id][t]
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
  let {xs, ys, zs, _} = eval;
  let numtiles = Tileset.numtiles(eval.tileset);
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

let finish_propagating = (eval, needs_propagate) => {
  while (!List.is_empty(needs_propagate^)) {
    let np = needs_propagate^;
    needs_propagate := [];
    List.iter(np, ~f=((x, y, z)) => {
      propagate_at(eval, needs_propagate, ~x, ~y, ~z)
    });
  };
};

let force_and_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let {xs, ys, zs, _} = eval;
  force_no_propagate(eval, ~x, ~y, ~z, tile_id);
  let needs_propagate = ref([]);
  push_neighbor_coords(needs_propagate, ~xs, ~ys, ~zs, ~x, ~y, ~z);
  finish_propagating(eval, needs_propagate);
};

let propagate_all = eval => {
  let needs_propagate = ref([]);
  let {xs, ys, zs, _} = eval;

  for (x in 0 to xs - 1) {
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        propagate_at(eval, needs_propagate, ~x, ~y, ~z);
      };
    };
  };
  finish_propagating(eval, needs_propagate);
};

let entropy_at = (eval, ~x: int, ~y: int, ~z: int) => {
  Array.sum((module Int), eval.possibilities[x][y][z], ~f=Bool.to_int) - 1;
};

let collapse_at = (eval, ~x, ~y, ~z) => {
  let options =
    Array.filter_mapi(eval.possibilities[x][y][z], ~f=(i, p) =>
      if (p) {
        Some(i);
      } else {
        None;
      }
    );
  if (Array.is_empty(options)) {
    /* TODO */
    failwith("Contradiction, cannot collapse");
  };
  let t = Array.random_element_exn(options);
  force_and_propagate(eval, ~x, ~y, ~z, t);
};

module Test_helpers = {
  let print_entropy = eval => {
    let {xs, ys, zs, _} = eval;
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
                [|"a", "a", "a"|], /* */
                [|"a", "0", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "0", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "0", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
            |],
          ),
          tile(
            ~weight=1.0,
            [|
              [|
                [|"x", "|", "x"|], /* */
                [|"x", "1", "-"|], /* */
                [|"x", "|", "x"|] /* */
              |],
              [|
                [|"x", "|", "x"|], /* */
                [|"x", "1", "-"|], /* */
                [|"x", "|", "x"|] /* */
              |],
              [|
                [|"x", "|", "x"|], /* */
                [|"x", "1", "-"|], /* */
                [|"x", "|", "x"|] /* */
              |],
            |],
          ),
          tile(
            ~weight=1.0,
            [|
              [|
                [|"x", "|", "x"|], /* */
                [|"-", "2", "x"|], /* */
                [|"x", "|", "x"|] /* */
              |],
              [|
                [|"x", "|", "x"|], /* */
                [|"-", "2", "x"|], /* */
                [|"x", "|", "x"|] /* */
              |],
              [|
                [|"x", "|", "x"|], /* */
                [|"-", "2", "x"|], /* */
                [|"x", "|", "x"|] /* */
              |],
            |],
          ),
        ],
      )
    );

  let eval = make_blank_wave(ts, ~xs=3, ~ys=1, ~zs=3);
  propagate_all(eval);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    2 2 2
    2 2 2
    2 2 2
  |};

  force_no_propagate(eval, ~x=0, ~y=0, ~z=1, 1);
  propagate_all(eval);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    1 1 1
    0 0 0
    1 1 1
  |};

  collapse_at(eval, ~x=1, ~y=0, ~z=0);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    0 0 0
    0 0 0
    1 1 1
  |};

  force_and_propagate(eval, ~x=2, ~y=0, ~z=2, 2);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    0 0 0
    0 0 0
    0 0 0
  |};
};