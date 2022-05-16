open! Core;

type wave_evaluator('a) = {
  tileset: Tileset.tileset('a),
  xs: int,
  ys: int,
  zs: int,
  /* x y z kind */
  mutable possibilities: array(array(array(array(bool)))),
  mutable entropy_queue: Priority_queue.Int.t((int, int, int)),
  mutable total_entropy: int,
  needs_propagate: Hash_queue.t((int, int, int), unit),
};

module Int3 = {
  [@deriving (eq, ord, hash, sexp)]
  type t = (int, int, int);
};
module Hq_I3 = Hash_queue.Make(Int3);

let make_blank_possibilities = (~numtiles: int, xs: int, ys: int, zs: int) =>
  Array.init(xs, ~f=_ =>
    Array.init(ys, ~f=_ =>
      Array.init(zs, ~f=_ => Array.create(~len=numtiles, true))
    )
  );

let copy_a4 = a => Array.(map(a, ~f=b => map(b, ~f=c => map(c, ~f=copy))));

let copy = eval => {
  let {
    tileset,
    xs,
    ys,
    zs,
    possibilities,
    entropy_queue,
    total_entropy,
    needs_propagate,
  } = eval;
  assert(Hash_queue.is_empty(needs_propagate));
  {
    tileset,
    xs,
    ys,
    zs,
    possibilities: copy_a4(possibilities),
    entropy_queue,
    total_entropy,
    // needs_propagate should always be empty, so just make a fresh copy
    needs_propagate: Hq_I3.create(),
  };
};

let entropy_at = (eval, ~x: int, ~y: int, ~z: int) => {
  Array.sum((module Int), eval.possibilities[x][y][z], ~f=Bool.to_int) - 1;
};

let force_no_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let numtiles = Tileset.numtiles(eval.tileset);
  let prev_entropy = entropy_at(eval, ~x, ~y, ~z);
  for (t in 0 to numtiles - 1) {
    eval.possibilities[x][y][z][t] = t == tile_id;
  };
  // Entropy is now 0, so no need to queue
  eval.total_entropy = eval.total_entropy - prev_entropy;
};

let make_blank_wave = (tileset, ~xs, ~ys, ~zs) => {
  let numtiles = Tileset.numtiles(tileset);
  // We don't need to enqueue all coords, just one. As soon as the wave starts
  // to collapse, the full entropy entries will become unreachable in the queue.
  // We just need one entry to start the process.
  let entropy_queue =
    Priority_queue.Int.insert(
      Priority_queue.Int.empty,
      numtiles - 1,
      (0, 0, 0),
    );
  {
    tileset,
    xs,
    ys,
    zs,
    possibilities: make_blank_possibilities(~numtiles, xs, ys, zs),
    entropy_queue,
    total_entropy: xs * ys * zs * (numtiles - 1),
    needs_propagate: Hq_I3.create(),
  };
};

let tile_fits_at =
    (eval: wave_evaluator('a), ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let {possibilities, tileset, _} = eval;
  (
    // x0_fits
    if (x > 0) {
      let neigh = possibilities[x - 1][y][z];
      Array.exists(tileset.x0_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  )
  && (
    // x1_fits
    if (x < eval.xs - 1) {
      let neigh = possibilities[x + 1][y][z];
      Array.exists(tileset.x1_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  )
  && (
    // y0_fits
    if (y > 0) {
      let neigh = possibilities[x][y - 1][z];
      Array.exists(tileset.y0_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  )
  && (
    // y1_fits
    if (y < eval.ys - 1) {
      let neigh = possibilities[x][y + 1][z];
      Array.exists(tileset.y1_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  )
  && (
    // z0_fits
    if (z > 0) {
      let neigh = possibilities[x][y][z - 1];
      Array.exists(tileset.z0_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  )
  && (
    // z1_fits
    if (z < eval.zs - 1) {
      let neigh = possibilities[x][y][z + 1];
      Array.exists(tileset.z1_requirements[tile_id], ~f=t1 => neigh[t1]);
    } else {
      true;
    }
  );
};

let push_neighbor_coords = (ls, ~xs, ~ys, ~zs, ~x, ~y, ~z) => {
  if (x > 0) {
    Hash_queue.enqueue_back(ls, (x - 1, y, z), ()) |> ignore;
  };
  if (x < xs - 1) {
    Hash_queue.enqueue_back(ls, (x + 1, y, z), ()) |> ignore;
  };

  if (y > 0) {
    Hash_queue.enqueue_back(ls, (x, y - 1, z), ()) |> ignore;
  };
  if (y < ys - 1) {
    Hash_queue.enqueue_back(ls, (x, y + 1, z), ()) |> ignore;
  };

  if (z > 0) {
    Hash_queue.enqueue_back(ls, (x, y, z - 1), ()) |> ignore;
  };
  if (z < zs - 1) {
    Hash_queue.enqueue_back(ls, (x, y, z + 1), ()) |> ignore;
  };
};

let propagate_at = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, _} = eval;
  let numtiles = Tileset.numtiles(eval.tileset);
  let removals = ref(0);
  let here = eval.possibilities[x][y][z];
  for (t in 0 to numtiles - 1) {
    if (here[t] && !tile_fits_at(eval, ~x, ~y, ~z, t)) {
      here[t] = false;
      incr(removals);
    };
  };
  if (removals^ != 0) {
    let now_entropy = entropy_at(eval, ~x, ~y, ~z);
    eval.total_entropy = eval.total_entropy - removals^;
    eval.entropy_queue =
      Priority_queue.Int.insert(eval.entropy_queue, now_entropy, (x, y, z));
    push_neighbor_coords(eval.needs_propagate, ~xs, ~ys, ~zs, ~x, ~y, ~z);
  };
};

let finish_propagating = eval => {
  while (!Hash_queue.is_empty(eval.needs_propagate)) {
    let ((x, y, z), _) =
      Hash_queue.dequeue_back_with_key_exn(eval.needs_propagate);
    propagate_at(eval, ~x, ~y, ~z);
  };
};

let force_and_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let {xs, ys, zs, _} = eval;
  force_no_propagate(eval, ~x, ~y, ~z, tile_id);
  push_neighbor_coords(eval.needs_propagate, ~xs, ~ys, ~zs, ~x, ~y, ~z);
  finish_propagating(eval);
};

let propagate_all = eval => {
  let {xs, ys, zs, _} = eval;

  for (x in 0 to xs - 1) {
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        propagate_at(eval, ~x, ~y, ~z);
      };
    };
  };
  finish_propagating(eval);
};

let rec next_lowest_entropy = eval => {
  let (next, entropy_queue) = Priority_queue.Int.extract(eval.entropy_queue);
  eval.entropy_queue = entropy_queue;
  switch (next) {
  | None => None
  | Some((x, y, z)) when entropy_at(eval, ~x, ~y, ~z) > 0 => next
  | Some(_) => next_lowest_entropy(eval)
  };
  // TODO randomize
};

let random_tile_by_weight =
    (options: array(int), ~tileset: Tileset.tileset('a)) => {
  let total_weight =
    Array.fold(options, ~init=0.0, ~f=(s, i) => s +. tileset.tiles[i].weight);
  let selected_weight = Random.float(total_weight);

  let rec select = (i, remaining_weight) => {
    let remaining_weight =
      remaining_weight -. tileset.tiles[options[i]].weight;
    if (Float.(remaining_weight <= 0.0)) {
      options[i];
    } else {
      select(i + 1, remaining_weight);
    };
  };

  select(0, selected_weight);
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
  let t = random_tile_by_weight(options, ~tileset=eval.tileset);
  force_and_propagate(eval, ~x, ~y, ~z, t);
};

let try_collapse_next_lowest_entropy = eval => {
  switch (next_lowest_entropy(eval)) {
  | Some((x, y, z)) =>
    collapse_at(eval, ~x, ~y, ~z);
    true;
  | None => false
  };
};

let collapse_all = eval => {
  while (try_collapse_next_lowest_entropy(eval)) {
    ();
  };
};

let observe_at_exn = (eval, ~x, ~y, ~z) => {
  if (entropy_at(eval, ~x, ~y, ~z) != 0) {
    failwith("Cannot get because entropy != 0");
  };
  let (t, _) = Array.findi_exn(eval.possibilities[x][y][z], ~f=(_, b) => b);
  t;
};

let observe_at = (eval, ~x, ~y, ~z) =>
  if (entropy_at(eval, ~x, ~y, ~z) != 0) {
    None;
  } else {
    let (t, _) =
      Array.findi_exn(eval.possibilities[x][y][z], ~f=(_, b) => b);
    Some(t);
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
    Printf.printf("total_entropy = %d\n\n", eval.total_entropy);
  };

  let tileset =
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
};

let%expect_test "propagation" = {
  let eval = make_blank_wave(Test_helpers.tileset, ~xs=3, ~ys=1, ~zs=3);
  propagate_all(eval);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    2 2 2
    2 2 2
    2 2 2

    total_entropy = 18
  |};

  force_no_propagate(eval, ~x=0, ~y=0, ~z=1, 1);
  propagate_all(eval);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    1 1 1
    0 0 0
    1 1 1

    total_entropy = 6
  |};

  collapse_at(eval, ~x=1, ~y=0, ~z=0);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    0 0 0
    0 0 0
    1 1 1

    total_entropy = 3
  |};

  force_and_propagate(eval, ~x=2, ~y=0, ~z=2, 2);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    0 0 0
    0 0 0
    0 0 0

    total_entropy = 0
  |};
};

let%expect_test "collapse" = {
  let eval = make_blank_wave(Test_helpers.tileset, ~xs=3, ~ys=1, ~zs=3);
  force_and_propagate(eval, ~x=2, ~y=0, ~z=2, 2);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    1 1 1
    1 1 1
    0 0 0

    total_entropy = 6
  |};

  while (try_collapse_next_lowest_entropy(eval)) {
    Test_helpers.print_entropy(eval);
  };
  %expect
  {|
    0 0 0
    1 1 1
    0 0 0

    total_entropy = 3

    0 0 0
    0 0 0
    0 0 0

    total_entropy = 0
  |};
};