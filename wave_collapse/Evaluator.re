open! Core;

type wave_evaluator('a) = {
  tileset: Tileset.tileset('a),
  xs: int,
  ys: int,
  zs: int,
  ts: int,
  /* Which tiles are not yet banned here. Used to avoid double-banning and
   * quickly count entropy */
  /* xyzt */
  possibilities: array(bool),
  /* In each direction, how many neighboring possible tiles would be compatible
   * with this tile */
  /* direction_inverted xyzt */
  supporters: array(array(int)),
  mutable entropy_queue: Priority_queue.Int.t((int, int, int)),
  mutable total_entropy: int,
  /* Tiles that have become impossible (ie. supporter count reached 0) but have
   * not yet been propagated */
  needs_ban: Hash_queue.t((int, int, int, int), unit),
};

module Int4 = {
  [@deriving (eq, ord, hash, sexp)]
  type t = (int, int, int, int);
};
module Hq_I4 = Hash_queue.Make(Int4);

let it_of_xyz = (~xs as _: int, ~ys, ~zs, ~ts, x, y, z): int =>
  (z + (y + x * ys) * zs) * ts;

let i_of_xyzt = (~xs as _: int, ~ys, ~zs, ~ts, x, y, z, t): int =>
  t + (z + (y + x * ys) * zs) * ts;

let make_blank_possibilities = (~numtiles: int, xs: int, ys: int, zs: int) =>
  Array.create(~len=xs * ys * zs * numtiles, true);

let copy_a2 = a => Array.(map(a, ~f=copy));

let copy_a3 = a => Array.(map(a, ~f=b => map(b, ~f=copy)));

let copy_a4 = a => Array.(map(a, ~f=b => map(b, ~f=c => map(c, ~f=copy))));

let copy_a5 = a =>
  Array.(map(a, ~f=b => map(b, ~f=c => map(c, ~f=d => map(d, ~f=copy)))));

let make_blank_supporters =
    (~tileset: Tileset.tileset('a), xs: int, ys: int, zs: int) => {
  let ts = Tileset.numtiles(tileset);
  Array.init(Tileset.numdirs, ~f=dir =>
    Array.init(
      xs * ys * zs * ts,
      ~f=i => {
        let t = i % ts;
        /* TODO fixme opposite direction */
        Array.length(tileset.requirements[t][dir]);
      },
    )
  );
};

let copy = eval => {
  let {
    tileset,
    xs,
    ys,
    zs,
    ts,
    possibilities,
    supporters,
    entropy_queue,
    total_entropy,
    needs_ban,
  } = eval;
  assert(Hash_queue.is_empty(needs_ban));
  {
    tileset,
    xs,
    ys,
    zs,
    ts,
    possibilities: Array.copy(possibilities),
    supporters: copy_a2(supporters),
    entropy_queue,
    total_entropy,
    // needs_ban should always be empty, so just make a fresh copy
    needs_ban: Hq_I4.create(),
  };
};

let entropy_at = (eval, it) => {
  let s = ref(0);
  for (t in 0 to eval.ts - 1) {
    s := s^ + Bool.to_int(eval.possibilities[it + t]);
  };
  s^ - 1;
};

let ban = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  let i = it + tile_id;
  if (eval.possibilities[i]) {
    eval.possibilities[i] = false;
    let now_entropy = entropy_at(eval, it);
    eval.total_entropy = eval.total_entropy - 1;
    eval.entropy_queue =
      Priority_queue.Int.insert(eval.entropy_queue, now_entropy, (x, y, z));

    let reqs = eval.tileset.requirements[tile_id];
    for (d in 0 to Tileset.numdirs - 1) {
      eval.supporters[d][i] = 0;
      let (dx, dy, dz) = Tileset.directions[d];
      let nx = x + dx;
      let ny = y + dy;
      let nz = z + dz;
      if (0 <= nx
          && nx < eval.xs
          && 0 <= ny
          && ny < eval.ys
          && 0 <= nz
          && nz < eval.zs) {
        let nit = it_of_xyz(~xs, ~ys, ~zs, ~ts, nx, ny, nz);
        Array.iter(
          reqs[d],
          ~f=t1 => {
            let neigh = eval.supporters[d];
            neigh[nit + t1] = neigh[nit + t1] - 1;
            if (neigh[nit + t1] == 0) {
              Hash_queue.enqueue_back(eval.needs_ban, (nx, ny, nz, t1), ())
              |> ignore;
            };
          },
        );
      };
    };
  };
};

let force_no_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  for (t in 0 to eval.ts - 1) {
    if (t != tile_id) {
      ban(eval, ~x, ~y, ~z, t);
    };
  };
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
    ts: numtiles,
    possibilities: make_blank_possibilities(~numtiles, xs, ys, zs),
    supporters: make_blank_supporters(~tileset, xs, ys, zs),
    entropy_queue,
    total_entropy: xs * ys * zs * (numtiles - 1),
    needs_ban: Hq_I4.create(),
  };
};

let finish_propagating = eval => {
  while (!Hash_queue.is_empty(eval.needs_ban)) {
    let ((x, y, z, t), _) =
      Hash_queue.dequeue_back_with_key_exn(eval.needs_ban);
    ban(eval, ~x, ~y, ~z, t);
  };
};

let force_and_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  force_no_propagate(eval, ~x, ~y, ~z, tile_id);
  finish_propagating(eval);
};

let rec next_lowest_entropy = eval => {
  let (next, entropy_queue) = Priority_queue.Int.extract(eval.entropy_queue);
  eval.entropy_queue = entropy_queue;
  let {xs, ys, zs, ts, _} = eval;
  switch (next) {
  | None => None
  | Some((x, y, z))
      when entropy_at(eval, it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z)) > 0 => next
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
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  let options =
    Mg_util.Range.flat_map(0, ts - 1, t =>
      if (eval.possibilities[it + t]) {
        Some(t);
      } else {
        None;
      }
    );
  if (List.is_empty(options)) {
    /* TODO */
    failwith("Contradiction, cannot collapse");
  };
  let t =
    random_tile_by_weight(Array.of_list(options), ~tileset=eval.tileset);
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
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  if (entropy_at(eval, it) != 0) {
    failwith("Cannot get because entropy != 0");
  };
  Mg_util.Range.find_exn(0, ts - 1, t => eval.possibilities[it + t]);
};

let observe_at = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  if (entropy_at(eval, it) != 0) {
    None;
  } else {
    let t = Mg_util.Range.find_exn(0, ts - 1, t => eval.possibilities[it + t]);
    Some(t);
  };
};

module Test_helpers = {
  let print_entropy = eval => {
    let {xs, ys, zs, ts, _} = eval;
    for (y in 0 to ys - 1) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
          let e = entropy_at(eval, it);
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
  Test_helpers.print_entropy(eval);
  %expect
  {|
    2 2 2
    2 2 2
    2 2 2

    total_entropy = 18
  |};

  force_and_propagate(eval, ~x=0, ~y=0, ~z=1, 1);
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