open! Core;

[@deriving sexp_of]
type contradiction_action =
  | Banning_tile(string)
  | Finding_tile_with_walkability(bool, bool)
  | Placing_tile_with_walkability(string, Tileset.walkability, (bool, bool));

[@deriving sexp_of]
type contradiction_info = {
  contradiction_at: (int, int, int),
  contradiction_action,
  during_collapse: option((int, int, int, string)),
};
exception Contradiction(contradiction_info);

type wave_evaluator('a, 'tag) = {
  tileset: Tileset.tileset('a, 'tag),
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
  /* xyzt-direction_inverted */
  supporters: array(int),
  mutable frontier: Priority_queue.Int.t((int, int, int)),
  mutable total_entropy: int,
  /* Tiles that have become impossible (ie. supporter count reached 0) but have
   * not yet been propagated */
  needs_ban: Hash_queue.t((int, int, int, int), unit),
  // TODO pull out of Frontier_stack
  walkable: Frontier_stack.Hs_I3.t,
  unwalkable: Frontier_stack.Hs_I3.t,
};

module Int4 = {
  [@deriving (eq, ord, hash, sexp)]
  type t = (int, int, int, int);
};
module Hq_I4 = Hash_queue.Make(Int4);

let assert_within = (n, size) =>
  if (!(0 <= n && n < size)) {
    failwithf("Expected 0 <= %d < %d", n, size, ());
  };

let it_of_xyz = (~xs: int, ~ys, ~zs, ~ts, x, y, z): int => {
  assert_within(x, xs);
  assert_within(y, ys);
  assert_within(z, zs);
  (z + (y + x * ys) * zs) * ts;
};

let i_of_xyzt = (~xs: int, ~ys, ~zs, ~ts, x, y, z, t): int => {
  assert_within(x, xs);
  assert_within(y, ys);
  assert_within(z, zs);
  assert_within(t, ts);
  t + (z + (y + x * ys) * zs) * ts;
};

let si_of_xyztd = (~xs as _: int, ~ys, ~zs, ~ts, x, y, z, t, d): int =>
  d + (t + (z + (y + x * ys) * zs) * ts) * Tileset.numdirs;

let make_blank_possibilities = (~numtiles: int, xs: int, ys: int, zs: int) =>
  Array.create(~len=xs * ys * zs * numtiles, true);

let copy_a2 = a => Array.(map(a, ~f=copy));

let copy_a3 = a => Array.(map(a, ~f=b => map(b, ~f=copy)));

let copy_a4 = a => Array.(map(a, ~f=b => map(b, ~f=c => map(c, ~f=copy))));

let copy_a5 = a =>
  Array.(map(a, ~f=b => map(b, ~f=c => map(c, ~f=d => map(d, ~f=copy)))));

let make_blank_supporters =
    (~tileset: Tileset.tileset('a, 'tag), xs: int, ys: int, zs: int) => {
  let ts = Tileset.numtiles(tileset);
  Array.init(
    xs * ys * zs * ts * Tileset.numdirs,
    ~f=i => {
      let dir = i % Tileset.numdirs;
      let t = i / Tileset.numdirs % ts;
      Array.length(tileset.supportees[t][Tileset.flip_direction(dir)]);
    },
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
    frontier,
    total_entropy,
    needs_ban,
    walkable,
    unwalkable,
  } = eval;
  assert(Hash_queue.is_empty(needs_ban));
  {
    tileset,
    xs,
    ys,
    zs,
    ts,
    possibilities: Array.copy(possibilities),
    supporters: Array.copy(supporters),
    frontier,
    total_entropy,
    // needs_ban should always be empty, so just make a fresh copy
    needs_ban: Hq_I4.create(),
    walkable: Hash_set.copy(walkable),
    unwalkable: Hash_set.copy(unwalkable),
  };
};

let blit = (src, dest) => {
  assert(src.xs == dest.xs);
  assert(src.ys == dest.ys);
  assert(src.zs == dest.zs);
  assert(src.ts == dest.ts);
  let {
    tileset: _,
    xs: _,
    ys: _,
    zs: _,
    ts: _,
    possibilities,
    supporters,
    frontier,
    total_entropy,
    needs_ban,
    walkable,
    unwalkable,
  } = src;
  Array.blito(~src=possibilities, ~dst=dest.possibilities, ());
  Array.blito(~src=supporters, ~dst=dest.supporters, ());
  dest.frontier = frontier;
  dest.total_entropy = total_entropy;
  assert(Hash_queue.is_empty(needs_ban));
  Hash_queue.clear(dest.needs_ban);
  Hash_set.clear(dest.walkable);
  Hash_set.iter(walkable, ~f=c => Hash_set.add(dest.walkable, c));
  Hash_set.clear(dest.unwalkable);
  Hash_set.iter(unwalkable, ~f=c => Hash_set.add(dest.unwalkable, c));
};

let add_neighbors_to_frontier = (eval, x, y, z, is_walkable) => {
  let {xs, ys, zs, _} = eval;
  let p = is_walkable ? 0 : 1;
  if (x > 0) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x - 1, y, z));
  };
  if (x < xs - 1) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x + 1, y, z));
  };
  if (y > 0) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x, y - 1, z));
  };
  if (y < ys - 1) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x, y + 1, z));
  };
  if (z > 0) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x, y, z - 1));
  };
  if (z < zs - 1) {
    eval.frontier =
      Priority_queue.Int.insert(eval.frontier, p, (x, y, z + 1));
  };
};

let entropy_at = (eval, it) => {
  let s = ref(0);
  for (t in 0 to eval.ts - 1) {
    s := s^ + Bool.to_int(eval.possibilities[it + t]);
  };
  s^ - 1;
};

let observe_at_exn = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  if (entropy_at(eval, it) != 0) {
    failwith("Cannot observe because entropy != 0");
  };
  Mg_util.Range.find_exn(0, ts - 1, t => eval.possibilities[it + t]);
};

let observe_at = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  let e = entropy_at(eval, it);
  if (e != 0) {
    Result.Error(e);
  } else {
    let t = Mg_util.Range.find_exn(0, ts - 1, t => eval.possibilities[it + t]);
    Result.Ok(t);
  };
};

let get_neighbor_walkability = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, _} = eval;
  let has_walkable = ref(Hash_set.mem(eval.walkable, (x, y, z)));
  let has_unwalkable = ref(Hash_set.mem(eval.unwalkable, (x, y, z)));
  if (x > 0) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x - 1, y, z));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x - 1, y, z));
  };
  if (x < xs - 1) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x + 1, y, z));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x + 1, y, z));
  };
  if (y > 0) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x, y - 1, z));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x, y - 1, z));
  };
  if (y < ys - 1) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x, y + 1, z));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x, y + 1, z));
  };
  if (z > 0) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x, y, z - 1));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x, y, z - 1));
  };
  if (z < zs - 1) {
    has_walkable :=
      has_walkable^ || Hash_set.mem(eval.walkable, (x, y, z + 1));
    has_unwalkable :=
      has_unwalkable^ || Hash_set.mem(eval.unwalkable, (x, y, z + 1));
  };
  (has_walkable^, has_unwalkable^);
};

let can_place_walkability =
    (walkability: Tileset.walkability, neighbor_walkability) => {
  let (has_walkable, has_unwalkable) = neighbor_walkability;
  switch (walkability) {
  | Walkable => has_walkable
  | Boundary => has_walkable
  | Unwalkable => has_unwalkable
  | Irrelevant => true
  };
};

let mark_walkability = (eval, ~x, ~y, ~z, walkability: Tileset.walkability) => {
  switch (walkability) {
  | Walkable => Hash_set.add(eval.walkable, (x, y, z))
  | Unwalkable => Hash_set.add(eval.unwalkable, (x, y, z))
  | Boundary
  | Irrelevant => ()
  };
};

let ban = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  let is_walkable =
    fun
    | Tileset.Walkable => true
    | _ => false;

  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  let i = it + tile_id;
  if (eval.possibilities[i]) {
    eval.possibilities[i] = false;
    let now_entropy = entropy_at(eval, it);
    if (now_entropy < 0) {
      raise_notrace(
        Contradiction({
          contradiction_at: (x, y, z),
          contradiction_action:
            Banning_tile(eval.tileset.tiles[tile_id].name),
          during_collapse: None,
        }),
      );
    } else if (now_entropy == 0) {
      let tile = observe_at_exn(eval, ~x, ~y, ~z);
      // let nw = get_neighbor_walkability(eval, ~x, ~y, ~z);
      let w = eval.tileset.tiles[tile].walkability;
      // if (!can_place_walkability(w, nw)) {
      //   raise_notrace(
      //     Contradiction({
      //       contradiction_at: (x, y, z),
      //       contradiction_action:
      //         Placing_tile_with_walkability(
      //           eval.tileset.tiles[tile_id].name,
      //           w,
      //           nw,
      //         ),
      //       during_collapse: None,
      //     }),
      //   );
      // };
      mark_walkability(eval, ~x, ~y, ~z, w);
      let w_priority = is_walkable(w);
      add_neighbors_to_frontier(eval, x, y, z, w_priority);
    };
    eval.total_entropy = eval.total_entropy - 1;

    let sups = eval.tileset.supportees[tile_id];
    let sid = si_of_xyztd(~xs, ~ys, ~zs, ~ts, x, y, z, tile_id, 0);
    for (d in 0 to Tileset.numdirs - 1) {
      eval.supporters[sid + d] = 0;
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
        Array.iter(
          sups[d],
          ~f=t1 => {
            let nsi = si_of_xyztd(~xs, ~ys, ~zs, ~ts, nx, ny, nz, t1, d);
            eval.supporters[nsi] = eval.supporters[nsi] - 1;
            if (eval.supporters[nsi] == 0) {
              Hash_queue.enqueue_back(eval.needs_ban, (nx, ny, nz, t1), ())
              |> ignore;
            };
          },
        );
      };
    };
  };
};

[@inline]
let wrap_contradiction_error = f =>
  try(f()) {
  | Contradiction(c) =>
    let msg = Sexp.to_string_hum(sexp_of_contradiction_info(c));
    failwithf("Caused contradiction outside collapse: %s", msg, ());
  };

let force_no_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) =>
  for (t in 0 to eval.ts - 1) {
    if (t != tile_id) {
      ban(eval, ~x, ~y, ~z, t);
    };
  };

let ban_multi_no_propagate =
    (eval, ~x: int, ~y: int, ~z: int, tiles_to_ban: array(int)) =>
  wrap_contradiction_error(() => {
    Array.iter(tiles_to_ban, ~f=t => ban(eval, ~x, ~y, ~z, t))
  });

let finish_propagating = eval => {
  while (!Hash_queue.is_empty(eval.needs_ban)) {
    let ((x, y, z, t), _) =
      Hash_queue.dequeue_back_with_key_exn(eval.needs_ban);
    ban(eval, ~x, ~y, ~z, t);
  };
};

let ban_impossible_tiles = eval => {
  let {xs, ys, zs, _} = eval;
  let impossible = Array.filter(eval.tileset.tiles, ~f=t => t.is_impossible);
  if (!Array.is_empty(impossible)) {
    for (x in 0 to xs - 1) {
      for (y in 0 to ys - 1) {
        for (z in 0 to zs - 1) {
          Array.iter(impossible, ~f=tile => {ban(eval, ~x, ~y, ~z, tile.id)});
        };
      };
    };
    finish_propagating(eval);
  };
};

let make_blank_wave = (tileset, ~xs, ~ys, ~zs) => {
  let numtiles = Tileset.numtiles(tileset);
  let wave = {
    tileset,
    xs,
    ys,
    zs,
    ts: numtiles,
    possibilities: make_blank_possibilities(~numtiles, xs, ys, zs),
    supporters: make_blank_supporters(~tileset, xs, ys, zs),
    frontier: Priority_queue.Int.empty,
    total_entropy: xs * ys * zs * (numtiles - 1),
    needs_ban: Hq_I4.create(),
    walkable: Frontier_stack.Hs_I3.create(),
    unwalkable: Frontier_stack.Hs_I3.create(),
  };
  ban_impossible_tiles(wave);
  wave;
};

let force_and_propagate = (eval, ~x: int, ~y: int, ~z: int, tile_id: int) => {
  force_no_propagate(eval, ~x, ~y, ~z, tile_id);
  finish_propagating(eval);
};

let rec next_lowest_entropy = eval => {
  let {xs, ys, zs, ts, _} = eval;
  let (next, frontier) = Priority_queue.Int.extract(eval.frontier);
  eval.frontier = frontier;
  switch (next) {
  | None => None
  | Some((x, y, z))
      when entropy_at(eval, it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z)) > 0 => next
  | Some(_) => next_lowest_entropy(eval)
  };
  // TODO randomize
};

let random_tile_by_weight =
    (options: array(int), ~tileset: Tileset.tileset('a, 'tag)) => {
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

let item_at_exn = (eval: wave_evaluator('a, 'tag), ~x: int, ~y: int, ~z: int) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  let t = observe_at_exn(eval, ~x=tx, ~y=ty, ~z=tz);
  eval.tileset.tiles[t].items[subx][suby][subz];
};

let item_at =
    (eval: wave_evaluator('a, 'tag), ~x: int, ~y: int, ~z: int, ~default) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  switch (observe_at(eval, ~x=tx, ~y=ty, ~z=tz)) {
  | Result.Ok(t) => eval.tileset.tiles[t].items[subx][suby][subz]
  | Result.Error(_) => default
  };
};

let item_or_entropy_at =
    (eval: wave_evaluator('a, 'tag), ~x: int, ~y: int, ~z: int) => {
  let tsz = eval.tileset.tilesize;
  let tx = max(0, (x - 1) / (tsz - 1));
  let ty = max(0, (y - 1) / (tsz - 1));
  let tz = max(0, (z - 1) / (tsz - 1));
  let subx = x - tx * (tsz - 1);
  let suby = y - ty * (tsz - 1);
  let subz = z - tz * (tsz - 1);

  switch (observe_at(eval, ~x=tx, ~y=ty, ~z=tz)) {
  | Result.Ok(t) => Result.Ok(eval.tileset.tiles[t].items[subx][suby][subz])
  | Result.Error(_) as e => e
  };
};

let collapse_at = (eval, ~x, ~y, ~z) => {
  let {xs, ys, zs, ts, _} = eval;
  let it = it_of_xyz(~xs, ~ys, ~zs, ~ts, x, y, z);
  let (has_walkable, has_unwalkable) =
    get_neighbor_walkability(eval, ~x, ~y, ~z);
  let neither_walkability = !(has_walkable || has_unwalkable);
  let options =
    Mg_util.Range.flat_map(0, ts - 1, t =>
      if (eval.possibilities[it + t]) {
        if (neither_walkability) {
          Some(t);
        } else {
          let tile = eval.tileset.tiles[t];
          switch (tile.walkability) {
          | Walkable when has_walkable => Some(t)
          | Unwalkable when has_unwalkable => Some(t)
          | Boundary when has_walkable /*&& has_unwalkable*/ => Some(t)
          | Irrelevant => Some(t)
          | _ => None
          };
        };
      } else {
        None;
      }
    );

  let name_of = Tileset.name_of(eval.tileset);
  if (!List.is_empty(options)) {
    let t =
      random_tile_by_weight(Array.of_list(options), ~tileset=eval.tileset);
    // Printf.printf("Collapsing %d %d %d to %s\n", x, y, z, name_of(t));
    try(force_and_propagate(eval, ~x, ~y, ~z, t)) {
    | Contradiction(c) =>
      raise_notrace(
        Contradiction({...c, during_collapse: Some((x, y, z, name_of(t)))}),
      )
    };
  } else {
    raise_notrace(
      Contradiction({
        contradiction_at: (x, y, z),
        contradiction_action:
          Finding_tile_with_walkability(has_walkable, has_unwalkable),
        during_collapse: None,
      }),
    );
  };
};

let try_collapse_next_lowest_entropy = eval => {
  switch (next_lowest_entropy(eval)) {
  | Some((x, y, z)) =>
    collapse_at(eval, ~x, ~y, ~z);
    true;
  | None => false
  };
};

let collapse_all = (~peek=ignore, eval) => {
  let start = copy(eval);
  let rec with_tries = tries =>
    if (tries > 0) {
      try(
        while (try_collapse_next_lowest_entropy(eval)) {
          ();
        }
      ) {
      | Contradiction(c) =>
        Printf.printf(
          "*** contra: %s\n",
          Sexp.to_string_hum([%sexp_of: contradiction_info](c)),
        );
        peek(eval);
        blit(start, eval);
        with_tries(tries - 1);
      };
    } else {
      failwith("Ran out of tries");
    };
  with_tries(10);
};

module Test_helpers = {
  let print_entropy = eval => {
    let {xs, ys, zs, ts, _} = eval;
    for (y in ys - 1 downto 0) {
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

  let print_supporters_at = (eval, x, y, z) => {
    let {xs, ys, zs, ts, _} = eval;
    for (t in 0 to eval.ts - 1) {
      let i = i_of_xyzt(~xs, ~ys, ~zs, ~ts, x, y, z, t);
      let possible = eval.possibilities[i];
      Printf.printf("%d: %s", t, possible ? " " : "B");
      for (d in 0 to Tileset.numdirs - 1) {
        Printf.printf(
          " %d",
          eval.supporters[si_of_xyztd(~xs, ~ys, ~zs, ~ts, x, y, z, t, d)],
        );
      };
      Printf.printf("\n");
    };
  };

  type tag =
    | A
    | One
    | Two
    | Three;

  let tileset =
    Tileset.(
      create_tileset(
        ~tilesize=3,
        [
          tile(
            ~tags=[A],
            ~weight=0.0,
            Unwalkable,
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
            ~tags=[One],
            Unwalkable,
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
            ~tags=[Two],
            Unwalkable,
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
          tile(
            ~weight=0.0,
            ~tags=[Three],
            Unwalkable,
            [|
              [|
                [|"a", "|", "x"|], /* */
                [|"a", "3", "-"|], /* */
                [|"a", "|", "x"|] /* */
              |],
              [|
                [|"a", "|", "x"|], /* */
                [|"a", "3", "-"|], /* */
                [|"a", "|", "x"|] /* */
              |],
              [|
                [|"a", "|", "x"|], /* */
                [|"a", "3", "-"|], /* */
                [|"a", "|", "x"|] /* */
              |],
            |],
          ),
        ],
      )
    );

  let vertical_tileset =
    Tileset.(
      create_tileset(
        ~tilesize=2,
        ~tagfix="",
        [
          tile(
            Unwalkable,
            [|
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "1", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "Y", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "1", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
            |],
          ),
          tile(
            Unwalkable,
            [|
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "1", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "X", "a"|], /* */
                [|"a", "a", "a"|] /* */
              |],
              [|
                [|"a", "a", "a"|], /* */
                [|"a", "0", "a"|], /* */
                [|"a", "a", "a"|] /* */
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
    3 3 3
    3 3 3
    3 3 3

    total_entropy = 27
  |};

  Test_helpers.print_supporters_at(eval, 0, 0, 0);
  %expect
  {|
    0:   2 1 1 1 1 1
    1:   1 1 1 1 2 2
    2:   1 2 1 1 2 2
    3:   1 1 1 1 1 1
  |};

  force_and_propagate(eval, ~x=0, ~y=0, ~z=1, 1);

  Test_helpers.print_supporters_at(eval, 1, 0, 1);
  %expect
  {|
    0: B 0 0 0 0 0 0
    1: B -1 0 0 0 0 0
    2:   1 1 1 1 2 2
    3: B -1 0 0 0 0 0
  |};

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

let%expect_test "vertical propagation" = {
  Tileset.Test_helpers.dump_tileset(
    Test_helpers.vertical_tileset,
    ~show_item=Fn.id,
  );
  %expect
  {|
    Tile 0:
      a a
      a Y

      a a
      a 1

      4 5 6 7 12 13 14 15 -X+ 4
      2 10 -Y+ 2
      1 3 5 7 9 11 13 15 -Z+ 1
    ----------
    Tile 1:
      a Y
      a a

      a 1
      a a

      4 5 6 7 12 13 14 15 -X+ 5
      3 11 -Y+ 3
      0 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 2:
      a a
      a 1

      a a
      a Y

      4 5 6 7 12 13 14 15 -X+ 6
      0 -Y+ 0
      1 3 5 7 9 11 13 15 -Z+ 3
    ----------
    Tile 3:
      a 1
      a a

      a Y
      a a

      4 5 6 7 12 13 14 15 -X+ 7
      1 -Y+ 1
      2 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 4:
      a a
      Y a

      a a
      1 a

      0 -X+ 0 1 2 3 8 9 10 11
      6 14 -Y+ 6
      1 3 5 7 9 11 13 15 -Z+ 5
    ----------
    Tile 5:
      Y a
      a a

      1 a
      a a

      1 -X+ 0 1 2 3 8 9 10 11
      7 15 -Y+ 7
      4 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 6:
      a a
      1 a

      a a
      Y a

      2 -X+ 0 1 2 3 8 9 10 11
      4 -Y+ 4
      1 3 5 7 9 11 13 15 -Z+ 7
    ----------
    Tile 7:
      1 a
      a a

      Y a
      a a

      3 -X+ 0 1 2 3 8 9 10 11
      5 -Y+ 5
      6 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 8: IMPOSSIBLE!
      a a
      a X

      a a
      a 0

      4 5 6 7 12 13 14 15 -X+ 12
      -Y+ 10
      1 3 5 7 9 11 13 15 -Z+ 9
    ----------
    Tile 9: IMPOSSIBLE!
      a X
      a a

      a 0
      a a

      4 5 6 7 12 13 14 15 -X+ 13
      -Y+ 11
      8 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 10:
      a a
      a 1

      a a
      a X

      4 5 6 7 12 13 14 15 -X+ 14
      8 -Y+ 0
      1 3 5 7 9 11 13 15 -Z+ 11
    ----------
    Tile 11:
      a 1
      a a

      a X
      a a

      4 5 6 7 12 13 14 15 -X+ 15
      9 -Y+ 1
      10 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 12: IMPOSSIBLE!
      a a
      X a

      a a
      0 a

      8 -X+ 0 1 2 3 8 9 10 11
      -Y+ 14
      1 3 5 7 9 11 13 15 -Z+ 13
    ----------
    Tile 13: IMPOSSIBLE!
      X a
      a a

      0 a
      a a

      9 -X+ 0 1 2 3 8 9 10 11
      -Y+ 15
      12 -Z+ 0 2 4 6 8 10 12 14
    ----------
    Tile 14:
      a a
      1 a

      a a
      X a

      10 -X+ 0 1 2 3 8 9 10 11
      12 -Y+ 4
      1 3 5 7 9 11 13 15 -Z+ 15
    ----------
    Tile 15:
      1 a
      a a

      X a
      a a

      11 -X+ 0 1 2 3 8 9 10 11
      13 -Y+ 5
      14 -Z+ 0 2 4 6 8 10 12 14
    ----------
  |};

  let eval =
    make_blank_wave(Test_helpers.vertical_tileset, ~xs=2, ~ys=5, ~zs=2);

  Test_helpers.print_supporters_at(eval, 0, 0, 0);
  %expect
  {|
    0:   1 8 1 2 1 8
    1:   1 8 1 2 6 1
    2:   1 8 1 1 1 8
    3:   1 8 1 1 6 1
    4:   6 1 1 2 1 8
    5:   6 1 1 2 6 1
    6:   6 1 1 1 1 8
    7:   6 1 1 1 6 1
    8: B -1 0 -1 0 -1 0
    9: B -1 0 -1 0 -2 0
    10:   1 8 1 1 1 8
    11:   1 8 1 1 6 1
    12: B -2 0 -1 0 -1 0
    13: B -2 0 -1 0 -2 0
    14:   6 1 1 1 1 8
    15:   6 1 1 1 6 1
  |};

  Test_helpers.print_entropy(eval);
  %expect
  {|
    7 7
    7 7

    7 7
    7 7

    7 7
    7 7

    7 7
    7 7

    11 11
    11 11

    total_entropy = 156
  |};

  force_and_propagate(eval, ~x=0, ~y=0, ~z=0, 0);

  Test_helpers.print_supporters_at(eval, 0, 1, 0);
  %expect
  {|
    0: B -1 0 -1 0 -1 0
    1: B 0 0 0 0 0 0
    2:   1 8 1 1 1 8
    3: B 0 0 0 0 0 0
    4: B 0 0 0 0 0 0
    5: B -4 0 0 0 0 0
    6: B 0 0 0 0 0 0
    7: B 0 0 0 0 0 0
    8: B -1 0 -1 0 -1 0
    9: B -1 0 -1 0 -8 0
    10: B 0 0 0 0 0 0
    11: B 0 0 -1 0 -4 0
    12: B -8 0 -1 0 -1 0
    13: B -8 0 -1 0 -8 0
    14: B -4 0 -1 0 0 0
    15: B -4 0 -1 0 -4 0
  |};

  Test_helpers.print_entropy(eval);
  %expect
  {|
    0 0
    0 0

    0 0
    0 0

    0 0
    0 0

    0 0
    0 0

    0 0
    0 0

    total_entropy = 0
  |};
};

let%expect_test "collapse" = {
  let eval = make_blank_wave(Test_helpers.tileset, ~xs=3, ~ys=1, ~zs=3);
  force_and_propagate(eval, ~x=2, ~y=0, ~z=2, 2);
  Test_helpers.print_entropy(eval);
  %expect
  {|
    2 2 1
    2 2 1
    1 1 0

    total_entropy = 12
  |};

  while (try_collapse_next_lowest_entropy(eval)) {
    Test_helpers.print_entropy(eval);
  };
  %expect
  {|
    2 2 1
    1 1 0
    1 1 0

    total_entropy = 9

    0 0 0
    0 0 0
    0 0 0

    total_entropy = 0
  |};
};