open! Core_kernel;

type tile('a) = {
  id: int,
  weight: float,
  /* x y z item */
  items: array(array(array('a))),
  /* whether the edges should auto-match */
  auto_x0: bool,
  auto_x1: bool,
  auto_y0: bool,
  auto_y1: bool,
  auto_z0: bool,
  auto_z1: bool,
};

type tileset('a) = {
  tilesize: int,
  /* kind tile */
  tiles: array(tile('a)),
  /* x-1 x allowed */
  x_pairs: array(array(bool)),
  /* y-1 y allowed */
  y_pairs: array(array(bool)),
  /* z-1 z allowed */
  z_pairs: array(array(bool)),
};

type wave('a) = {
  tileset: tileset('a),
  /* x y z kind */
  possibilities: array(array(array(array(bool)))),
};

type wave_evaluator('a) = {
  mutable wave: wave('a),
  mutable entropy_queue: Priority_queue.t((int, int, int)),
};

module Builder = {
  type btile('a) = {
    weight: float,
    /* x y z item */
    items: array(array(array('a))),
  };

  let xyz_of_yzx = (yzx: array(array(array('a)))) => {
    let xs = Array.length(yzx[0][0]);
    let ys = Array.length(yzx);
    let zs = Array.length(yzx[0]);

    Array.init(xs, ~f=x =>
      Array.init(ys, ~f=y => Array.init(zs, ~f=z => yzx[ys - 1 - y][z][x]))
    );
  };

  let tile = (~weight: float, items: array(array(array('a)))): btile('a) => {
    items,
    weight,
  };

  let make_blank_pairs = numtiles =>
    Array.init(numtiles, ~f=_ => Array.create(~len=numtiles, false));

  let x_match = (x0: tile('a), x1: tile('a)): bool =>
    if (x0.auto_x1 && x1.auto_x0) {
      let tilesize = Array.length(x0.items);
      let all_match = ref(true);
      for (y in 0 to tilesize - 1) {
        for (z in 0 to tilesize - 1) {
          if (Poly.(x0.items[tilesize - 1][y][z] != x1.items[0][y][z])) {
            all_match := false;
          };
        };
      };
      all_match^;
    } else {
      false;
    };

  let y_match = (y0: tile('a), y1: tile('a)): bool =>
    if (y0.auto_y1 && y1.auto_y0) {
      let tilesize = Array.length(y0.items);
      let all_match = ref(true);
      for (x in 0 to tilesize - 1) {
        for (z in 0 to tilesize - 1) {
          if (Poly.(y0.items[x][tilesize - 1][z] != y1.items[x][0][z])) {
            all_match := false;
          };
        };
      };
      all_match^;
    } else {
      false;
    };

  let z_match = (z0: tile('a), z1: tile('a)): bool =>
    if (z0.auto_z1 && z1.auto_z0) {
      let tilesize = Array.length(z0.items);
      let all_match = ref(true);
      for (x in 0 to tilesize - 1) {
        for (y in 0 to tilesize - 1) {
          if (Poly.(z0.items[x][y][tilesize - 1] != z1.items[x][y][0])) {
            all_match := false;
          };
        };
      };
      all_match^;
    } else {
      false;
    };

  let create_tileset = (~tilesize: int, btiles: list(btile('a))) => {
    let tiles = ref([]);
    let next_id = ref(0);
    let x_pairs_s = Hash_set.Poly.create();
    let y_pairs_s = Hash_set.Poly.create();
    let z_pairs_s = Hash_set.Poly.create();

    List.iter(
      btiles,
      ~f=btile => {
        let xyz_items = xyz_of_yzx(btile.items);
        let xs = Array.length(xyz_items);
        let ys = Array.length(xyz_items[0]);
        let zs = Array.length(xyz_items[0][0]);

        if (xs != tilesize || ys != tilesize || zs != tilesize) {
          failwith("TODO");
        };

        let tile = {
          id: next_id^,
          weight: btile.weight,
          items: xyz_items,
          auto_x0: true,
          auto_x1: true,
          auto_y0: true,
          auto_y1: true,
          auto_z0: true,
          auto_z1: true,
        };
        next_id := next_id^ + 1;

        List.iter(
          tiles^,
          ~f=pred => {
            if (x_match(pred, tile)) {
              Hash_set.add(x_pairs_s, (pred.id, tile.id));
            };
            if (x_match(tile, pred)) {
              Hash_set.add(x_pairs_s, (tile.id, pred.id));
            };
            if (y_match(pred, tile)) {
              Hash_set.add(y_pairs_s, (pred.id, tile.id));
            };
            if (y_match(tile, pred)) {
              Hash_set.add(y_pairs_s, (tile.id, pred.id));
            };
            if (z_match(pred, tile)) {
              Hash_set.add(z_pairs_s, (pred.id, tile.id));
            };
            if (z_match(tile, pred)) {
              Hash_set.add(z_pairs_s, (tile.id, pred.id));
            };
          },
        );

        tiles := [tile, ...tiles^];
      },
    );

    let tiles = Array.of_list(tiles^);
    let numtiles = Array.length(tiles);
    let x_pairs = make_blank_pairs(numtiles);
    let y_pairs = make_blank_pairs(numtiles);
    let z_pairs = make_blank_pairs(numtiles);

    for (t0 in 0 to numtiles - 1) {
      for (t1 in 0 to numtiles - 1) {
        x_pairs[t0][t1] = Hash_set.mem(x_pairs_s, (t0, t1));
        y_pairs[t0][t1] = Hash_set.mem(y_pairs_s, (t0, t1));
        z_pairs[t0][t1] = Hash_set.mem(z_pairs_s, (t0, t1));
      };
    };

    {tilesize, tiles, x_pairs, y_pairs, z_pairs};
  };
};

let%expect_test "single tiles" = {
  let ts =
    Builder.(
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
                [|"g", "h", "i"|],
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

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.x_pairs),
  );
  %expect
  "((false false false) (false false false) (true false true))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.y_pairs),
  );
  %expect
  "((true false false) (false true false) (false false true))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.z_pairs),
  );
  %expect
  "((false true false) (false true false) (false false false))";
};