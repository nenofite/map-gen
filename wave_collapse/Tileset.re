open! Core;

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
  /* here_tile direction other_tiles */
  requirements: array(array(array(int))),
};

type btile('a) = {
  weight: float,
  /* x y z item */
  items: array(array(array('a))),
  rotate: bool,
  flip_x: bool,
  flip_z: bool,
};

let numdirs = 6;
let directions = [|
  ((-1), 0, 0),
  (1, 0, 0),
  (0, (-1), 0),
  (0, 1, 0),
  (0, 0, (-1)),
  (0, 0, 1),
|];
let flip_direction = d =>
  switch (d) {
  | 0 => 1
  | 1 => 0
  | 2 => 3
  | 3 => 2
  | 4 => 5
  | 5
  | _ => 4
  };

let numtiles = ts => Array.length(ts.tiles);

let pop_id = next_id => {
  let id = next_id^;
  next_id := id + 1;
  id;
};

let xyz_of_yzx = (yzx: array(array(array('a)))) => {
  let xs = Array.length(yzx[0][0]);
  let ys = Array.length(yzx);
  let zs = Array.length(yzx[0]);

  Array.init(xs, ~f=x =>
    Array.init(ys, ~f=y => Array.init(zs, ~f=z => yzx[ys - 1 - y][z][x]))
  );
};

let tile =
    (
      ~weight: float=1.0,
      ~rotate=false,
      ~flip_x=false,
      ~flip_z=false,
      items: array(array(array('a))),
    )
    : btile('a) => {
  items,
  rotate,
  flip_x,
  flip_z,
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

let split_multitile =
    (
      ~tilesize: int,
      ~next_id: ref(int),
      ~weight: float,
      multitile: array(array(array('a))),
    ) => {
  let xs = Array.length(multitile);
  let ys = Array.length(multitile[0]);
  let zs = Array.length(multitile[0][0]);

  let txs = (xs - 1) / (tilesize - 1);
  let tys = (ys - 1) / (tilesize - 1);
  let tzs = (zs - 1) / (tilesize - 1);
  if (txs * (tilesize - 1) + 1 != xs) {
    failwithf("multitile has bad X size: %d", xs, ());
  };
  if (tys * (tilesize - 1) + 1 != ys) {
    failwithf("multitile has bad Y size: %d", ys, ());
  };
  if (tzs * (tilesize - 1) + 1 != zs) {
    failwithf("multitile has bad Z size: %d", zs, ());
  };

  let cut_tile = (~tx, ~ty, ~tz) => {
    let bx = tx * (tilesize - 1);
    let by = ty * (tilesize - 1);
    let bz = tz * (tilesize - 1);
    let items =
      Array.init(tilesize, ~f=ox =>
        Array.init(tilesize, ~f=oy =>
          Array.init(tilesize, ~f=oz => multitile[bx + ox][by + oy][bz + oz])
        )
      );
    {
      id: pop_id(next_id),
      weight,
      items,
      auto_x0: tx == 0,
      auto_x1: tx == txs - 1,
      auto_y0: ty == 0,
      auto_y1: ty == tys - 1,
      auto_z0: tz == 0,
      auto_z1: tz == tzs - 1,
    };
  };

  let subtiles =
    Array.init(txs, ~f=tx =>
      Array.init(tys, ~f=ty =>
        Array.init(tzs, ~f=tz => cut_tile(~tx, ~ty, ~tz))
      )
    );
  (txs, tys, tzs, subtiles);
};

let rotate_multitile = (rotate_cw, tile) => {
  let xs = Array.length(tile);
  let ys = Array.length(tile[0]);
  let zs = Array.length(tile[0][0]);
  Array.init(zs, ~f=z =>
    Array.init(ys, ~f=y =>
      Array.init(xs, ~f=x => rotate_cw(tile[x][y][zs - 1 - z]))
    )
  );
};

let flip_x_multitile = (flip_x, tile) => {
  let xs = Array.length(tile);
  Array.mapi(
    tile,
    ~f=(i, _) => {
      let yz = tile[xs - 1 - i];
      Array.map(yz, ~f=z => Array.map(z, ~f=item => flip_x(item)));
    },
  );
};

let flip_z_multitile = (flip_z, tile) => {
  let zs = Array.length(tile[0][0]);
  Array.map(tile, ~f=yz =>
    Array.map(yz, ~f=z =>
      Array.mapi(
        z,
        ~f=(i, _) => {
          let item = z[zs - 1 - i];
          flip_z(item);
        },
      )
    )
  );
};

let rec expand_btile = (~rotate_cw, ~flip_x, ~flip_z, btile, ls) =>
  if (btile.rotate) {
    let btile = {...btile, rotate: false};
    let btile_r1 = {
      ...btile,
      items: rotate_multitile(rotate_cw, btile.items),
    };
    let btile_r2 = {
      ...btile,
      items: rotate_multitile(rotate_cw, btile_r1.items),
    };
    let btile_r3 = {
      ...btile,
      items: rotate_multitile(rotate_cw, btile_r2.items),
    };
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile, ls);
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile_r1, ls);
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile_r2, ls);
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile_r3, ls);
    ls;
  } else if (btile.flip_x) {
    let btile = {...btile, flip_x: false};
    let btile_fx = {...btile, items: flip_x_multitile(flip_x, btile.items)};
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile, ls);
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile_fx, ls);
    ls;
  } else if (btile.flip_z) {
    let btile = {...btile, flip_z: false};
    let btile_fz = {...btile, items: flip_z_multitile(flip_z, btile.items)};
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile, ls);
    let ls = expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile_fz, ls);
    ls;
  } else {
    [btile, ...ls];
  };

let twice = (f, x) => f(f(x));

let create_tileset =
    (
      ~tilesize: int,
      ~rotate_cw=Fn.id,
      ~flip_x=twice(rotate_cw),
      ~flip_z=twice(rotate_cw),
      btiles: list(btile('a)),
    ) => {
  let next_id = ref(0);
  let x_pairs_s = Hash_set.Poly.create();
  let y_pairs_s = Hash_set.Poly.create();
  let z_pairs_s = Hash_set.Poly.create();

  let tiles =
    List.map(btiles, ~f=btile => {...btile, items: xyz_of_yzx(btile.items)})
    |> List.concat_map(~f=btile =>
         expand_btile(~rotate_cw, ~flip_x, ~flip_z, btile, [])
       )
    |> List.concat_map(~f=btile => {
         let (txs, tys, tzs, subtiles) =
           split_multitile(
             btile.items,
             ~tilesize,
             ~next_id,
             ~weight=btile.weight,
           );

         for (tx in 0 to txs - 1) {
           for (ty in 0 to tys - 1) {
             for (tz in 0 to tzs - 1) {
               let here_id = subtiles[tx][ty][tz].id;
               if (tx > 0) {
                 Hash_set.add(
                   x_pairs_s,
                   (subtiles[tx - 1][ty][tz].id, here_id),
                 );
               };
               if (ty > 0) {
                 Hash_set.add(
                   y_pairs_s,
                   (subtiles[tx][ty - 1][tz].id, here_id),
                 );
               };
               if (tz > 0) {
                 Hash_set.add(
                   z_pairs_s,
                   (subtiles[tx][ty][tz - 1].id, here_id),
                 );
               };
             };
           };
         };

         Array.to_list(subtiles)
         |> List.concat_map(~f=a =>
              Array.to_list(a) |> List.concat_map(~f=Array.to_list)
            );
       });

  let tiles = Array.of_list(tiles);
  let numtiles = Array.length(tiles);

  for (ta in 0 to numtiles - 1) {
    for (tb in 0 to ta) {
      let ta_ = tiles[ta];
      let tb_ = tiles[tb];
      if (x_match(ta_, tb_)) {
        Hash_set.add(x_pairs_s, (ta_.id, tb_.id));
      };
      if (x_match(tb_, ta_)) {
        Hash_set.add(x_pairs_s, (tb_.id, ta_.id));
      };
      if (y_match(ta_, tb_)) {
        Hash_set.add(y_pairs_s, (ta_.id, tb_.id));
      };
      if (y_match(tb_, ta_)) {
        Hash_set.add(y_pairs_s, (tb_.id, ta_.id));
      };
      if (z_match(ta_, tb_)) {
        Hash_set.add(z_pairs_s, (ta_.id, tb_.id));
      };
      if (z_match(tb_, ta_)) {
        Hash_set.add(z_pairs_s, (tb_.id, ta_.id));
      };
    };
  };

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

  let x0_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (x_pairs[t1][t]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });
  let x1_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (x_pairs[t][t1]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });

  let y0_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (y_pairs[t1][t]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });
  let y1_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (y_pairs[t][t1]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });

  let z0_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (z_pairs[t1][t]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });
  let z1_requirements =
    Array.init(numtiles, ~f=t => {
      Mg_util.Range.flat_map(0, numtiles - 1, t1 =>
        if (z_pairs[t][t1]) {
          Some(t1);
        } else {
          None;
        }
      )
      |> Array.of_list
    });

  {
    tilesize,
    tiles,
    x_pairs,
    y_pairs,
    z_pairs,
    requirements:
      Array.transpose_exn([|
        x0_requirements,
        x1_requirements,
        y0_requirements,
        y1_requirements,
        z0_requirements,
        z1_requirements,
      |]),
  };
};

module Test_helpers = {
  let print_multitile = multitile => {
    let xs = Array.length(multitile);
    let ys = Array.length(multitile[0]);
    let zs = Array.length(multitile[0][0]);

    for (y in ys - 1 downto 0) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          Printf.printf("%s ", multitile[x][y][z]);
        };
        Out_channel.newline(stdout);
      };
      Out_channel.newline(stdout);
    };
  };
};

let%expect_test "single tiles" = {
  let ts =
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

let%expect_test "multi tiles" = {
  let ts =
    create_tileset(
      ~tilesize=3,
      [
        tile(
          ~weight=1.0,
          [|
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
          |],
        ),
        tile(
          ~weight=1.0,
          [|
            [|
              [|"h", "h", "i"|], /* */
              [|"d", "2", "f"|], /* */
              [|"a", "b", "b"|] /* */
            |],
            [|
              [|"h", "h", "i"|], /* */
              [|"d", "2", "f"|], /* */
              [|"a", "b", "b"|] /* */
            |],
            [|
              [|"h", "h", "i"|], /* */
              [|"d", "2", "f"|], /* */
              [|"a", "b", "b"|] /* */
            |],
          |],
        ),
        tile(
          ~weight=1.0,
          [|
            [|
              [|"b", "y", "b"|], /* */
              [|"0", "3", "0"|], /* */
              [|"h", "z", "h"|] /* */
            |],
            [|
              [|"b", "y", "b"|], /* */
              [|"0", "3", "0"|], /* */
              [|"h", "z", "h"|] /* */
            |],
            [|
              [|"b", "x", "b"|], /* */
              [|"0", "3", "0"|], /* */
              [|"h", "x", "h"|] /* */
            |],
          |],
        ),
      ],
    );

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.x_pairs),
  );
  %expect
  "
    ((false true false false) (false false false false) (false false false false)
     (false false false true))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.y_pairs),
  );
  %expect
  "
    ((true false false false) (false true false false) (false false true false)
     (false false false false))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.z_pairs),
  );
  %expect
  "
    ((false false false false) (false false true false) (true false false false)
     (false false false false))";
};

let%expect_test "flipping" = {
  let flipped_x =
    flip_x_multitile(
      String.uppercase,
      xyz_of_yzx([|
        [|
          [|"a", "b", "c"|], /* */
          [|"x", "y", "y"|], /* */
          [|"m", "n", "o"|] /* */
        |],
        [|
          [|"x", "y", "y"|], /* */
          [|"a", "b", "c"|], /* */
          [|"m", "n", "o"|] /* */
        |],
        [|
          [|"x", "y", "y"|], /* */
          [|"m", "n", "o"|], /* */
          [|"a", "b", "c"|] /* */
        |],
      |]),
    );
  Test_helpers.print_multitile(flipped_x);
  %expect
  {|
  C B A
  Y Y X
  O N M

  Y Y X
  C B A
  O N M

  Y Y X
  O N M
  C B A
  |};

  let flipped_z =
    flip_z_multitile(
      String.uppercase,
      xyz_of_yzx([|
        [|
          [|"a", "b", "c"|], /* */
          [|"x", "y", "y"|], /* */
          [|"m", "n", "o"|] /* */
        |],
        [|
          [|"x", "y", "y"|], /* */
          [|"a", "b", "c"|], /* */
          [|"m", "n", "o"|] /* */
        |],
        [|
          [|"x", "y", "y"|], /* */
          [|"m", "n", "o"|], /* */
          [|"a", "b", "c"|] /* */
        |],
      |]),
    );
  Test_helpers.print_multitile(flipped_z);
  %expect
  {|
  M N O
  X Y Y
  A B C

  M N O
  A B C
  X Y Y

  A B C
  M N O
  X Y Y
  |};
};

let%expect_test "flipping multi tiles" = {
  let ts =
    create_tileset(
      ~tilesize=3,
      [
        tile(
          ~flip_z=true,
          [|
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
            [|
              [|"a", "b", "b", "b", "c"|], /* */
              [|"d", "0", "0", "0", "f"|], /* */
              [|"g", "h", "h", "h", "i"|] /* */
            |],
          |],
        ),
      ],
    );

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.x_pairs),
  );
  %expect
  "
    ((false true false false) (false false false false) (false false false true)
     (false false false false))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.y_pairs),
  );
  %expect
  "
    ((true false false false) (false true false false) (false false true false)
     (false false false true))";

  Sexp.output_hum(
    Stdio.stdout,
    [%sexp_of: array(array(bool))](ts.z_pairs),
  );
  %expect
  "
    ((false false true false) (false false false true) (true false false false)
     (false true false false))";
};
