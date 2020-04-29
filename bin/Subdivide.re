let subdivide_with_fill =
    (oldGrid: Grid.t('a), f: ('a, 'a, 'a, 'a) => 'a): Grid.t('a) => {
  open Grid;
  let {width: oldWidth, height: oldHeight} = oldGrid;
  let width = oldWidth * 2;
  let height = oldHeight * 2;

  /* Copy into larger grid */
  let empty = Obj.magic(0);
  let grid =
    init(width, height, (x, y) =>
      if (x mod 2 == 0 && y mod 2 == 0) {
        at(oldGrid, x / 2, y / 2);
      } else {
        empty;
      }
    );

  /* Diamonds */
  for (oldY in 0 to oldHeight - 1) {
    let y = oldY * 2 + 1;
    for (oldX in 0 to oldWidth - 1) {
      let x = oldX * 2 + 1;

      let nw = at'(grid, x - 1, y - 1);
      let ne = at'(grid, x + 1, y - 1);
      let se = at'(grid, x + 1, y + 1);
      let sw = at'(grid, x - 1, y + 1);

      let v = f(nw, ne, se, sw);
      put(grid, x, y, v);
    };
  };

  /* Squares */
  for (y in 0 to height - 1) {
    let xOffset =
      if (y mod 2 == 0) {
        1;
      } else {
        0;
      };
    for (oldX in 0 to oldWidth - 1) {
      let x = oldX * 2 + xOffset;

      let n = at'(grid, x, y - 1);
      let e = at'(grid, x + 1, y);
      let s = at'(grid, x, y + 1);
      let w = at'(grid, x - 1, y);

      let v = f(n, e, s, w);
      put(grid, x, y, v);
    };
  };

  grid;
};

/** random_fill picks a random neighbor and uses its value */
let random_fill = (a, b, c, d) => {
  switch (Random.int(4)) {
  | 0 => a
  | 1 => b
  | 2 => c
  | _ => d
  };
};

/** subdivide uses [random_fill] */
let subdivide = subdivide_with_fill(_, random_fill);