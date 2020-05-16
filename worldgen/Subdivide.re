let subdivide_with_fill = (old_grid: Grid.t('a), fill): Grid.t('a) => {
  open Grid;

  /*
     old_grid:
     1-2-3-
     ------
     4-5-6-
     ------
     7-8-9-
     ------

     diamonds:
     =-=-=-
     -1-2-3
     =-=-=-
     -4-5-6
     =-=-=-
     -7-8-9

     offset_squares:
     =1=2=3
     -=-=-=
     =4=5=6
     -=-=-=
     =7=8=9
     -=-=-=

     center_squares:
     ======
     1=2=3=
     ======
     4=5=6=
     ======
     7=8=9=
   */

  let diamonds =
    init(
      old_grid.side,
      (x, y) => {
        let nw = at_w(old_grid, x, y);
        let ne = at_w(old_grid, x + 1, y);
        let se = at_w(old_grid, x + 1, y + 1);
        let sw = at_w(old_grid, x, y + 1);
        fill(nw, ne, se, sw);
      },
    );
  let offset_squares =
    init(
      old_grid.side,
      (x, y) => {
        let n = at_w(diamonds, x, y - 1);
        let e = at_w(old_grid, x + 1, y);
        let s = at_w(diamonds, x, y);
        let w = at_w(old_grid, x, y);
        fill(n, e, s, w);
      },
    );
  let center_squares =
    init(
      old_grid.side,
      (x, y) => {
        let n = at_w(old_grid, x, y);
        let e = at_w(diamonds, x, y);
        let s = at_w(old_grid, x, y + 1);
        let w = at_w(diamonds, x - 1, y);
        fill(n, e, s, w);
      },
    );
  let combined =
    init(
      old_grid.side * 2,
      (x, y) => {
        let x' = x / 2;
        let y' = y / 2;
        switch (x mod 2 == 0, y mod 2 == 0) {
        | (true, true) => at(old_grid, x', y')
        | (false, true) => at(center_squares, x', y')
        | (false, false) => at(diamonds, x', y')
        | (true, false) => at(offset_squares, x', y')
        };
      },
    );
  combined;
};

let overwrite_subdivide_with_fill = (old_grid: Grid.t('a), fill) => {
  let grid = subdivide_with_fill(old_grid, fill);
  Grid.map(grid, (x, y, here) =>
    if (x mod 2 == 0 && y mod 2 == 0) {
      let n = Grid.at_w(grid, x, y - 1);
      let e = Grid.at_w(grid, x + 1, y);
      let s = Grid.at_w(grid, x, y + 1);
      let w = Grid.at_w(grid, x - 1, y);
      fill(n, e, s, w);
    } else {
      here;
    }
  );
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