let wrapCoord = (width, height, x, y) => {
  let x' =
    switch (x mod width) {
    | x when x < 0 => x + width
    | x => x
    };
  let y' =
    switch (y mod height) {
    | y when y < 0 => y + height
    | y => y
    };
  (x', y');
};

let subdivide = (grid: Grid.t('a), f: ('a, 'a, 'a, 'a) => 'a): Grid.t('a) => {
  open Grid;
  let {width: oldWidth, height: oldHeight} = grid;
  let width = oldWidth * 2;
  let height = oldHeight * 2;

  /* Copy into larger grid */
  let empty = Obj.magic(0);
  let grid' =
    init(width, height, (x, y) =>
      if (x mod 2 == 0 && y mod 2 == 0) {
        at'(grid, x / 2, y / 2);
      } else {
        empty;
      }
    );

  /* Diamonds */
  for (y in 0 to oldHeight - 1) {
    let y' = y * 2 + 1;
    for (x in 0 to oldWidth - 1) {
      let x' = x * 2 + 1;

      let nw = {
        let (nx, ny) = wrapCoord(width, height, x' - 1, y' - 1);
        at(grid', nx, ny);
      };
      let ne = {
        let (nx, ny) = wrapCoord(width, height, x' + 1, y' - 1);
        at(grid', nx, ny);
      };
      let se = {
        let (nx, ny) = wrapCoord(width, height, x' + 1, y' + 1);
        at(grid', nx, ny);
      };
      let sw = {
        let (nx, ny) = wrapCoord(width, height, x' - 1, y' + 1);
        at(grid', nx, ny);
      };

      let v = f(nw, ne, se, sw);
      put(grid', x', y', v);
    };
  };

  /* Squares */
  for (y' in 0 to height - 1) {
    let xOffset =
      if (y' mod 2 == 0) {
        1;
      } else {
        0;
      };
    for (x in 0 to oldWidth - 1) {
      let x' = x * 2 + xOffset;

      let n = {
        let (nx, ny) = wrapCoord(width, height, x', y' - 1);
        at(grid', nx, ny);
      };
      let e = {
        let (nx, ny) = wrapCoord(width, height, x' + 1, y');
        at(grid', nx, ny);
      };
      let s = {
        let (nx, ny) = wrapCoord(width, height, x', y' + 1);
        at(grid', nx, ny);
      };
      let w = {
        let (nx, ny) = wrapCoord(width, height, x' - 1, y');
        at(grid', nx, ny);
      };

      let v = f(n, e, s, w);
      put(grid', x', y', v);
    };
  };

  grid';
};