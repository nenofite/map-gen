let print = (grid: Grid.t('a), symbol: 'a => string) => {
  open Grid;

  let {width, height} = grid;

  for (y in 0 to height - 1) {
    for (x in 0 to width - 1) {
      let v = at(grid, x, y);
      print_string(symbol(v));
      print_string(" ");
    };
    print_newline();
  };
};