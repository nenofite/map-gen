let print = (grid: Grid.t('a), print_symbol: 'a => unit) => {
  for (y in 0 to grid.height - 1) {
    for (x in 0 to grid.width - 1) {
      let v = Grid.at(grid, x, y);
      print_symbol(v);
      print_string(" ");
    };
    print_newline();
  };
};

let print' = (grid: Grid.t('a), symbol: 'a => string) => {
  print(grid, s => print_string(symbol(s)));
};