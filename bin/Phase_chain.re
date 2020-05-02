type updater = (~completed: int, ~total: int, ~grid_side: int) => unit;

type t('a, 'b) = {
  length: int,
  next: (~completed: int, ~total: int, ~updater: updater, 'a) => Grid.t('b),
};

let empty_chain = {length: 0, next: (~completed, ~total, ~updater, x) => x};

let start_chain = (f: unit => Grid.t('a)): t(unit, 'a) => {
  length: 1,
  next: (~completed, ~total, ~updater, _) => {
    let grid = f();
    let completed = completed + 1;
    updater(~completed, ~total, ~grid_side=grid.width);
    chain.next(~completed, ~total, ~updater, grid);
  },
};

let prepend = (f: 'a => Grid.t('b), chain) => {
  length: chain.length + 1,
  next: (~completed, ~total, ~updater, grid) => {
    let grid = f(grid);
    let completed = completed + 1;
    updater(~completed, ~total, ~grid_side=grid.width);
    chain.next(~completed, ~total, ~updater, grid);
  },
};

let run_all = (updater, chain, grid) => {
  chain.next(~completed=0, ~total=chain.length, ~updater, grid);
};

let updater: updater =
  (~completed, ~total, ~grid_side) => {
    Printf.printf(
      "Finished %d of %d\t(%d x %d)\n",
      completed,
      total,
      grid_side,
      grid_side,
    );
  };