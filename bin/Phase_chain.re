type updater = (~completed: int, ~total: int, ~grid_side: int) => unit;

type t(_, _) =
  | Empty: t(Grid.t('a), Grid.t('a))
  | Phase('a => Grid.t('b), t(Grid.t('b), Grid.t('c)))
    : t('a, Grid.t('c));

let prepend = (f, chain) => Phase(f, chain);
let singular = f => Phase(f, Empty);

let rec concat:
  type a b c.
    (t(a, Grid.t(b)), t(Grid.t(b), Grid.t(c))) => t(a, Grid.t(c)) =
  (first, second) =>
    switch (first) {
    | Empty => second
    | Phase(f, rest) => Phase(f, concat(rest, second))
    };

let (@>) = prepend;
let (@@>) = concat;
let finish = Empty;

let rec repeat = (times, f) =>
  if (times > 0) {
    f @> repeat(times - 1, f);
  } else {
    singular(f);
  };

let rec length': type a b. (int, t(a, b)) => int =
  (acc, chain) =>
    switch (chain) {
    | Empty => acc
    | Phase(_f, rest) => length'(acc + 1, rest)
    };

let length = length'(0, _);

let rec run_all':
  type a b.
    (
      ~updater: updater,
      ~completed: int,
      ~total: int,
      ~grid: a,
      ~chain: t(a, b)
    ) =>
    b =
  (~updater, ~completed, ~total, ~grid, ~chain) =>
    switch (chain) {
    | Empty => grid
    | Phase(f, rest) =>
      let grid = f(grid);
      let completed = completed + 1;
      updater(~completed, ~total, ~grid_side=grid.width);
      run_all'(~updater, ~completed, ~total, ~grid, ~chain=rest);
    };

let default_updater: updater =
  (~completed, ~total, ~grid_side) => {
    Printf.printf(
      "Finished %d of %d phases\t(%d x %d)\n",
      completed,
      total,
      grid_side,
      grid_side,
    );
    flush(stdout);
  };

let run_all = chain => {
  let total = length(chain);
  run_all'(~updater=default_updater, ~completed=0, ~total, ~grid=(), ~chain);
};