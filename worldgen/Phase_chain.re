type t(_, _) =
  | Empty: t(Grid.t('a), Grid.t('a))
  | Phase(string, 'a => Grid.t('b), t(Grid.t('b), Grid.t('c)))
    : t('a, Grid.t('c));

let rec concat:
  type a b c.
    (t(a, Grid.t(b)), t(Grid.t(b), Grid.t(c))) => t(a, Grid.t(c)) =
  (first, second) =>
    switch (first) {
    | Empty => second
    | Phase(name, f, rest) => Phase(name, f, concat(rest, second))
    };

let (@>) = concat;

let phase = (name, f) => Phase(name, f, Empty);

let rec phase_repeat' = (times, name, f, chain) =>
  if (times > 0) {
    let chain = Phase(name, f, chain);
    phase_repeat'(times - 1, name, f, chain);
  } else {
    chain;
  };
let phase_repeat = (times, name, f) => phase_repeat'(times, name, f, Empty);

let rec length': type a b. (int, t(a, b)) => int =
  (acc, chain) =>
    switch (chain) {
    | Empty => acc
    | Phase(_name, _f, rest) => length'(acc + 1, rest)
    };
let length = length'(0, _);

let rec run_all':
  type a b. (~completed: int, ~total: int, ~grid: a, ~chain: t(a, b)) => b =
  (~completed, ~total, ~grid, ~chain) =>
    switch (chain) {
    | Empty => grid
    | Phase(name, f, rest) =>
      let completed = completed + 1;
      let grid =
        Mg_util.print_progress(
          Printf.sprintf("[%d of %d] %s", completed, total, name), () =>
          f(grid)
        );
      ANSITerminal.printf(
        [],
        "[%d of %d] [%d x %d] %s\n",
        completed,
        total,
        grid.side,
        grid.side,
        name,
      );
      run_all'(~completed, ~total, ~grid, ~chain=rest);
    };

let run_all = chain => {
  let total = length(chain);
  run_all'(~completed=0, ~total, ~grid=(), ~chain);
};