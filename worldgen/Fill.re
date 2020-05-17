type fill('a) = ('a, 'a, 'a, 'a) => 'a;

type opt_fill('a) = (~next: fill('a)) => fill('a);

/** Chain an opt_fill so that it falls through to the next fill */
let ( **> ) = (fill: opt_fill('a), next_fill: fill('a)) =>
  fill(~next=next_fill);

/** random picks a random neighbor and uses its value */
let random = (a, b, c, d) => {
  switch (Random.int(4)) {
  | 0 => a
  | 1 => b
  | 2 => c
  | _ => d
  };
};

/** random_avg picks a random int within the range of neighbors */
let random_avg = (a, b, c, d) => {
  let lowest = min(min(a, b), min(c, d));
  let highest = max(max(a, b), max(c, d));
  lowest + Random.int(highest - lowest + 1);
};

/** line will continue a "line" if two opposing sides are equal */
let line = (~eq=(==), ~subfill=(a, _) => a, (), ~next, a, b, c, d) => {
  switch (eq(a, c), eq(b, d)) {
  | (true, true) =>
    if (Random.bool()) {
      subfill(a, c);
    } else {
      subfill(b, d);
    }
  | (true, false) => subfill(a, c)
  | (false, true) => subfill(b, d)
  | (false, false) => next(a, b, c, d)
  };
};

/** diag will fill adjacent sides, similar to line_fill */
let diag = (~eq=(==), ~subfill, ~next, a, b, c, d) => {
  let sides =
    [(a, b), (b, c), (c, d), (d, a)]
    |> List.filter(((n, m)) => eq(n, m), _)
    |> Util.shuffle;
  switch (sides) {
  | [(n, m), ..._] => subfill(n, m)
  | [] => next(a, b, c, d)
  };
};

/** chance applies fill [percent] / 100 of the time, otherwise falls through */
let chance = (~percent, ~fill, ~next, a, b, c, d) =>
  if (Random.int(100) < percent) {
    fill(a, b, c, d);
  } else {
    next(a, b, c, d);
  };