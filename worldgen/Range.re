let rec exists = (min, max, f) =>
  if (min > max) {
    false;
  } else if (f(min)) {
    true;
  } else {
    exists(min + 1, max, f);
  };

let for_all = (min, max, f) => {
  let not_f = n => !f(n);
  !exists(min, max, not_f);
};

let rec fold = (min, max, acc, f) =>
  if (min <= max) {
    let acc = f(acc, min);
    fold(min + 1, max, acc, f);
  } else {
    acc;
  };

let map = (min, max, f) => fold(min, max, [], (ls, n) => [f(n), ...ls]);