let rec exists = (min, max, f) =>
  if (min > max) {
    false;
  } else if (f(min)) {
    true;
  } else {
    exists(min + 1, max, f);
  };

let rec fold = (min, max, acc, f) =>
  if (min <= max) {
    let acc = f(acc, min);
    fold(min + 1, max, acc, f);
  } else {
    acc;
  };