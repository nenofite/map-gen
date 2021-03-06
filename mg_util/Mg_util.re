module Floats = {
  let (~.) = float_of_int;
  let (~~) = int_of_float;
};

let time_ms = () => Int64.of_float(Unix.gettimeofday() *. 1000.);

/** times repeats f for iters iterations */
let rec times = (f, iters, input) => {
  switch (iters) {
  | 0 => input
  | _ => times(f, iters - 1, f(input))
  };
};

let shuffle = list => {
  List.(
    map(x => (Random.bits(), x), list)
    |> fast_sort(((a, _), (b, _)) => Int.compare(a, b), _)
    |> map(((_, x)) => x, _)
  );
};

let rec take = (amount, list) =>
  switch (list) {
  | [a, ...b] when amount > 0 => [a, ...take(amount - 1, b)]
  | [_, ..._]
  | [] => []
  };

let rec take_both = (amount, list, result) =>
  switch (list) {
  | [a, ...b] when amount > 0 => take_both(amount - 1, b, [a, ...result])
  | [_, ..._]
  | [] => (List.rev(result), list)
  };
let take_both = (amount, list) => take_both(amount, list, []);

let rec drop = (amount, list) =>
  switch (list) {
  | [] => []
  | list when amount <= 0 => list
  | [_, ...rest] => drop(amount - 1, rest)
  };

let print_progress = (title: string, f: unit => 'a): 'a => {
  Tale.block(title, ~f);
};

/** mkdir creates all directories in path, using the shell command [mkdir -p <path>] */
let mkdir = (path: string): unit =>
  if (Sys.os_type == "Win32") {
    Unix.system("mkdir " ++ path) |> ignore;
  } else {
    Unix.system("mkdir -p " ++ path) |> ignore;
  };

let read_file = (path, f) => {
  let fin = open_in(path);
  let result = f(fin);
  close_in(fin);
  result;
};

let distance = ((ax, ay), (bx, by)): float => {
  sqrt((ax -. bx) ** 2. +. (ay -. by) ** 2.);
};

let distance_int = ((ax, ay), (bx, by)): int => {
  Floats.(~~distance((~.ax, ~.ay), (~.bx, ~.by)));
};

/** output_int32_be writes an int without allocating any buffers */
let output_int32_be = (f: out_channel, i: int32): unit => {
  output_byte(f, Int32.(shift_right(i, 24) |> logand(_, 0xFFl) |> to_int));
  output_byte(f, Int32.(shift_right(i, 16) |> logand(_, 0xFFl) |> to_int));
  output_byte(f, Int32.(shift_right(i, 8) |> logand(_, 0xFFl) |> to_int));
  output_byte(f, Int32.(logand(i, 0xFFl) |> to_int));
};

/** output_int64_be writes an int without allocating any buffers */
let output_int64_be = (f: out_channel, i: int64): unit => {
  output_byte(f, Int64.(shift_right(i, 56) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 48) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 40) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 32) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 24) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 16) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 8) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(logand(i, 0xFFL) |> to_int));
};

let write_file = (path, f) => {
  Tale.blockf(
    "Writing file %s",
    path,
    ~f=() => {
      let file = open_out_bin(path);
      f(file);
      close_out(file);
    },
  );
};

module Range = {
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

  let map = (min, max, f) => fold(min, max, [], (ls, n) => [f(n), ...ls]);
};

/** is the same as Random.int, but handles zero nicely */
let random = var =>
  if (var > 0) {
    Random.int(var);
  } else {
    0;
  };