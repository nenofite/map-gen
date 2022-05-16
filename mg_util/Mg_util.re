module Floats = {
  let (~.) = float_of_int;
  let (~~) = int_of_float;
};

module Geometry = Geometry;
module Parse_grid = Parse_grid;
module Perlin = Perlin;

module Color = {
  let split_rgb = color => {
    let r = (color land 0xFF0000) lsr 16;
    let g = (color land 0x00FF00) lsr 8;
    let b = color land 0x0000FF;
    (r, g, b);
  };

  let unsplit_rgb = (r, g, b) => {
    (r land 0xFF) lsl 16 lor (g land 0xFF) lsl 8 lor (b land 0xFF);
  };

  let blend = (a, b, fraction) => {
    open Floats;

    let (ar, ag, ab) = split_rgb(a);
    let (br, bg, bb) = split_rgb(b);
    let fraction = Float.min(1., Float.max(0., fraction));

    let r = ~~(~.ar *. (1. -. fraction) +. ~.br *. fraction);
    let g = ~~(~.ag *. (1. -. fraction) +. ~.bg *. fraction);
    let b = ~~(~.ab *. (1. -. fraction) +. ~.bb *. fraction);
    unsplit_rgb(r, g, b);
  };

  let blend_split = (a, b, fraction) => {
    open Floats;

    let (ar, ag, ab) = a;
    let (br, bg, bb) = b;
    let fraction = min(1., max(0., fraction));

    let r = ~~(~.ar *. (1. -. fraction) +. ~.br *. fraction);
    let g = ~~(~.ag *. (1. -. fraction) +. ~.bg *. fraction);
    let b = ~~(~.ab *. (1. -. fraction) +. ~.bb *. fraction);
    (r, g, b);
  };
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
    let cmd = Filename.quote_command("mkdir", [path]);
    Unix.system(cmd) |> ignore;
  } else {
    let cmd = Filename.quote_command("mkdir", ["-p", path]);
    Unix.system(cmd) |> ignore;
  };

let read_file = (path, f) => {
  let fin = open_in(path);
  let result = f(fin);
  close_in(fin);
  result;
};

let copy_file = (src: string, ~dest: string): unit =>
  if (Sys.os_type == "Win32") {
    let cmd = Filename.quote_command("copy", ["/y", src, dest]);
    Unix.system(cmd) |> ignore;
  } else {
    let cmd = Filename.quote_command("cp", ["-f", src, dest]);
    Unix.system(cmd) |> ignore;
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

  let flat_map = (min, max, f) =>
    fold(min, max, [], (ls, n) =>
      switch (f(n)) {
      | Some(x) => [x, ...ls]
      | None => ls
      }
    );

  let rec iter_m = (min, max, ~bind, expr) => {
    let s = expr(min);
    if (min < max) {
      bind(s, ~f=() => iter_m(min + 1, max, ~bind, expr));
    } else {
      s;
    };
  };
};

/** is the same as Random.int, but handles zero nicely */
let random = var =>
  if (var > 0) {
    Random.int(var);
  } else {
    0;
  };