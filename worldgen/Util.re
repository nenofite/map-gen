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

let print_progress = (title: string, f: unit => 'a): 'a => {
  ANSITerminal.printf([ANSITerminal.blue], "⌜ %s ⌝\n", title);
  flush(stdout);
  let result = f();
  ANSITerminal.printf([ANSITerminal.green], "⌞ %s ⌟\n", title);
  flush(stdout);
  result;
};

/** mkdir creates all directories in path, using the shell command [mkdir -p <path>] */
let mkdir = (path: string): unit => {
  Unix.system("mkdir -p " ++ path) |> ignore;
};

let read_file = (path, f) => {
  let fin = open_in(path);
  let result = f(fin);
  close_in(fin);
  result;
};