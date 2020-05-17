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
  ANSITerminal.printf([ANSITerminal.green], "%s", title);
  flush(stdout);
  ANSITerminal.move_bol();
  let result = f();
  ANSITerminal.erase(ANSITerminal.Eol);
  ANSITerminal.printf([], "%s\n", title);
  result;
};