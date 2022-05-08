open! Core;

let parse_3d = (~has_spaces=true, s) => {
  let append_if_not_empty = (l, ls) =>
    switch (l) {
    | [] => ls
    | _ => [l, ...ls]
    };

  let rec split_layers = (cur_layer, layers, lines) =>
    switch (lines) {
    | [] => append_if_not_empty(List.rev(cur_layer), layers) |> List.rev
    | [[""] | [], ...lines] =>
      let layers = append_if_not_empty(List.rev(cur_layer), layers);
      split_layers([], layers, lines);
    | [line, ...lines] => split_layers([line, ...cur_layer], layers, lines)
    };

  let split_chars =
    has_spaces
      ? String.split(~on=' ')
      : (s => String.to_list(s) |> List.map(~f=String.of_char));

  let lines =
    Caml.String.trim(s)
    |> String.split_on_chars(~on=['\n'])
    |> List.map(~f=ln => Caml.String.trim(ln) |> split_chars);
  split_layers([], [], lines);
};

let parse_2d = (~has_spaces=?, s) => {
  switch (parse_3d(~has_spaces?, s)) {
  | [] => []
  | [layer] => layer
  | _ => failwith("parse_2d expects a single layer")
  };
};