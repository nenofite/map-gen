type color = (int, int, int);

type draw_sparse('s) = 's => list((int, int, color));
type draw_dense('s) = ('s, int, int) => option(color);

type layer('s) = {
  mutable state: 's,
  draw_sparse: draw_sparse('s),
  draw_dense: draw_dense('s),
};

type layer_gadt =
  | Layer(layer('s)): layer_gadt;

type stack = {mutable layers: list(layer_gadt)};

let default_draw_sparse = _s => [];
let default_draw_dense = (_s, _x, _z) => None;

let make_layer_stack = () => {layers: []};

let push_layer =
    (
      ~state,
      ~draw_sparse=default_draw_sparse,
      ~draw_dense=default_draw_dense,
      stack: stack,
    ) => {
  let l = {state, draw_sparse, draw_dense};
  stack.layers = [Layer(l), ...stack.layers];
  l;
};

let update = (~state: 's, layer: layer('s), _stack: stack) => {
  layer.state = state;
};

let draw_all_layers =
    (
      ~x: (int, int),
      ~z: (int, int),
      set_coord: (int, int, color) => unit,
      stack: stack,
    ) => {
  let (min_x, max_x) = x;
  let (min_z, max_z) = z;
  let draw_one = (Layer(l)) => {
    for (z in min_z to max_z) {
      for (x in min_x to max_x) {
        switch (l.draw_dense(l.state, x, z)) {
        | Some(c) => set_coord(x, z, c)
        | None => ()
        };
      };
    };
    List.iter(
      ((x, z, c)) =>
        if (min_x <= x && x <= max_x && min_z <= z && z <= max_z) {
          set_coord(x, z, c);
        },
      l.draw_sparse(l.state),
    );
  };
  List.iter(draw_one, List.rev(stack.layers));
  ();
};

let%expect_test "draw two layers" = {
  let stack = make_layer_stack();
  let a =
    push_layer(
      ~state=0,
      ~draw_dense=(i, x, z) => Some((i, x, z)),
      ~draw_sparse=i => [(3, 0, (i + 22, 3, 0))],
      stack,
    );
  let _b =
    push_layer(
      ~state=10,
      ~draw_sparse=i => [(0, 0, (i, 0, 0)), (0, 1, (i, 1, 0))],
      stack,
    );
  update(~state=1, a, stack);

  let set_coord = (x, z, (r, g, b)) =>
    Printf.printf("set %d,%d to %d,%d,%d\n", x, z, r, g, b);
  draw_all_layers(~x=(0, 3), ~z=(0, 0), set_coord, stack);

  %expect
  {|
  set 0,0 to 1,0,0
  set 1,0 to 1,1,0
  set 2,0 to 1,2,0
  set 3,0 to 1,3,0
  set 3,0 to 23,3,0
  set 0,0 to 10,0,0
  |};
};