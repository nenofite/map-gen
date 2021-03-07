type color = (int, int, int);

type draw_sparse('s) = ('s, (~size: int, int, int, color) => unit) => unit;
type draw_dense('s) = ('s, int, int) => option(color);

type layer_i =
  | Layer('s, draw_sparse('s), draw_dense('s)): layer_i;
type layer = ref(layer_i);

type stack = {mutable layers: list(layer)};

let default_draw_sparse = (_s, _f) => ();
let default_draw_dense = (_s, _x, _z) => None;
let empty_layer = Layer((), default_draw_sparse, default_draw_dense);

let make_layer_stack = () => {layers: []};

let push_layer = (stack: stack) => {
  let l = ref(empty_layer);
  stack.layers = [l, ...stack.layers];
  l;
};

let remove_layer = (layer: layer, stack: stack) => {
  stack.layers = List.filter(n => n !== layer, stack.layers);
};

let last_layer = (stack: stack) => Core_kernel.List.hd(stack.layers);

let remove_after_layer = (layer: layer, stack: stack) => {
  Core_kernel.(
    stack.layers =
      List.drop_while(stack.layers, ~f=l => !phys_equal(l, layer))
  );
};

let update =
    (
      ~draw_sparse=default_draw_sparse,
      ~draw_dense=default_draw_dense,
      ~state: 's,
      layer: layer,
      _stack: stack,
    ) => {
  layer := Layer(state, draw_sparse, draw_dense);
};

let draw_all_layers =
    (
      stack: stack,
      ~zoom: int,
      ~x: (int, int),
      ~z: (int, int),
      set_coord: (int, int, color) => unit,
    ) => {
  let (min_x, max_x) = x;
  let (min_z, max_z) = z;
  let draw_point = (~size, x, z, c) => {
    let minx = x - size / 2;
    let minz = z - size / 2;
    for (z in minz to minz + size - 1) {
      for (x in minx to minx + size - 1) {
        if (min_x <= x && x <= max_x && min_z <= z && z <= max_z) {
          set_coord(x, z, c);
        };
      };
    };
  };
  let draw_one = ({contents: Layer(state, draw_sparse, draw_dense)}) => {
    let zsteps = (max_z - min_z) / zoom;
    let xsteps = (max_x - min_x) / zoom;
    for (z in 0 to zsteps) {
      let z = z * zoom + min_z;
      for (x in 0 to xsteps) {
        let x = x * zoom + min_x;
        switch (draw_dense(state, x, z)) {
        | Some(c) => set_coord(x, z, c)
        | None => ()
        };
      };
    };
    draw_sparse(state, draw_point);
  };
  List.iter(draw_one, List.rev(stack.layers));
  ();
};

let%expect_test "draw two layers" = {
  let stack = make_layer_stack();
  let a = push_layer(stack);
  let a_dense = (i, x, z) => Some((i, x, z));
  let a_sparse = (i, f) => f(~size=1, 3, 0, (i + 22, 3, 0));
  update(~draw_dense=a_dense, ~draw_sparse=a_sparse, ~state=0, a, stack);
  let b = push_layer(stack);
  update(
    ~draw_sparse=
      (i, f) => {
        f(~size=1, 0, 0, (i, 0, 0));
        f(~size=1, 0, 1, (i, 1, 0));
      },
    ~state=10,
    b,
    stack,
  );
  update(~state=1, ~draw_dense=a_dense, ~draw_sparse=a_sparse, a, stack);

  let set_coord = (x, z, (r, g, b)) =>
    Printf.printf("set %d,%d to %d,%d,%d\n", x, z, r, g, b);
  draw_all_layers(stack, ~zoom=1, ~x=(0, 3), ~z=(0, 0), set_coord);

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
