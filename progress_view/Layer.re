type draw_sparse = ((~size: int, ~color: int, int, int) => unit) => unit;
type draw_dense = (int, int) => option(int);

type layer_i = {
  draw_sparse,
  draw_dense,
};
type layer = ref(layer_i);

type stack = {mutable layers: list(layer)};

let default_draw_sparse = _f => ();
let default_draw_dense = (_x, _z) => None;
let empty_layer = {
  draw_sparse: default_draw_sparse,
  draw_dense: default_draw_dense,
};

let make_layer_stack = () => {layers: []};

let fake_layer = () => ref(empty_layer);

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
      layer: layer,
      _stack: stack,
    ) => {
  layer := {draw_dense, draw_sparse};
};

let draw_all_layers =
    (
      stack: stack,
      ~zoom: int,
      ~x: (int, int),
      ~z: (int, int),
      set_coord: (int, int, ~color: int) => unit,
    ) => {
  let (min_x, max_x) = x;
  let (min_z, max_z) = z;
  let draw_point = (~size, ~color, x, z) => {
    let minx = x - size / 2;
    let minz = z - size / 2;
    for (z in minz to minz + size - 1) {
      for (x in minx to minx + size - 1) {
        if (min_x <= x && x <= max_x && min_z <= z && z <= max_z) {
          set_coord(x, z, ~color);
        };
      };
    };
  };
  let draw_one = ({contents: {draw_sparse, draw_dense}}) => {
    let zsteps = (max_z - min_z) / zoom;
    let xsteps = (max_x - min_x) / zoom;
    for (z in 0 to zsteps) {
      let z = z * zoom + min_z;
      for (x in 0 to xsteps) {
        let x = x * zoom + min_x;
        switch (draw_dense(x, z)) {
        | Some(color) => set_coord(x, z, ~color)
        | None => ()
        };
      };
    };
    draw_sparse(draw_point);
  };
  List.iter(draw_one, List.rev(stack.layers));
  ();
};

let%expect_test "draw two layers" = {
  let rgb = Mg_util.Color.unsplit_rgb;
  let stack = make_layer_stack();
  let a = push_layer(stack);
  let a_dense = (i, x, z) => Some(rgb(i, x, z));
  let a_sparse = (i, f) => f(~size=1, ~color=rgb(i + 22, 3, 0), 3, 0);
  update(~draw_dense=a_dense(0), ~draw_sparse=a_sparse(0), a, stack);
  let b = push_layer(stack);
  update(
    ~draw_sparse=
      f => {
        f(~size=1, 0, 0, ~color=rgb(10, 0, 0));
        f(~size=1, 0, 1, ~color=rgb(10, 1, 0));
      },
    b,
    stack,
  );
  update(~draw_dense=a_dense(1), ~draw_sparse=a_sparse(1), a, stack);

  let set_coord = (x, z, ~color as rgb) =>
    Printf.printf("set %d,%d to 0x%06x\n", x, z, rgb);
  draw_all_layers(stack, ~zoom=1, ~x=(0, 3), ~z=(0, 0), set_coord);

  %expect
  {|
  set 0,0 to 0x010000
  set 1,0 to 0x010100
  set 2,0 to 0x010200
  set 3,0 to 0x010300
  set 3,0 to 0x170300
  set 0,0 to 0x0a0000
  |};
};
