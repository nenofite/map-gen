exception Not_init;

type state = {
  window: Window.t,
  stack: Layer.stack,
  mutable center_x: int,
  mutable center_z: int,
  mutable title: string,
};

let global_state = ref(None);

let init = () =>
  if (Option.is_none(global_state^)) {
    global_state :=
      Some({
        window: Window.make_window(),
        stack: Layer.make_layer_stack(),
        center_x: 200,
        center_z: 200,
        title: "the world",
      });
  };

let unwrap_state = () => {
  switch (global_state^) {
  | Some(state) => state
  | None => raise(Not_init)
  };
};

let close = () => {
  let s = unwrap_state();
  Window.close_window(s.window);
  global_state := None;
};

let update_window = () => {
  let s = unwrap_state();
  Window.update(
    ~center_x=s.center_x,
    ~center_z=s.center_z,
    s.title,
    Layer.draw_all_layers(s.stack),
    s.window,
  );
};

let apply_optionals = (center, title) => {
  let s = unwrap_state();
  switch (center) {
  | Some((x, z)) =>
    s.center_x = x;
    s.center_z = z;
  | None => ()
  };
  switch (title) {
  | Some(title) => s.title = title
  | None => ()
  };
};

let push_layer = () => {
  let s = unwrap_state();
  let l = Layer.push_layer(s.stack);
  l;
};

let update =
    (~center=?, ~title=?, ~draw_sparse=?, ~draw_dense=?, ~state, layer) => {
  let s = unwrap_state();
  apply_optionals(center, title);
  Layer.update(~state, ~draw_dense?, ~draw_sparse?, layer, s.stack);
  update_window();
};