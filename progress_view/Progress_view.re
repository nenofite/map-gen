exception Not_init;

type state = {
  window: Window.t,
  stack: Layer.stack,
  mutable center_x: int,
  mutable center_z: int,
  /**
   * zoom is how many tiles of map to draw in the space of a single tile on
   * the screen; the higher it is, the more zoomed out the map is.
   */
  mutable zoom: int,
  mutable title: string,
};

type layer = Layer.layer;

let global_state = ref(None);

let init = () =>
  if (Option.is_none(global_state^)) {
    global_state :=
      Some({
        window: Window.make_window(),
        stack: Layer.make_layer_stack(),
        center_x: 0,
        center_z: 0,
        zoom: 1,
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
    ~zoom=s.zoom,
    ~center_x=s.center_x,
    ~center_z=s.center_z,
    s.title,
    Layer.draw_all_layers(s.stack),
    s.window,
  );
};

/**
 * fits the given box into the window, zooming in as much as possible while
 * still fitting the whole box
 */
let apply_fit = (s, ~minx, ~maxx, ~minz, ~maxz) => {
  let center_x = (maxx + minx) / 2;
  let center_z = (maxz + minz) / 2;
  let width = maxx - minx;
  let height = maxz - minz;
  let zoom_x = (width - 1) / s.window.width + 1;
  let zoom_z = (height - 1) / s.window.height + 1;
  s.zoom = max(zoom_x, zoom_z);
  s.center_x = center_x;
  s.center_z = center_z;
};

let apply_optionals = (zoom, center, fit, title) => {
  let s = unwrap_state();
  switch (zoom) {
  | Some(z) => s.zoom = z
  | None => ()
  };
  switch (center) {
  | Some((x, z)) =>
    s.center_x = x;
    s.center_z = z;
  | None => ()
  };
  switch (fit) {
  | Some((minx, maxx, minz, maxz)) =>
    apply_fit(s, ~minx, ~maxx, ~minz, ~maxz)
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

let remove_layer = layer => {
  let s = unwrap_state();
  Layer.remove_layer(layer, s.stack);
};

let last_layer = () => {
  let s = unwrap_state();
  Layer.last_layer(s.stack);
};

let remove_after_layer = layer => {
  let s = unwrap_state();
  Layer.remove_after_layer(layer, s.stack);
};

let update =
    (
      ~zoom=?,
      ~center=?,
      ~fit=?,
      ~title=?,
      ~draw_sparse=?,
      ~draw_dense=?,
      ~state,
      layer,
    ) => {
  apply_optionals(zoom, center, fit, title);
  let s = unwrap_state();
  Layer.update(~state, ~draw_dense?, ~draw_sparse?, layer, s.stack);
  update_window();
};

let center = (~zoom=?, ~title=?, center) => {
  apply_optionals(zoom, Some(center), None, title);
  update_window();
};

let fit = (~title=?, fit) => {
  apply_optionals(None, None, Some(fit), title);
  update_window();
};

let save = (~side, ~img_side=1024, ~format=Images.Png, file) => {
  open Images;
  open OImages;
  let s = unwrap_state();
  let file = Config.Paths.drawing(file ++ "." ++ Images.extension(format));

  let zoom = max(1, side / img_side);
  let img_side = side / zoom;
  let img = (new rgb24)(img_side, img_side);
  Layer.draw_all_layers(
    s.stack,
    ~zoom,
    ~x=(0, side - 1),
    ~z=(0, side - 1),
    (x, z, color) => {
      let (r, g, b) = color;
      img#set(x / zoom, z / zoom, {r, g, b});
    },
  );
  img#save(file, Some(format), []);
  img#destroy;
  ();
};
