open! Core;

exception Not_init;

type state = {
  window: option(Window.t),
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

type usage_state =
  | Uninit
  | Ignore
  | Active(state);
let global_state = ref(Uninit);

type layer = Layer.layer;

let init = (~make_window as w: bool) =>
  switch (global_state^) {
  | Uninit =>
    global_state :=
      Active({
        window:
          if (w) {
            Some(Window.make_window());
          } else {
            None;
          },
        stack: Layer.make_layer_stack(),
        center_x: 0,
        center_z: 0,
        zoom: 1,
        title: "the world",
      })
  | Ignore
  | Active(_) => ()
  };

let init_ignore = () =>
  switch (global_state^) {
  | Uninit => global_state := Ignore
  | Ignore
  | Active(_) => ()
  };

let with_state = f =>
  switch (global_state^) {
  | Uninit => raise(Not_init)
  | Ignore => ()
  | Active(state) => f(state)
  };

let with_state_else = (~default, f) =>
  switch (global_state^) {
  | Uninit => raise(Not_init)
  | Ignore => default()
  | Active(state) => f(state)
  };

let close = () => {
  with_state(s => {
    Option.iter(s.window, ~f=Window.close_window);
    global_state := Uninit;
  });
};

let update_window = () => {
  with_state(s => {
    Option.iter(
      s.window,
      ~f=
        Window.update(
          ~zoom=s.zoom,
          ~center_x=s.center_x,
          ~center_z=s.center_z,
          s.title,
          Layer.draw_all_layers(s.stack),
        ),
    )
  });
};

let pump_events = () => {
  with_state(s => {Option.iter(s.window, ~f=Window.pump_events)});
};

/**
 * fits the given box into the window, zooming in as much as possible while
 * still fitting the whole box
 */
let apply_fit = (s, ~minx, ~maxx, ~minz, ~maxz) => {
  switch (s.window) {
  | Some(window) =>
    let center_x = (maxx + minx) / 2;
    let center_z = (maxz + minz) / 2;
    let width = maxx - minx;
    let height = maxz - minz;
    let zoom_x = (width - 1) / window.width + 1;
    let zoom_z = (height - 1) / window.height + 1;
    s.zoom = max(zoom_x, zoom_z);
    s.center_x = center_x;
    s.center_z = center_z;
  | None => ()
  };
};

let apply_optionals = (zoom, center, fit, title) => {
  with_state(s => {
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
  });
};

let push_layer = () => {
  with_state_else(
    ~default=Layer.fake_layer,
    s => {
      let l = Layer.push_layer(s.stack);
      l;
    },
  );
};

let remove_layer = layer => {
  with_state(s => {Layer.remove_layer(layer, s.stack)});
};

let last_layer = () => {
  with_state_else(~default=() => None, s => {Layer.last_layer(s.stack)});
};

let remove_after_layer = layer => {
  with_state(s => {Layer.remove_after_layer(layer, s.stack)});
};

let update =
    (
      ~zoom=?,
      ~center=?,
      ~fit=?,
      ~title=?,
      ~draw_sparse=?,
      ~draw_dense=?,
      layer,
    ) => {
  apply_optionals(zoom, center, fit, title);
  with_state(s => {
    Tale.block(
      ~always_close=true,
      "Layer update",
      ~f=() => {
        Layer.update(~draw_dense?, ~draw_sparse?, layer, s.stack);
        update_window();
      },
    )
  });
};

let push_update =
    (~zoom=?, ~center=?, ~fit=?, ~title=?, ~draw_sparse=?, ~draw_dense=?, ()) => {
  let l = push_layer();
  update(~zoom?, ~center?, ~fit?, ~title?, ~draw_sparse?, ~draw_dense?, l);
  l;
};

let center = (~zoom=?, ~title=?, center) => {
  apply_optionals(zoom, Some(center), None, title);
  update_window();
};

let fit = (~title=?, fit) => {
  apply_optionals(None, None, Some(fit), title);
  update_window();
};

let save = (~side, ~img_side=512, ~format=Images.Png, file) => {
  Images.(
    with_state(s => {
      let file =
        Config.Paths.drawing(file ++ "." ++ Images.extension(format));

      Tale.blockf(
        ~always_close=true,
        "Saving %s",
        file,
        ~f=() => {
          let zoom = max(1, side / img_side);
          let img_side = side / zoom;
          let img = Rgb24.create(img_side, img_side);
          Layer.draw_all_layers(
            s.stack,
            ~zoom,
            ~x=(0, side - 1),
            ~z=(0, side - 1),
            (x, z, ~color) => {
              let (r, g, b) = Mg_util.Color.split_rgb(color);
              Rgb24.set(img, x / zoom, z / zoom, {r, g, b});
            },
          );
          save(file, Some(format), [], Rgb24(img));
          Rgb24.destroy(img);
          ();
        },
      );
    })
  );
};