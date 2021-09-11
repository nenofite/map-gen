open! Core_kernel;

type text_grid = Grid.Mut.t(string);

let show_grid =
    (~side, ~get, ~show_cell, ~center=(side / 2, side / 2), ~radius=side, ())
    : text_grid => {
  let (cx, cz) = center;
  let min_x = Int.clamp_exn(cx - radius, ~min=0, ~max=side - 1);
  let max_x = Int.clamp_exn(cx + radius, ~min=0, ~max=side - 1);
  let min_z = Int.clamp_exn(cz - radius, ~min=0, ~max=side - 1);
  let max_z = Int.clamp_exn(cz + radius, ~min=0, ~max=side - 1);
  Grid.Mut.init(
    ~side=max(max_x - min_x + 1, max_z - min_z + 1),
    ~f=
      (~x, ~z) => {
        let x = x + min_x;
        let z = z + min_z;
        if (min_x <= x && x <= max_x && min_z <= z && z <= max_z) {
          show_cell(get(x, z));
        } else {
          "";
        };
      },
    "",
  );
};

let diff_grid = (new_grid: text_grid, ~base: text_grid): text_grid => {
  assert(Grid.Mut.side(new_grid) == Grid.Mut.side(base));
  Grid.Mut.init(
    ~side=Grid.Mut.side(new_grid),
    ~f=
      (~x, ~z) => {
        let old_val = Grid.Mut.get(~x, ~z, base);
        let new_val = Grid.Mut.get(~x, ~z, new_grid);
        if (String.(new_val != old_val)) {
          new_val;
        } else {
          " ";
        };
      },
    "",
  );
};

let print_grid = (~center=?, ~radius=?, grid: text_grid): unit => {
  let side = Grid.Mut.side(grid);
  let (cx, cz) = Option.value(center, ~default=(side / 2, side / 2));
  let radius = Option.value(radius, ~default=side);
  let min_x = Int.clamp_exn(cx - radius, ~min=0, ~max=side - 1);
  let max_x = Int.clamp_exn(cx + radius, ~min=0, ~max=side - 1);
  let min_z = Int.clamp_exn(cz - radius, ~min=0, ~max=side - 1);
  let max_z = Int.clamp_exn(cz + radius, ~min=0, ~max=side - 1);
  for (z in min_z to max_z) {
    for (x in min_x to max_x) {
      Out_channel.output_string(stdout, Grid.Mut.get(~x, ~z, grid));
      Out_channel.output_char(stdout, ' ');
    };
    Out_channel.newline(stdout);
  };
};

let make_running_diff = () => {
  let previous = ref(None);
  (new_grid: text_grid) => {
    let to_show =
      switch (previous^) {
      | Some(base) => diff_grid(new_grid, ~base)
      | None => new_grid
      };
    previous := Some(new_grid);
    to_show;
  };
};

let init_overlays_for_test = (~seed): unit => {
  Progress_view.init_ignore();
  Overlay.init(seed);
  Overlay.Canon.init(~side=Minecraft.Region.block_per_region_side);
};