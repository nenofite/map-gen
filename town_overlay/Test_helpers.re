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

let show_region_top_down =
    (~show_block, ~resolve_multi=List.hd_exn, r: Minecraft.Region.t)
    : text_grid => {
  let (ox, oz) = Minecraft.Region.region_offset(r);
  let top_down_at = (~x, ~z) => {
    let x = x + ox;
    let z = z + oz;
    let matches =
      Mg_util.Range.map(0, Minecraft.Region.block_per_region_vertical - 1, y =>
        show_block(Minecraft.Region.get_block(~x, ~y, ~z, r))
      )
      |> List.filter_map(~f=opt => opt);
    switch (matches) {
    | [] => " "
    | [single] => single
    | multi => resolve_multi(multi)
    };
  };
  Grid.Mut.init(
    ~side=Minecraft.Region.block_per_region_side,
    ~f=top_down_at,
    "",
  );
};

let show_region_south_north =
    (~show_block, ~resolve_multi=List.hd_exn, r: Minecraft.Region.t)
    : text_grid => {
  let (ox, oz) = Minecraft.Region.region_offset(r);
  let south_north_at = (~x, ~z as y) => {
    let x = x + ox;
    let y = Minecraft.Region.block_per_region_vertical - y - 1;
    if (y < 0) {
      " ";
    } else {
      let matches =
        Mg_util.Range.map(
          oz, oz + Minecraft.Region.block_per_region_side - 1, z =>
          show_block(Minecraft.Region.get_block(~x, ~y, ~z, r))
        )
        |> List.filter_map(~f=opt => opt);
      switch (matches) {
      | [] => " "
      | [single] => single
      | multi => resolve_multi(multi)
      };
    };
  };
  Grid.Mut.init(
    ~side=Minecraft.Region.block_per_region_side,
    ~f=south_north_at,
    "",
  );
};

let cached_region = ref(None: option(Minecraft.Region.t));

let build_test_region = (~rx=0, ~rz=0, ()) => {
  open Minecraft.Region;
  let r =
    switch (cached_region^) {
    | Some(r) =>
      reset(~rx, ~rz, r);
      r;
    | None =>
      let r = create(~rx, ~rz);
      cached_region := Some(r);
      r;
    };

  let elevation = 40;
  iter_region_xz(
    r,
    ~f=(~x, ~z) => {
      set_block(~x, ~y=0, ~z, Bedrock, r);
      for (y in 1 to elevation - 1) {
        set_block(~x, ~y, ~z, Dirt, r);
      };
      set_block(~x, ~y=elevation, ~z, Grass_block, r);
    },
  );
  r;
};