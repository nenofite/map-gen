open! Core_kernel;

let t = 3 * Base_overlay.precision_coef;
let sea_level = Base_overlay.sea_level * Base_overlay.precision_coef;
let shore_level = Base_overlay.shore_level * Base_overlay.precision_coef;

let erode_shore = (~x, ~z, elevation) => {
  let height = Grid.get(~x, ~z, elevation);
  if (height > sea_level && height <= shore_level) {
    let sum = ref(height);
    List.iter(
      Grid.four_directions,
      ~f=((dx, dz)) => {
        let hx = x + dx;
        let hz = z + dz;
        sum := sum^ + Grid.get(~x=hx, ~z=hz, elevation);
      },
    );
    let newheight =
      Int.clamp_exn(
        height / 5,
        ~min=sea_level + Base_overlay.precision_coef,
        ~max=shore_level,
      );
    Grid.set(~x, ~z, newheight, elevation);
  };
};

let erode_flat_slope = (~x, ~z, elevation) => {
  let height = Grid.get(~x, ~z, elevation);
  if (height > sea_level) {
    let dmax = ref(-1);
    let xmax = ref(0);
    let zmax = ref(0);
    List.iter(
      Grid.four_directions,
      ~f=((dx, dz)) => {
        let hx = x + dx;
        let hz = z + dz;
        let dh = height - Grid.get(~x=hx, ~z=hz, elevation);
        if (dh > dmax^) {
          dmax := dh;
          xmax := hx;
          zmax := hz;
        };
      },
    );
    if (dmax^ > 0 && dmax^ <= t) {
      let height2 = Grid.get(~x=xmax^, ~z=zmax^, elevation);
      let newh = (height + height2) / 2;
      Grid.set(~x, ~z, newh, elevation);
      Grid.set(~x=xmax^, ~z=zmax^, newh, elevation);
    };
  };
};

let erode_at = (~x, ~z, ~elevation, ~biome_at) => {
  /* switch (Biome_overlay.biome_at(~x, ~z, biomes)) { */
  switch (biome_at(~x, ~z)) {
  | Biome_overlay_i.River
  | Ocean => ()
  | Shore
  | Stone_shore => erode_shore(~x, ~z, elevation)
  | Desert(_) => /* TODO */ ()
  | Plain(_)
  | Forest(_)
  | Savanna
  | Pine_forest
  | Barren_mountain
  | Snow_mountain
  | Snow_plains
  | Snow_taiga => erode_flat_slope(~x, ~z, elevation)
  };
};

let smooth_edges = (~elevation) => {
  let side = Grid.side(elevation);
  let ns = Array.create(~len=List.length(Grid.four_directions) + 1, 0);
  for (z in 1 to side - 2) {
    for (x in 1 to side - 2) {
      ns[0] = Grid.get(~x, ~z, elevation);
      List.iteri(
        Grid.four_directions,
        ~f=(i, (dx, dz)) => {
          let hx = x + dx;
          let hz = z + dz;
          let h = Grid.get(~x=hx, ~z=hz, elevation);
          ns[i + 1] = h;
        },
      );
      Array.sort(~compare=Int.compare, ns);
      Grid.set(~x, ~z, ns[Array.length(ns) / 2], elevation);
    };
  };
};

let erode = (~biome_at, ~elevation) => {
  let side = Grid.side(elevation);

  Grid.map_in_place(elevation, ~f=(~x as _, ~z as _, e) =>
    e * Base_overlay.precision_coef
  );

  Tale.log_progress(1, 50, ~label="Eroding", ~f=_ => {
    for (z in 1 to side - 2) {
      for (x in 1 to side - 2) {
        erode_at(~x, ~z, ~elevation, ~biome_at);
      };
    }
  });

  Tale.log("Smoothing");
  smooth_edges(~elevation);

  Grid.map_in_place(elevation, ~f=(~x as _, ~z as _, e) =>
    (e + Base_overlay.precision_coef / 2) / Base_overlay.precision_coef
  );
};