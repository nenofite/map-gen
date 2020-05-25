type site =
  | Cavern_entrance(int);

type t = Point_cloud.t(site);

let prepare = (base: Base_overlay.t, cavern: Cavern_overlay.t, ()) => {
  Point_cloud.init_f(
    ~width=base.side,
    ~height=base.side,
    ~spacing=128,
    (~xf, ~yf, ~xi as _, ~yi as _) => {
      let x = int_of_float(xf);
      let y = int_of_float(yf);
      switch (Grid.at(base, x, y)) {
      | {ocean: false, river: false, _} =>
        switch (Grid.at(cavern, x, y)) {
        | {floor_elev, ceiling_elev}
            when
              ceiling_elev > floor_elev
              && floor_elev > Cavern_overlay.magma_sea_elev =>
          /* TODO remove */
          Printf.printf("cavern entrance at %d, %d\n", x, y);
          Some(Cavern_entrance(floor_elev));
        | _ => None
        }
      | _ => None
      };
    },
  );
};

let apply_standard = (args, ~x, ~z, template) => {
  Building.apply_template(args, ~x, ~z, template);
};

let apply_cavern_entrance = (args, ~tube_depth, ~x, ~z): unit => {
  let y =
    Building.apply_template_y(args, ~x, ~z, Site_templates.cavern_entrance);
  let tube_height =
    Minecraft.Template.height(Site_templates.cavern_entrance_tube);
  let base_height =
    Minecraft.Template.height(Site_templates.cavern_entrance_base);
  let tube_sections = (y - tube_depth - base_height) / tube_height;
  for (i in 1 to tube_sections) {
    let y = y - i * tube_height;
    Minecraft.Template.place_overwrite(
      Site_templates.cavern_entrance_tube,
      args.region,
      x,
      y,
      z,
    );
  };
  let y = y - tube_sections * tube_height - base_height;
  let base = Site_templates.cavern_entrance_base;
  Minecraft.Template.place_overwrite(base, args.region, x, y, z);
  let (minx, maxx) = base.bounds_x;
  let (minz, maxz) = base.bounds_z;
  Building.stair_foundation(
    args,
    ~minx=minx + x,
    ~maxx=maxx + x,
    ~y,
    ~minz=minz + z,
    ~maxz=maxz + z,
  );
};

let apply_region = (_base, sites, args: Minecraft_converter.region_args): unit => {
  let Minecraft_converter.{
        region: _,
        rx: _,
        rz: _,
        gx_offset,
        gy_offset,
        gsize,
      } = args;
  List.iter(
    (Point_cloud.{x, y: z, value: site}) => {
      let x = int_of_float(x) - gx_offset;
      let z = int_of_float(z) - gy_offset;
      if (0 <= x && x < gsize && 0 <= z && z < gsize) {
        switch (site) {
        | Some(Cavern_entrance(tube_depth)) =>
          apply_cavern_entrance(args, ~tube_depth, ~x, ~z)
        | None => ()
        };
      };
    },
    Point_cloud.(sites.points),
  );
};

let overlay = (base, cavern) =>
  Overlay.make("site", prepare(base, cavern), apply_region(base));