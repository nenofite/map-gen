type site =
  | Test
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
        | {floor_elev, ceiling_elev} when ceiling_elev > floor_elev =>
          /* TODO remove */
          Printf.printf("cavern entrance at %d, %d\n", x, y);
          Some(Cavern_entrance(floor_elev + 4));
        | _ =>
          /* TODO remove */
          Printf.printf("test site at %d, %d\n", x, y);
          Some(Test);
        }
      | _ => None
      };
    },
  );
};

let apply_standard = (args, ~x, ~z, template) => {
  Building.apply_template(args, ~x, ~z, template);
};

let apply_cavern_entrance = (args, ~tube_depth, ~x, ~z) => {
  let y =
    Building.apply_template_y(args, ~x, ~z, Site_templates.cavern_entrance);
  for (y in tube_depth to y - 1) {
    Minecraft.Template.place_overwrite(
      Site_templates.cavern_entrance_tube,
      args.region,
      x,
      y,
      z,
    );
  };
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
        | Some(Test) => apply_standard(args, ~x, ~z, Site_templates.test)
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