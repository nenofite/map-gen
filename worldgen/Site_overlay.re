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

let max_height_within =
    (
      args: Minecraft_converter.region_args,
      ~minx,
      ~maxx,
      ~y=?,
      ~minz,
      ~maxz,
      (),
    )
    : int => {
  let m = ref(0);
  for (z in minz to maxz) {
    for (x in minx to maxx) {
      let here = Minecraft.Region.height_at(args.region, ~x, ~y?, ~z);
      m := max(m^, here);
    };
  };
  m^;
};

let apply_cavern_entrance = (args, ~tube_depth, ~x, ~z): unit => {
  let top = Site_templates.cavern_entrance;
  let tube = Site_templates.cavern_entrance_tube;
  let base = Site_templates.cavern_entrance_base;
  let (minx, maxx) = top.bounds_x;
  let (minz, maxz) = top.bounds_z;
  let y =
    max_height_within(
      args,
      ~minx=minx + x,
      ~maxx=maxx + x,
      ~minz=minz + z,
      ~maxz=maxz + z,
      (),
    )
    + 3;
  Building.stair_foundation(
    args,
    ~minx=minx + x,
    ~maxx=maxx + x,
    ~y,
    ~minz=minz + z,
    ~maxz=maxz + z,
  );
  Minecraft.Template.place_overwrite(top, args.region, ~x, ~y, ~z);
  let tube_height = Minecraft.Template.height(tube);
  let base_height = Minecraft.Template.height(base);
  let tube_sections = (y - tube_depth - base_height) / tube_height;
  for (i in 1 to tube_sections) {
    let y = y - i * tube_height;
    Minecraft.Template.place_overwrite(tube, args.region, ~x, ~y, ~z);
  };
  let y = y - tube_sections * tube_height - base_height;
  Minecraft.Template.place_overwrite(base, args.region, ~x, ~y, ~z);
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
  List.iter(
    (Point_cloud.{x, y: z, value: site}) => {
      let x = int_of_float(x);
      let z = int_of_float(z);
      if (Minecraft.Region.is_within(~x, ~y=0, ~z, args.region)) {
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