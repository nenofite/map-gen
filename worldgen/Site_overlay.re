type site =
  | Test
  | Cavern_entrance(int);

type template =
  | Test_txt
  | Cavern_entrance_txt
  | Cavern_entrance_b_txt;

type t = Point_cloud.t(site);

let template_path = t => {
  let file =
    switch (t) {
    | Test_txt => "test"
    | Cavern_entrance_txt => "cavern_entrance"
    | Cavern_entrance_b_txt => "cavern_entrance_b"
    };
  Filename.concat("sites", file ++ ".txt");
};

let load_template = t =>
  Util.read_file(template_path(t), f => Template_txt.read_template(f));

let lazy_templates =
  lazy({
    let test = load_template(Test_txt);
    let cavern_entrance = load_template(Cavern_entrance_txt);
    let cavern_entrance_b = load_template(Cavern_entrance_b_txt);
    fun
    | Test_txt => test
    | Cavern_entrance_txt => cavern_entrance
    | Cavern_entrance_b_txt => cavern_entrance_b;
  });

let get_template = t => Lazy.force(lazy_templates, t);

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

let apply_standard = (args, ~x, ~z, t) => {
  let template = get_template(t);
  Building.apply_template(args, ~x, ~z, template);
};

let apply_cavern_entrance = (args, ~tube_depth, ~x, ~z) => {
  let structure = get_template(Cavern_entrance_txt);
  let y = Building.apply_template_y(args, ~x, ~z, structure);
  let tube = get_template(Cavern_entrance_b_txt);
  for (y in tube_depth to y - 1) {
    Minecraft.Template.place_overwrite(tube, args.region, x, y, z);
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
        | Some(Test) => apply_standard(args, ~x, ~z, Test_txt)
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