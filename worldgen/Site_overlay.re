type template =
  | Test;

type site =
  | From_template(template);

type t = Point_cloud.t(site);

let template_path = t => {
  let file =
    switch (t) {
    | Test => "test"
    };
  Filename.concat("sites", file ++ ".txt");
};

let load_template = t =>
  Util.read_file(template_path(t), f => Template_txt.read_template(f));

let lazy_templates =
  lazy({
    let test = load_template(Test);
    fun
    | Test => test;
  });

let get_template = t => Lazy.force(lazy_templates, t);

let prepare = (base: Base_overlay.t, ()) => {
  Point_cloud.init_f(
    ~width=base.side,
    ~height=base.side,
    ~spacing=128,
    (~xf, ~yf, ~xi as _, ~yi as _) => {
      let x = int_of_float(xf);
      let y = int_of_float(yf);
      switch (Grid.at(base, x, y)) {
      | {ocean: false, river: false, _} =>
        /* TODO remove */
        Printf.printf("site at %d, %d\n", x, y);
        Some(From_template(Test));
      | _ => None
      };
    },
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
        | Some(From_template(t)) =>
          let template = get_template(t);
          Building.apply_template(args, ~x, ~z, template);
        | None => ()
        };
      };
    },
    Point_cloud.(sites.points),
  );
};

let overlay = base =>
  Overlay.make("site", prepare(base), apply_region(base));