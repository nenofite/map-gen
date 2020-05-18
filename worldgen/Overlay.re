type monad('state) = {
  prepare: unit => ('state, Minecraft_converter.region_args => unit),
};

let make =
    (
      name: string,
      prepare: unit => 'a,
      apply_region: ('a, Minecraft_converter.region_args) => unit,
    )
    : monad('a) => {
  let prepare = () => {
    let state =
      Util.print_progress("Preparing " ++ name ++ " overlay", prepare);
    let apply_region = args =>
      Util.print_progress("Applying " ++ name ++ " overlay", () =>
        apply_region(state, args)
      );
    (state, apply_region);
  };
  {prepare: prepare};
};

let bind = (m, ~f) => {
  let prepare = () => {
    let next_seed = Random.bits();
    let (m_state, m_apply_f) = m.prepare();
    Random.init(next_seed);
    let (o_state, o_apply_f) = f(m_state).prepare();
    let apply_f = args => {
      m_apply_f(args);
      Random.init(next_seed);
      o_apply_f(args);
    };
    (o_state, apply_f);
  };
  {prepare: prepare};
};

let return = v => {
  let prepare = () => {
    (v, _ => ());
  };
  {prepare: prepare};
};

module Let_syntax = {
  let bind = bind;
  let return = return;
};

let prepare = (seed, monad) => {
  Random.init(seed);
  let (_state, apply_region) = monad.prepare();
  apply_region;
};