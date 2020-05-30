type monad('state) = {
  prepare: unit => ('state, Minecraft_converter.region_args => unit),
};

exception Overlay_cache_error(string);

let magic = 89809344;
let cache_path = name => Filename.concat("overlays", name ++ ".overlay");

let assert_magic = f => {
  let matched =
    switch (input_binary_int(f)) {
    | num => num == magic
    | exception End_of_file => false
    };
  if (!matched) {
    raise(Overlay_cache_error("cached overlay does not have magic value"));
  };
};

let read_cache = f => {
  assert_magic(f);
  let state: 'a =
    try(Marshal.from_channel(f)) {
    | End_of_file
    | Failure(_) => raise(Overlay_cache_error("could not read cache file"))
    };
  close_in(f);
  state;
};

let save_cache = (name, state) => {
  Printf.printf("Saving to cache...");
  flush(stdout);
  Mg_util.mkdir("overlays");
  let f = open_out_bin(cache_path(name));
  output_binary_int(f, magic);
  Marshal.to_channel(f, state, []);
  close_out(f);
  Printf.printf(" done\n");
};

let make =
    (
      name: string,
      prepare: unit => 'a,
      apply_region: ('a, Minecraft_converter.region_args) => unit,
    )
    : monad('a) => {
  let prepare = () => {
    /* Try to read a cached version first */
    let state =
      switch (open_in_bin(cache_path(name))) {
      | f =>
        Mg_util.print_progress("Reading " ++ name ++ " overlay from cache", () =>
          read_cache(f)
        )
      | exception (Sys_error(_)) =>
        /* If cache doesn't exist, generate */
        Mg_util.print_progress(
          "Preparing " ++ name ++ " overlay",
          () => {
            let state = prepare();
            save_cache(name, state);
            state;
          },
        )
      };
    let apply_region = args =>
      Mg_util.print_progress("Applying " ++ name ++ " overlay", () =>
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

let%expect_test "consistent random state" = {
  let overlay_a: monad(unit) = {
    prepare: () => {
      /* Calls twice */
      Random.bits() |> ignore;
      Random.bits() |> ignore;
      ((), _ => ());
    },
  };
  let overlay_b: monad(unit) = {
    prepare: () => {
      /* Calls once */
      Random.bits() |> ignore;
      ((), _ => ());
    },
  };
  let spy: monad(unit) = {
    prepare: () => {
      let test = Random.int(100);
      Printf.printf("%d\n", test);
      ((), _ => ());
    },
  };

  /*
    The two should print the same number, despite their predecessor calling
    Random different amounts
   */
  prepare(1, bind(overlay_a, ~f=_ => spy)) |> ignore;
  %expect
  "13";
  prepare(1, bind(overlay_b, ~f=_ => spy)) |> ignore;
  %expect
  "13";
};