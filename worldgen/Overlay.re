open Core_kernel;

type monad('state) = {
  prepare: unit => ('state, Minecraft_converter.region_args => unit),
};

exception Overlay_cache_error(string);

let cache_path = name => Filename.concat("overlays", name ++ ".overlay");

let read_cache = (reader, path) => {
  switch (In_channel.read_all(path)) {
  | str =>
    switch (Bigstring.(of_string(str) |> read_bin_prot(_, reader))) {
    | Ok((state, _)) => Some(state)
    | Error(_) => None
    }
  | exception (Sys_error(_)) => None
  };
};

let save_cache = (name, writer, state) => {
  open Out_channel;
  Tale.logf("Saving to cache...");
  flush(stdout);
  Mg_util.mkdir("overlays");
  with_file(
    cache_path(name),
    ~f=f => {
      let buf = Bin_prot.Utils.bin_dump(~header=true, writer, state);
      /* TODO this is probably not efficient */
      output_bytes(f, Bigstring.to_bytes(buf));
    },
  );
  Tale.logf("Done");
};

let make =
    (
      name: string,
      ~apply_progress_view: 'a => unit=_ => (),
      prepare: unit => 'a,
      apply_region: ('a, Minecraft_converter.region_args) => unit,
      reader: Bin_prot.Type_class.reader('a),
      writer: Bin_prot.Type_class.writer('a),
    )
    : monad('a) => {
  let prepare = () => {
    /* Try to read a cached version first */
    let state =
      switch (read_cache(reader, cache_path(name))) {
      | Some(state) =>
        Tale.logf("Read %s overlay from cache", name);
        state;
      | None =>
        Mg_util.print_progress(
          "Preparing " ++ name ++ " overlay",
          () => {
            let state = prepare();
            save_cache(name, writer, state);
            state;
          },
        )
      };
    apply_progress_view(state);
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
    let next_seed = /* TODO */ Caml.Random.bits();
    let (m_state, m_apply_f) = m.prepare();
    /* TODO */ Caml.Random.init(next_seed);
    let (o_state, o_apply_f) = f(m_state).prepare();
    let apply_f = args => {
      m_apply_f(args);
      /* TODO */ Caml.Random.init(next_seed);
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
  /* TODO */ Caml.Random.init(seed);
  let (_state, apply_region) = monad.prepare();
  apply_region;
};

let%expect_test "consistent random state" = {
  let overlay_a: monad(unit) = {
    prepare: () => {
      /* Calls twice */
      /* TODO */ Caml.Random.bits() |> ignore;
      /* TODO */ Caml.Random.bits() |> ignore;
      ((), _ => ());
    },
  };
  let overlay_b: monad(unit) = {
    prepare: () => {
      /* Calls once */
      /* TODO */ Caml.Random.bits() |> ignore;
      ((), _ => ());
    },
  };
  let spy: monad(unit) = {
    prepare: () => {
      let test = /* TODO */ Caml.Random.int(100);
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