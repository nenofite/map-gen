open! Core;

let should_install_at = ref(None: option(string));

let set_install_world_at = p => {
  should_install_at := p;
  switch (p) {
  | Some(path) => Tale.logf("Will install to %s", path)
  | None => ()
  };
};

let install_path = (f: string) => {
  let remove_seed_prefix = f => {
    switch (Filename.parts(f)) {
    | [".", seed, "worlds", ...rest]
        when String.is_substring_at(~pos=0, ~substring="seed-", seed) =>
      Filename.of_parts(rest)
    | [_, seed, ..._] =>
      failwithf("Expected seed- prefix but got %s in %s", seed, f, ())
    | _ => failwith("Cannot install empty path")
    };
  };
  switch (should_install_at^) {
  | Some(install_base) =>
    let dest = Filename.concat(install_base, remove_seed_prefix(f));
    let dest_dir = Filename.dirname(dest);
    Tale.logf("Installing %s -> %s", f, dest);
    Mg_util.mkdir(dest_dir);
    Mg_util.copy_file(f, ~dest);
  | None => ()
  };
};