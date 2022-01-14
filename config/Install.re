open! Core_kernel;

let install_base =
  Filename.concat(
    Sys.getenv("HOME"),
    "Library/Application Support/minecraft/saves/",
  );

let should_install = ref(false);

let set_install_world = v => {
  should_install := v;
  if (v) {
    Tale.logf("Will install to %s", install_base);
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
  if (should_install^) {
    let dest = Filename.concat(install_base, remove_seed_prefix(f));
    let dest_dir = Filename.dirname(dest);
    Tale.logf("Installing %s -> %s", f, dest);
    Mg_util.mkdir(dest_dir);
    Mg_util.copy_file(f, ~dest);
  };
};