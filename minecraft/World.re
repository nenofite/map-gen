/**
  t is a world, which theoretically could contain multiple levels, however we
  don't need that functionality so it's not implemented
 */
type t = {level: Level.t};

/** save writes the world and all its levels to storage */
let save = world => {
  /* Create the directory structure: worlds, level, region */
  let base_path = Filename.current_dir_name;
  let worlds_path = Filename.concat(base_path, "worlds");
  let level_path = Filename.concat(worlds_path, world.level.name);

  Utils.mkdir(level_path);

  /* Create the lock file */
  let session_lock_path = Filename.concat(level_path, "session.lock");
  Utils.write_file(session_lock_path, f => {
    Utils.output_int64_be(f, Utils.time_ms())
  });

  /* Save each level */
  Level.save(level_path, world.level);

  print_endline("Finished saving world.");
};