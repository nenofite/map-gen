/**
  config describes a world and single level. Theoretically it could contain
  multiple levels, however we don't need that functionality so it's not
  implemented
 */
type config = {
  name: string,
  spawn: (int, int, int),
  generator: Generator.t,
};

/** level_dat is the NBT that should be written to "level.dat" */
let level_dat = config => {
  let (x, y, z) = config.spawn;
  let generator_options =
    switch (Generator.options(config.generator)) {
    | Some(options) => Nbt.Node.["generatorOptions" >: String(options)]
    | None => []
    };
  Nbt.Node.(
    ""
    >: Compound([
         "Data"
         >: Compound([
              "allowCommands" >: Byte(1),
              "GameType" >: Int(1l), /* creative */
              "generatorName" >: String(Generator.name(config.generator)),
              "LastPlayed" >: Long(Mg_util.time_ms()),
              "LevelName" >: String(config.name),
              "MapFeatures" >: Byte(0), /* don't generate structures */
              "RandomSeed" >: Long(1L),
              "SpawnX" >: Int(Int32.of_int(x)),
              "SpawnY" >: Int(Int32.of_int(y)),
              "SpawnZ" >: Int(Int32.of_int(z)),
              "version" >: Int(19133l),
              ...generator_options,
            ]),
       ])
  );
};

/**
  save writes the world data and directory structure. It does not save any
  regions. To save regions, use the region path it returns
 */
let save = config => {
  /* Create the directory structure: worlds, level, region */
  let base_path = Filename.current_dir_name;
  let worlds_path = Filename.concat(base_path, "worlds");
  let level_path = Filename.concat(worlds_path, config.name);

  Mg_util.mkdir(level_path);

  /* Create the lock file */
  let session_lock_path = Filename.concat(level_path, "session.lock");
  Mg_util.write_file(session_lock_path, f => {
    Mg_util.output_int64_be(f, Mg_util.time_ms())
  });

  /* Create level.dat */
  let level_dat_path = Filename.concat(level_path, "level.dat");
  let level_dat_nbt = level_dat(config);
  Mg_util.write_file(level_dat_path, f => {
    Nbt.Nbt_printer.print_nbt_f(f, level_dat_nbt)
  });

  /* Create the region directory */
  let region_path = Filename.concat(level_path, "region");
  Mg_util.mkdir(region_path);

  region_path;
};