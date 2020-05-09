/** t is a Minecraft level, which contains multiple worlds */
type t = {
  name: string,
  spawn: (int, int, int),
  generator: Generator.t,
};

/** level_dat is the NBT that should be written to "level.dat" */
let level_dat = level => {
  let (x, y, z) = level.spawn;
  let generator_options =
    switch (Generator.options(level.generator)) {
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
              "generatorName" >: String(Generator.name(level.generator)),
              "LastPlayed" >: Long(Utils.time_ms()),
              "LevelName" >: String(level.name),
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

/** save writes the level to storage within the given path */
let save = (level_path, level) => {
  /* Create level.dat */
  let level_dat_path = Filename.concat(level_path, "level.dat");
  let level_dat_nbt = level_dat(level);
  Utils.write_file(level_dat_path, f =>
    Nbt.Nbt_printer.print_node(f, level_dat_nbt)
  );

  /* Create the region directory */
  let region_path = Filename.concat(level_path, "region");
  Unix.mkdir(region_path, 0);
  /*
    TODO heightmaps and sunlight should be calculated, though perhaps
    separately from the save function
   */
  /* Save each region */
  /* TODO */
};