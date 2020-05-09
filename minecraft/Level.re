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
              "LastPlayed" >: Long(0L),
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