open Mg_util;

type game_type =
  | Survival
  | Creative;

type builder = {
  path: string,
  mutable cached_region: option(Region.t),
  memory: Nbt.Nbt_printer.nbt_printer_memory,
};

/** level_dat is the NBT that should be written to "level.dat" */
let level_dat = (~name, ~spawn, ~mode, ~generator) => {
  let (x, y, z) = spawn;
  let generator_options =
    switch (Generator.options(generator)) {
    | Some(options) => Nbt.Node.["generatorOptions" >: String(options)]
    | None => []
    };
  Nbt.Node.(
    ""
    >: Compound([
         "Data"
         >: Compound([
              "allowCommands" >: Byte(1),
              "GameType"
              >: Int(
                   switch (mode) {
                   | Survival => 0l
                   | Creative => 1l
                   },
                 ),
              "generatorName" >: String(Generator.name(generator)),
              "LastPlayed" >: Long(Mg_util.time_ms()),
              "LevelName" >: String(name),
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

let exists_block_in_section = (~cx, ~sy, ~cz, r, fn) => {
  open Mg_util;
  let y_off = Region.section_offset(~sy, r);
  let (x_off, z_off) = Region.chunk_offset(~cx, ~cz, r);
  Range.exists(0, pred(Region.block_per_section_vertical), y => {
    Range.exists(0, pred(Region.block_per_chunk_side), z => {
      Range.exists(0, pred(Region.block_per_chunk_side), x => {
        fn(~x=x + x_off, ~y=y + y_off, ~z=z + z_off)
      })
    })
  });
};

let map_blocks_in_section = (~cx, ~sy, ~cz, r, fn) => {
  open Mg_util;
  let y_off = Region.section_offset(~sy, r);
  let (x_off, z_off) = Region.chunk_offset(~cx, ~cz, r);
  Range.fold(0, pred(Region.block_per_section_vertical), [], (ls, y) => {
    Range.fold(0, pred(Region.block_per_chunk_side), ls, (ls, z) => {
      Range.fold(0, pred(Region.block_per_chunk_side), ls, (ls, x) => {
        [fn(~x=x + x_off, ~y=y + y_off, ~z=z + z_off), ...ls]
      })
    })
  })
  |> List.rev;
};

let exists_block_in_chunk = (~cx, ~cz, r, fn) => {
  Mg_util.(
    Range.exists(0, pred(Region.section_per_chunk_vertical), sy => {
      exists_block_in_section(~cx, ~sy, ~cz, r, fn)
    })
  );
};

let section_nbt = (~cx, ~sy, ~cz, r) => {
  let block_ids =
    map_blocks_in_section(~cx, ~sy, ~cz, r, (~x, ~y, ~z) =>
      Block.id(Region.get_block(~x, ~y, ~z, r))
    );
  let block_data =
    map_blocks_in_section(~cx, ~sy, ~cz, r, (~x, ~y, ~z) =>
      Block.data(Region.get_block(~x, ~y, ~z, r))
    )
    |> Nbt.Node.make_nibble_list;
  Nbt.Node.(
    ""
    >: Compound([
         "Blocks" >: Byte_array(block_ids),
         "Data" >: Byte_array(block_data),
         "Y" >: Byte(sy),
         /*
           Luckily we don't need to calculate and include light levels.
           Minecraft will do it for us when fixing the chunk
          */
       ])
  );
};

let chunk_heightmap = (~cx, ~cz, r) => {
  let (x_off, z_off) = Region.chunk_offset(~cx, ~cz, r);
  Range.fold(0, Region.block_per_chunk_side - 1, [], (ls, z) => {
    Range.fold(
      0,
      Region.block_per_chunk_side - 1,
      ls,
      (ls, x) => {
        let height = Region.height_at(~x=x + x_off, ~z=z + z_off, r);
        [Int32.of_int(height), ...ls];
      },
    )
  })
  |> List.rev;
};

let entities_in_chunk = (~cx, ~cz, r) => {
  List.filter(
    (ent: Entity.t) => {
      open Floats;
      let ent_cx = ~~ent.x / Region.block_per_chunk_side;
      let ent_cz = ~~ent.z / Region.block_per_chunk_side;
      cx == ent_cx && cz == ent_cz;
    },
    Region.all_entities(r),
  );
};

let entity_nbt = (entity: Entity.t) => {
  let Entity.{id, x, y, z} = entity;
  Nbt.Node.(
    Compound([
      "id" >: String(id),
      "Pos" >: List([Double(x), Double(y), Double(z)]),
    ])
  );
};

let chunk_nbt = (~cx, ~cz, r) => {
  /* Save all non-empty sections */
  let sections_nbt =
    List.init(Region.section_per_chunk_vertical, sy => sy)
    |> List.filter(sy =>
         exists_block_in_section(~cx, ~sy, ~cz, r, (~x, ~y, ~z) =>
           Region.get_block(~x, ~y, ~z, r) != Block.Air
         )
       )
    |> List.map(sy => section_nbt(~cx, ~sy, ~cz, r).payload);

  let entities_nbt = entities_in_chunk(~cx, ~cz, r) |> List.map(entity_nbt);

  let heightmap = chunk_heightmap(~cx, ~cz, r);
  let global_cx = r.rx * Region.chunk_per_region_side + cx;
  let global_cz = r.rz * Region.chunk_per_region_side + cz;
  Nbt.Node.(
    ""
    >: Compound([
         "Level"
         >: Compound([
              "Sections" >: List(sections_nbt),
              "xPos" >: Int(global_cx |> Int32.of_int),
              "zPos" >: Int(global_cz |> Int32.of_int),
              "LastUpdate" >: Long(Mg_util.time_ms()),
              "V" >: Byte(1),
              "LightPopulated" >: Byte(1),
              "TerrainPopulated" >: Byte(1),
              "HeightMap" >: Int_array(heightmap),
              "Entities" >: List(entities_nbt),
            ]),
       ])
  );
};

/** sector_bytes is 4 KB, the size of one sector in a region file */
let sector_bytes = 4096;

/**
  save_region writes the given region to storage

  Region files are split into 4KB sectors. The first two sectors are headers:

  - Sector 0: 3 bytes per chunk listing the sector # where that chunk starts
    and 1 byte listing # of sectors in length. Chunks are in [[z][x]] order.
  - Sector 1: 4 bytes per chunk listing the modification timestamp of that
    chunk. Same order as above.

  After that, each region can take 1 or more sectors to write its NBT. At the start of each chunk are 5 bytes:

  - A 4 byte integer describing the size in bytes of the chunk, not including this integer.
  - One byte describing the version. We currently only use 2, which means Zlib-compressed NBT.
  - [length - 1] bytes of compressed chunk NBT data.
*/
let save_region = (memory, region_path, r: Region.t) => {
  let Region.{rx, rz, _} = r;

  let region_file_path =
    Printf.sprintf("r.%d.%d.mca", rx, rz) |> Filename.concat(region_path, _);
  Mg_util.write_file(
    region_file_path,
    f => {
      /* Write chunk sector offsets & lengths */
      /*
        First we'll write them all as zero, then we'll update the byte array as
        we serialize chunks. Finally at the end we'll rewind the file and write
        the updated bytes.
       */
      let chunk_offset_sizes =
        Bytes.make(
          4 * Region.chunk_per_region_side * Region.chunk_per_region_side,
          Char.chr(0),
        );
      output_bytes(f, chunk_offset_sizes);

      /* Write modification times (repeat current time 32 * 32 times) */
      let now = Mg_util.time_ms() |> Int64.to_int32(_);
      let now_bytes = Bytes.create(4);
      Bytes.set_int32_be(now_bytes, 0, now);
      for (_ in
           0 to
           pred(Region.chunk_per_region_side * Region.chunk_per_region_side)) {
        output_bytes(f, now_bytes);
      };

      /* Write each chunk */
      for (cz in 0 to pred(Region.chunk_per_region_side)) {
        for (cx in 0 to pred(Region.chunk_per_region_side)) {
          let i = cz * Region.chunk_per_region_side + cx;
          if (exists_block_in_chunk(~cx, ~cz, r, (~x, ~y, ~z) =>
                Region.get_block(~x, ~y, ~z, r) != Block.Air
              )) {
            /* Make sure we're at the start of a sector */
            assert(pos_out(f) mod sector_bytes == 0);

            /* Deflate chunk NBT. Keep chunk NBT and buffer in smaller scope to reduce memory, perhaps */
            let chunk_deflated = {
              let nbt = chunk_nbt(~cx, ~cz, r);
              Nbt.Nbt_printer.print_nbt(~memory, ~gzip=false, nbt);
            };

            let start = pos_out(f);
            let offset_sectors = start / sector_bytes;
            let length =
              Int32.of_int(Bigarray.Array1.dim(chunk_deflated) + 1);
            /* 4 bytes of length. Always use version 2 */
            let%bitstring sector_header = {| length : 32; 2 : 8 |};
            Bitstring.bitstring_to_chan(sector_header, f);
            /* Write the deflated chunk */
            for (cd_i in 0 to pred(Bigarray.Array1.dim(chunk_deflated))) {
              output_char(f, chunk_deflated.{cd_i});
            };

            /* Move up to the next 4 KB sector */
            let until_next_sector =
              sector_bytes - pos_out(f) mod sector_bytes;
            if (until_next_sector < sector_bytes) {
              for (_ in 0 to pred(until_next_sector)) {
                output_byte(f, 0);
              };
            };
            /* Make sure we're at the start of a sector */
            assert(pos_out(f) mod sector_bytes == 0);

            /*
              We know we're at the next sector boundary, so it'll divide into a
              whole number. Otherwise we'd have to use float division and take
              the ceiling.
             */
            let length_sectors = (pos_out(f) - start) / sector_bytes;

            /* Store the offset and length into the header bytes */
            /* let%bitstring header_offset_size = {| offset_sectors : 24; length_sectors : 8 |}; */
            let header_byte =
              Int32.(
                logor(
                  shift_left(of_int(offset_sectors), 8),
                  logand(of_int(length_sectors), 0xFFl),
                )
              );
            Bytes.set_int32_be(chunk_offset_sizes, i * 4, header_byte);
          };
        };
      };

      seek_out(f, 0);
      output_bytes(f, chunk_offset_sizes);
    },
  );
};

let make =
    (name, ~generator=Generator.Flat, ~mode=Creative, ~spawn=(0, 0, 0), fn) => {
  /* Create the directory structure: worlds, level, region */
  let base_path = Filename.current_dir_name;
  let worlds_path = Filename.concat(base_path, "worlds");
  let level_path = Filename.concat(worlds_path, name);

  Mg_util.mkdir(level_path);

  /* Create the lock file */
  let session_lock_path = Filename.concat(level_path, "session.lock");
  Mg_util.write_file(session_lock_path, f => {
    Mg_util.output_int64_be(f, Mg_util.time_ms())
  });

  /* Create level.dat */
  let level_dat_path = Filename.concat(level_path, "level.dat");
  let level_dat_nbt = level_dat(~name, ~spawn, ~mode, ~generator);
  Mg_util.write_file(level_dat_path, f => {
    Nbt.Nbt_printer.print_nbt_f(f, level_dat_nbt)
  });

  /* Create the region directory */
  let region_path = Filename.concat(level_path, "region");
  Mg_util.mkdir(region_path);

  fn({
    path: region_path,
    cached_region: None,
    memory: Nbt.Nbt_printer.create_memory(),
  });
};

let make_region = (~rx: int, ~rz: int, builder: builder, fn) => {
  Printf.printf("Creating region (%d, %d)\n", rx, rz);
  let start_time = Mg_util.time_ms();

  let r =
    switch (builder.cached_region) {
    | Some(r) =>
      Region.reset(~rx, ~rz, r);
      r;
    | None =>
      let r = Region.create(~rx, ~rz);
      builder.cached_region = Some(r);
      r;
    };
  let result = fn(r);
  save_region(builder.memory, builder.path, r);

  let elapsed_time =
    Int64.sub(Mg_util.time_ms(), start_time) |> Int64.to_float;
  Printf.printf("Finished (%d, %d) in %fs\n", rx, rz, elapsed_time /. 1000.);

  result;
};