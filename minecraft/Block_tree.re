/** section is 16x16x16 blocks. Blocks are stored in [[x][z][y]] order. */
type section = {blocks: array(array(array(Block.material)))};

/** chunk is 16 sections stacked vertically */
type chunk = {sections: array(section)};

/** region is 32x32 chunks */
type region = {chunks: array(array(chunk))};

type t = {regions: Hashtbl.t((int, int), region)};

let chunk_side = 16;
let section_volume = chunk_side * chunk_side * chunk_side;
let chunk_sections = 16;
let region_side = 32;

/** create creates an empty block tree with no regions yet */
let create = () => {regions: Hashtbl.create(16)};

let make_empty_section = (_cx, _cz, _section_y) => {
  blocks:
    Array.(
      init(chunk_side, _x =>
        init(chunk_side, _z => make(chunk_side, Block.Air))
      )
    ),
};

let make_empty_chunk = (cx, cz) => {
  sections:
    Array.init(chunk_sections, section_y =>
      make_empty_section(cx, cz, section_y)
    ),
};

let make_empty_region = (rx, rz) => {
  chunks:
    Array.(
      init(region_side, cx_off =>
        Array.init(region_side, cz_off =>
          make_empty_chunk(rx + cx_off, rz + cz_off)
        )
      )
    ),
};

/* Chunk and block access */

/** get_region retrieves the region at the given region-level coords. It creates the region if needed. */
let get_region = (tree, rx, rz) => {
  switch (Hashtbl.find_opt(tree.regions, (rx, rz))) {
  | Some(region) => region
  | None =>
    let region = make_empty_region(rx, rz);
    Hashtbl.replace(tree.regions, (rx, rz), region);
    region;
  };
};

/** get_chunk retrieves the chunk at the given chunk-level coords. It creates the parent region if needed. */
let get_chunk = (tree, cx, cz) => {
  let rx = cx / region_side;
  let rz = cz / region_side;
  let cx_off = cx - rx * region_side;
  let cz_off = cz - rz * region_side;
  let region = get_region(tree, rx, rz);
  region.chunks[cx_off][cz_off];
};

let assert_xyz = (x, y, z) =>
  if (!(
        0 <= x
        && x < chunk_side
        && 0 <= y
        && y < chunk_side
        * chunk_sections
        && 0 <= z
        && z < chunk_side
      )) {
    let msg =
      Printf.sprintf(
        "local coordinates outside of chunk: (%d, %d, %d)",
        x,
        y,
        z,
      );
    raise(Invalid_argument(msg));
  };

let get_block = (chunk, x, y, z) => {
  assert_xyz(x, y, z);
  let section_y = y / chunk_side;
  /* We know y >= 0, so it's safe to use mod */
  let y = y mod chunk_side;
  chunk.sections[section_y].blocks[x][z][y];
};

let set_block = (chunk, x, y, z, block) => {
  assert_xyz(x, y, z);
  let section_y = y / chunk_side;
  /* We know y >= 0, so it's safe to use mod */
  let y = y mod chunk_side;
  chunk.sections[section_y].blocks[x][z][y] = block;
};

let set_block_any_chunk = (tree, x, y, z, block) => {
  let cx = x / chunk_side;
  let cz = z / chunk_side;
  let x = x - cx * chunk_side;
  let z = z - cz * chunk_side;
  let chunk = get_chunk(tree, cx, cz);
  set_block(chunk, x, y, z, block);
};

let rec height_at' = (chunk, x, y, z) =>
  if (y > 0) {
    switch (get_block(chunk, x, y, z)) {
    | Air => height_at'(chunk, x, y - 1, z)
    | _ => y
    };
  } else {
    0;
  };
/** height_at is the y-coord of the highest non-Air block */
let height_at = (chunk, x, z) => {
  let y = chunk_side * chunk_sections - 1;
  height_at'(chunk, x, y, z);
};

/** section_has_blocks is true iff the section contains at least one non-Air block */
let section_has_blocks = section => {
  Array.(
    exists(
      zys => exists(ys => exists(block => block != Block.Air, ys), zys),
      section.blocks,
    )
  );
};

/** chunk_has_blocks is true iff the chunk contains at least one non-Air block */
let chunk_has_blocks = chunk => {
  Array.exists(section_has_blocks, chunk.sections);
};

/* Saving */

let section_i_of_xyz = (x, y, z) => {
  assert_xyz(x, y, z);
  y * chunk_side * chunk_side + z * chunk_side + x;
};

let section_xyz_of_i = i => {
  let y = i / chunk_side / chunk_side;
  let i = i - y * chunk_side * chunk_side;
  let z = i / chunk_side;
  let i = i - z * chunk_side;
  let x = i;
  assert_xyz(x, y, z);
  (x, y, z);
};

let section_nbt = (section, section_y) => {
  let block_ids =
    Array.init(
      section_volume,
      i => {
        let (x, y, z) = section_xyz_of_i(i);
        Block.id(section.blocks[x][z][y]);
      },
    );
  let block_data =
    Nibble_array.init(
      section_volume,
      i => {
        let (x, y, z) = section_xyz_of_i(i);
        Block.data(section.blocks[x][z][y]);
      },
    );
  Nbt.Node.(
    ""
    >: Compound([
         "Blocks" >: make_byte_array(block_ids),
         "Data" >: make_byte_array(block_data), /* TODO add support for block data */
         "Y" >: Byte(section_y),
         /*
           Luckily we don't need to calculate and include light levels.
           Minecraft will do it for us when fixing the chunk
          */
       ])
  );
};

let chunk_heightmap = chunk => {
  Array.init(
    chunk_side * chunk_side,
    i => {
      let z = i / chunk_side;
      let i = i - z * chunk_side;
      let x = i;
      height_at(chunk, x, z) |> Int32.of_int;
    },
  );
};

let chunk_nbt = (chunk, cx, cz) => {
  /* Save all non-empty sections */
  let sections =
    List.(
      Array.to_list(chunk.sections)
      |> mapi((section_y, section) => (section_y, section), _)
      |> filter(((_, section)) => section_has_blocks(section), _)
      |> map(((section_y, section)) =>
           section_nbt(section, section_y).payload
         )
    );
  let heightmap = chunk_heightmap(chunk);
  Nbt.Node.(
    ""
    >: Compound([
         "Level"
         >: Compound([
              "Sections" >: List(sections),
              "xPos" >: Int(cx |> Int32.of_int),
              "zPos" >: Int(cz |> Int32.of_int),
              "LastUpdate" >: Long(Utils.time_ms()),
              "V" >: Byte(1),
              "LightPopulated" >: Byte(1),
              "TerrainPopulated" >: Byte(1),
              "HeightMap" >: make_int_array(heightmap),
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
let save_region = (region_path, tree, rx, rz) => {
  let region = get_region(tree, rx, rz);
  let region_file_path =
    Printf.sprintf("r.%d.%d.mca", rx, rz) |> Filename.concat(region_path, _);
  Utils.write_file(
    region_file_path,
    f => {
      /* Write chunk sector offsets & lengths */
      /*
        First we'll write them all as zero, then we'll update the byte array as
        we serialize chunks. Finally at the end we'll rewind the file and write
        the updated bytes.
       */
      let chunk_offset_sizes =
        Bytes.make(4 * region_side * region_side, Char.chr(0));
      output_bytes(f, chunk_offset_sizes);

      /* Write modification times (repeat current time 32 * 32 times) */
      let now = Utils.time_ms() |> Int64.to_int32(_);
      let now_bytes = Bytes.create(4);
      Bytes.set_int32_be(now_bytes, 0, now);
      for (_ in 0 to pred(region_side * region_side)) {
        output_bytes(f, now_bytes);
      };

      /* Write each chunk */
      for (z in 0 to pred(region_side)) {
        for (x in 0 to pred(region_side)) {
          let i = z * region_side + x;
          let chunk = region.chunks[x][z];
          if (chunk_has_blocks(chunk)) {
            /* Make sure we're at the start of a sector */
            assert(pos_out(f) mod sector_bytes == 0);

            /* Deflate chunk NBT. Keep chunk NBT and buffer in smaller scope to reduce memory, perhaps */
            let chunk_deflated = {
              let nbt = chunk_nbt(chunk, x, z);
              Nbt.Nbt_printer.print_nbt(~gzip=false, nbt);
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

/** save writes all regions to storage */
let save = (region_path, tree) => {
  Hashtbl.iter(
    ((rx, rz), _region) => save_region(region_path, tree, rx, rz),
    tree.regions,
  );
};