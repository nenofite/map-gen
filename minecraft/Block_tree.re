/** section is 16x16x16 blocks. Blocks are stored in [[y][z][x]] order. */
type section = {blocks: array(Block.material)};

/**
  entity is a Minecraft entity[0] such as a villager

  [0]: https://minecraft.gamepedia.com/Chunk_format#Entity_format
 */
type entity = {
  id: string,
  x: float,
  y: float,
  z: float,
};

/**
  t is one Anvil region, 32x32 chunks and each chunk is 16 sections stacked
  vertically. Sections are stored in [[x][z][y]] order
 */
type t = {
  mutable rx: int,
  mutable rz: int,
  sections: array(section),
  mutable entities: list(entity),
};

let block_per_chunk = 16;
let section_volume = block_per_chunk * block_per_chunk * block_per_chunk;
let section_per_chunk = 16;
let chunk_per_region = 32;
let block_per_region = chunk_per_region * block_per_chunk;
let block_per_region_vertical = section_per_chunk * block_per_chunk;

let make_empty_section = () => {
  blocks:
    Array.make(
      block_per_chunk * block_per_chunk * block_per_chunk,
      Block.Air,
    ),
};

let create = (~rx, ~rz) => {
  rx,
  rz,
  sections:
    Array.init(chunk_per_region * chunk_per_region * section_per_chunk, _ =>
      make_empty_section()
    ),
  entities: [],
};

/**
  reset clears all blocks to Air, restoring the region to a consistent state so
  it can be re-used
 */
let reset = (tree, ~rx, ~rz) => {
  Array.(
    iter(
      section => fill(section.blocks, 0, section_volume, Block.Air),
      tree.sections,
    )
  );
  tree.rx = rx;
  tree.rz = rz;
  tree.entities = [];
};

let within_region = (x, y, z) =>
  0 <= x
  && x < block_per_region
  && 0 <= y
  && y < block_per_region_vertical
  && 0 <= z
  && z < block_per_region;

let within_this_region = (tree, x, y, z) => {
  let {rx, rz, _} = tree;
  within_region(x - rx * block_per_region, y, z - rz * block_per_region);
};

/* Entities */

/** adds the entity to the region in-place */
let add_entity = (tree, ~id, ~x, ~y, ~z) => {
  if (!within_this_region(tree, x, y, z)) {
    let msg =
      Printf.sprintf("entity is outside region (%d, %d, %d)", x, y, z);
    raise(Invalid_argument(msg));
  };
  let entity = Floats.{id, x: ~.x, y: ~.y, z: ~.z};
  tree.entities = [entity, ...tree.entities];
};

/** filters down to entities within the given chunk */
let find_entities = (tree, ~cx, ~cz) => {
  let minx = tree.rx * block_per_region + cx * block_per_chunk;
  let maxx = minx + block_per_chunk;
  let minz = tree.rz * block_per_region + cz * block_per_chunk;
  let maxz = minz + block_per_chunk;
  List.filter(
    ent =>
      Floats.(
        minx <= ~~ent.x && ~~ent.x < maxx && minz <= ~~ent.z && ~~ent.z < maxz
      ),
    tree.entities,
  );
};

/* Chunk and block access */

/**
  get_section retrieves the section at the given chunk-level coords and section
  Y. It creates the parent region if needed.
 */
let get_section = (tree, cx, sy, cz) => {
  assert(0 <= cx && cx < chunk_per_region);
  assert(0 <= cz && cz < chunk_per_region);
  assert(0 <= sy && sy < section_per_chunk);
  let i =
    cx * chunk_per_region * section_per_chunk + cz * section_per_chunk + sy;
  tree.sections[i];
};

let assert_xyz = (x, y, z) =>
  if (!(
        0 <= x
        && x < block_per_chunk
        && 0 <= y
        && y < block_per_chunk
        && 0 <= z
        && z < block_per_chunk
      )) {
    let msg =
      Printf.sprintf(
        "local coordinates outside of section: (%d, %d, %d)",
        x,
        y,
        z,
      );
    raise(Invalid_argument(msg));
  };

let get_block_in_section = (section, x, y, z) => {
  assert_xyz(x, y, z);
  let i = y * block_per_chunk * block_per_chunk + z * block_per_chunk + x;
  section.blocks[i];
};

let set_block_in_section = (section, x, y, z, block) => {
  assert_xyz(x, y, z);
  let i = y * block_per_chunk * block_per_chunk + z * block_per_chunk + x;
  section.blocks[i] = block;
};

let get_block = (tree, x, y, z) => {
  let cx = x / block_per_chunk;
  let sy = y / block_per_chunk;
  let cz = z / block_per_chunk;
  let x = x - cx * block_per_chunk;
  let y = y - sy * block_per_chunk;
  let z = z - cz * block_per_chunk;
  let section = get_section(tree, cx, sy, cz);
  get_block_in_section(section, x, y, z);
};

let get_block_opt = (tree, x, y, z) =>
  if (!(
        0 <= x
        && x < block_per_region
        && 0 <= y
        && y < block_per_region_vertical
        && 0 <= z
        && z < block_per_region
      )) {
    None;
  } else {
    Some(get_block(tree, x, y, z));
  };

let set_block = (tree, x, y, z, block) => {
  let cx = x / block_per_chunk;
  let sy = y / block_per_chunk;
  let cz = z / block_per_chunk;
  let x = x - cx * block_per_chunk;
  let y = y - sy * block_per_chunk;
  let z = z - cz * block_per_chunk;
  let section = get_section(tree, cx, sy, cz);
  set_block_in_section(section, x, y, z, block);
};

let set_block_opt = (tree, x, y, z, block) =>
  if (0 <= x
      && x < block_per_region
      && 0 <= y
      && y < block_per_region_vertical
      && 0 <= z
      && z < block_per_region) {
    set_block(tree, x, y, z, block);
  };

let rec highest_such_block = (region, x, y, z, predicate) =>
  if (y > 0) {
    let here = get_block(region, x, y, z);
    if (predicate(here)) {
      Some(y);
    } else {
      highest_such_block(region, x, y - 1, z, predicate);
    };
  } else {
    None;
  };
let highest_such_block =
    (region, ~x, ~y=block_per_region_vertical - 1, ~z, predicate) =>
  highest_such_block(region, x, y, z, predicate);

/** height_at is the y-coord of the highest non-Air block */
let height_at = (tree, ~x, ~y=?, ~z, ()) => {
  switch (highest_such_block(tree, ~x, ~y?, ~z, b => b != Block.Air)) {
  | Some(y) => y
  | None => 0
  };
};

/** section_has_blocks is true iff the section contains at least one non-Air block */
let section_has_blocks = section => {
  Array.exists(block => block != Block.Air, section.blocks);
};

let rec chunk_has_blocks' = (tree, cx, sy, cz) =>
  if (sy < section_per_chunk) {
    if (section_has_blocks(get_section(tree, cx, sy, cz))) {
      true;
    } else {
      chunk_has_blocks'(tree, cx, sy + 1, cz);
    };
  } else {
    false;
  };
/** chunk_has_blocks is true iff the chunk contains at least one non-Air block */
let chunk_has_blocks = (tree, cx, cz) => {
  /* Iterate from bottom to top since lower chunks are much more likely to have blocks */
  chunk_has_blocks'(
    tree,
    cx,
    0,
    cz,
  );
};

/* Saving */

/**
  block_tree_memory contains the large allocations that could be re-used
  between calls to save_region
 */
type block_tree_memory = {
  nbt_printer_memory: Nbt.Nbt_printer.nbt_printer_memory,
};

let create_memory = () => {
  nbt_printer_memory: Nbt.Nbt_printer.create_memory(),
};

let section_i_of_xyz = (x, y, z) => {
  assert_xyz(x, y, z);
  y * block_per_chunk * block_per_chunk + z * block_per_chunk + x;
};

let section_xyz_of_i = i => {
  let y = i / block_per_chunk / block_per_chunk;
  let i = i - y * block_per_chunk * block_per_chunk;
  let z = i / block_per_chunk;
  let i = i - z * block_per_chunk;
  let x = i;
  assert_xyz(x, y, z);
  (x, y, z);
};

let section_nbt = (section, sy) => {
  let block_ids = Array.map(Block.id, section.blocks);
  let block_data =
    Nibble_array.init(section_volume, i => {Block.data(section.blocks[i])});
  Nbt.Node.(
    ""
    >: Compound([
         "Blocks" >: make_byte_array(block_ids),
         "Data" >: make_byte_array(block_data), /* TODO add support for block data */
         "Y" >: Byte(sy),
         /*
           Luckily we don't need to calculate and include light levels.
           Minecraft will do it for us when fixing the chunk
          */
       ])
  );
};

let chunk_heightmap = (tree, cx, cz) => {
  Array.init(
    block_per_chunk * block_per_chunk,
    i => {
      let z = i / block_per_chunk;
      let i = i - z * block_per_chunk;
      let x = i;
      height_at(
        tree,
        ~x=cx * block_per_chunk + x,
        ~z=cz * block_per_chunk + z,
        (),
      )
      |> Int32.of_int;
    },
  );
};

let section_coords = (~rx, ~rz, ~cx, ~cz, ~sy) => {
  let min_x = rx * block_per_region + cx * block_per_chunk;
  let min_y = sy * block_per_chunk;
  let min_z = rz * block_per_region + cz * block_per_chunk;
  List.(
    init(
      block_per_chunk,
      y => {
        let y = min_y + y;
        init(
          block_per_chunk,
          z => {
            let z = min_z + z;
            init(
              block_per_chunk,
              x => {
                let x = min_x + x;
                (x, y, z);
              },
            );
          },
        )
        |> concat;
      },
    )
    |> concat
  );
};

let entity_nbt = entity => {
  let {id, x, y, z} = entity;
  Nbt.Node.(
    Compound([
      "id" >: String(id),
      "Pos" >: List([Double(x), Double(y), Double(z)]),
    ])
  );
};

let chunk_nbt = (tree, ~rx, ~rz, ~cx, ~cz) => {
  /* Save all non-empty sections */
  let sections =
    List.init(section_per_chunk, sy => (sy, get_section(tree, cx, sy, cz)));
  let sections_nbt =
    List.(
      sections
      |> filter(((_, section)) => section_has_blocks(section), _)
      |> map(((section_y, section)) =>
           section_nbt(section, section_y).payload
         )
    );

  let entities_nbt = find_entities(tree, ~cx, ~cz) |> List.map(entity_nbt);

  let heightmap = chunk_heightmap(tree, cx, cz);
  let global_cx = rx * chunk_per_region + cx;
  let global_cz = rz * chunk_per_region + cz;
  Nbt.Node.(
    ""
    >: Compound([
         "Level"
         >: Compound([
              "Sections" >: List(sections_nbt),
              "xPos" >: Int(global_cx |> Int32.of_int),
              "zPos" >: Int(global_cz |> Int32.of_int),
              "LastUpdate" >: Long(Util.time_ms()),
              "V" >: Byte(1),
              "LightPopulated" >: Byte(1),
              "TerrainPopulated" >: Byte(1),
              "HeightMap" >: make_int_array(heightmap),
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
let save_region = (~memory=create_memory(), region_path, tree) => {
  let {nbt_printer_memory} = memory;
  let {rx, rz, _} = tree;

  let region_file_path =
    Printf.sprintf("r.%d.%d.mca", rx, rz) |> Filename.concat(region_path, _);
  Util.write_file(
    region_file_path,
    f => {
      /* Write chunk sector offsets & lengths */
      /*
        First we'll write them all as zero, then we'll update the byte array as
        we serialize chunks. Finally at the end we'll rewind the file and write
        the updated bytes.
       */
      let chunk_offset_sizes =
        Bytes.make(4 * chunk_per_region * chunk_per_region, Char.chr(0));
      output_bytes(f, chunk_offset_sizes);

      /* Write modification times (repeat current time 32 * 32 times) */
      let now = Util.time_ms() |> Int64.to_int32(_);
      let now_bytes = Bytes.create(4);
      Bytes.set_int32_be(now_bytes, 0, now);
      for (_ in 0 to pred(chunk_per_region * chunk_per_region)) {
        output_bytes(f, now_bytes);
      };

      /* Write each chunk */
      for (cz in 0 to pred(chunk_per_region)) {
        for (cx in 0 to pred(chunk_per_region)) {
          let i = cz * chunk_per_region + cx;
          if (chunk_has_blocks(tree, cx, cz)) {
            /* Make sure we're at the start of a sector */
            assert(pos_out(f) mod sector_bytes == 0);

            /* Deflate chunk NBT. Keep chunk NBT and buffer in smaller scope to reduce memory, perhaps */
            let chunk_deflated = {
              let nbt = chunk_nbt(tree, ~rx, ~rz, ~cx, ~cz);
              Nbt.Nbt_printer.print_nbt(
                ~memory=nbt_printer_memory,
                ~gzip=false,
                nbt,
              );
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