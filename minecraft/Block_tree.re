/** section is 16x16x16 blocks. Blocks are stored in [[x][z][y]] order. */
type section = {blocks: array(array(array(Block.material)))};

/** chunk is 16 sections stacked vertically */
type chunk = {sections: array(section)};

/** region is 32x32 chunks */
type region = {chunks: array(array(chunk))};

type t = {
  regions: Hashtbl.t((int, int), region),
  generator: Generator.t,
};

let chunk_side = 16;
let section_volume = chunk_side * chunk_side * chunk_side;
let chunk_sections = 16;
let region_side = 32;

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
    Nibble_array.init(
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
  /* 4 bits per block in the section, all zeros */
  let zeros = Array.make(chunk_side * chunk_side * chunk_side / 2, 0);
  Nbt.Node.(
    ""
    >: Compound([
         "Blocks" >: make_byte_array(block_ids),
         "Data" >: make_byte_array(block_data),
         "BlockLight" >: make_byte_array(zeros),
         "SkyLight" >: make_byte_array(zeros), /* TODO once we calculate sky light */
         "Y" >: Byte(section_y),
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
  let sections =
    Array.mapi(
      (section_y, section) => section_nbt(section, section_y).payload,
      chunk.sections,
    )
    |> Array.to_list;
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
              "LightPopulated" >: Byte(0), /* maybe by leaving this as 0, Minecraft will calculate for us? */
              "TerrainPopulated" >: Byte(1),
              "HeightMap" >: make_int_array(heightmap),
            ]),
       ])
  );
};

/** save_region writes the given region to storage */
let save_region = (region_path, tree, rx, rz) => {
  let _region = get_region(tree, rx, rz);
  let region_file_path =
    Printf.sprintf("r.%d.%d.mca", rx, rz) |> Filename.concat(region_path, _);
  Utils.write_file(region_file_path, f => {output_string(f, "todo :(\n")});
};