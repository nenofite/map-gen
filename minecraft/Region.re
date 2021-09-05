open Core_kernel;

/** stores blocks in [[z][x][y]] order */
type section = array(Block.material);

type t = {
  mutable rx: int,
  mutable rz: int,
  /* dimensions are z x y */
  sections: array(section),
  mutable entities: list(Entity.t),
  biomes: array(array(Biome.t)),
};

/*
 Coord naming standards used internally within this module:

 - x,y,z block global coords
 - lx,ly,lz block region-local coords
 - ci chunk index within region.biomes
 - si section index within region.sections
 - cx, sy, cz chunk and section region-local coords
 - bi block index within section
 - bx, by, bz block section-local coords
 */

let block_per_chunk_side = 16;
let block_per_section_vertical = 16;
let block_per_section_volume =
  block_per_chunk_side * block_per_chunk_side * block_per_section_vertical;
let section_per_chunk_vertical = 16;
let block_per_chunk_vertical =
  block_per_section_vertical * section_per_chunk_vertical;
let chunk_per_region_side = 32;
let block_per_region_side = chunk_per_region_side * block_per_chunk_side;
let block_per_region_vertical = block_per_chunk_vertical;
let section_per_region_volume =
  chunk_per_region_side * chunk_per_region_side * section_per_chunk_vertical;

let block_per_biome_side = 4;
let biome_per_chunk_side = 4;
let biome_per_chunk_vertical = block_per_chunk_vertical / block_per_biome_side;
let biome_per_chunk_volume = 1024;
let biome_per_region_side = biome_per_chunk_side * chunk_per_region_side;

let create = (~rx, ~rz) => {
  let sections =
    Array.init(section_per_region_volume, ~f=_i =>
      Array.create(~len=block_per_section_volume, Block.Air)
    );
  let biomes =
    Array.init(chunk_per_region_side * chunk_per_region_side, ~f=_i =>
      Array.create(~len=biome_per_chunk_volume, Biome.Forest)
    );
  {rx, rz, sections, entities: [], biomes};
};

let reset = (~rx, ~rz, r) => {
  Array.iter(r.sections, ~f=s => {
    Array.fill(s, ~pos=0, ~len=Array.length(s), Block.Air)
  });
  Array.iter(r.biomes, ~f=c =>
    Array.fill(c, ~pos=0, ~len=Array.length(c), Biome.Forest)
  );
  r.rx = rx;
  r.rz = rz;
  r.entities = [];
  ();
};

let get_region_coords = r => (r.rx, r.rz);

/**
 * translates a global coord to a region-local coord. These are split into three
 * separate functions because allocating the tuple was causing a surprising
 * amount of memory churn
 */
let localize_x = (x, r) => x - r.rx * block_per_region_side;
let localize_y = (y, _r) => y;
let localize_z = (z, r) => z - r.rz * block_per_region_side;

/**
 * same as localize_* but allocates a tuple. Generally better to use this unless
 * you're in a hot loop.
 */
let local_of_region_coords = (~x, ~y, ~z, r) => {
  (localize_x(x, r), localize_y(y, r), localize_z(z, r));
};

let is_within = (~x, ~y, ~z, r) => {
  let lx = localize_x(x, r);
  let ly = localize_y(y, r);
  let lz = localize_z(z, r);
  0 <= lx
  && lx < block_per_region_side
  && 0 <= ly
  && ly < block_per_region_vertical
  && 0 <= lz
  && lz < block_per_region_side;
};

let assert_within = (~x, ~y, ~z, r) =>
  if (!is_within(~x, ~y, ~z, r)) {
    let msg =
      Printf.sprintf(
        "coords outside of %d, %d region: %d, %d, %d",
        r.rx,
        r.rz,
        x,
        y,
        z,
      );
    raise(Invalid_argument(msg));
  };

/** calculates the rx, rz coordinates of the region which contains these global coordinates */
let region_containing = (~x, ~z) => {
  (
    x / block_per_region_side - (x < 0 ? 1 : 0),
    z / block_per_region_side - (z < 0 ? 1 : 0),
  );
};

/**
  checks whether the given bounding box is within the world and fits within a
  single region. In other words, it checks that the box is region-aligned, so
  it won't cause issues during the apply phase.
  */
let within_region_boundaries = (~canon_side, ~min_x, ~max_x, ~min_z, ~max_z) => {
  let x_side = max_x - min_x;
  let z_side = max_z - min_z;
  let within_world =
    0 <= min_x && max_x < canon_side && 0 <= min_z && max_z < canon_side;

  let within_region =
    min_x
    mod block_per_region_side < block_per_region_side
    - x_side
    && min_z
    mod block_per_region_side < block_per_region_side
    - z_side;

  within_world && within_region;
};

/** converts a chunk coord to an index of a biome chunk within biomes */
let chunk_i_of_c = (~cx, ~cz) => {
  let ci = cz * chunk_per_region_side + cx;
  ci;
};

/** converts a local coord to an index of a biome chunk within biomes */
let chunk_i_of_l = (~lx, ~lz) => {
  let cx = lx / block_per_chunk_side;
  let cz = lz / block_per_chunk_side;
  chunk_i_of_c(~cx, ~cz);
};

/** converts a local coord to an index within a biome chunk */
let biome_i_of_l = (~lx, ~ly, ~lz) => {
  let bmx = lx % block_per_chunk_side / block_per_biome_side;
  let bmy = ly / block_per_biome_side;
  let bmz = lz % block_per_chunk_side / block_per_biome_side;
  bmy
  * biome_per_chunk_side
  * biome_per_chunk_side
  + bmz
  * biome_per_chunk_side
  + bmx;
};

/** converts a local coord to an index of a section within sections */
let section_i = (~lx, ~ly, ~lz) => {
  let cx = lx / block_per_chunk_side;
  let sy = ly / block_per_section_vertical;
  let cz = lz / block_per_chunk_side;
  let si =
    cz
    * chunk_per_region_side
    * section_per_chunk_vertical
    + cx
    * section_per_chunk_vertical
    + sy;
  assert(0 <= si && si < section_per_region_volume);
  si;
};

/** converts a local coord to an index of a block within a section */
let block_i = (~lx, ~ly, ~lz) => {
  let bx = lx % block_per_chunk_side;
  let by = ly % block_per_section_vertical;
  let bz = lz % block_per_chunk_side;
  let bi =
    bz
    * block_per_chunk_side
    * block_per_section_vertical
    + bx
    * block_per_section_vertical
    + by;
  assert(0 <= bi && bi < block_per_section_volume);
  bi;
};

let set_block = (material, ~x, ~y, ~z, r) => {
  assert_within(~x, ~y, ~z, r);
  let lx = localize_x(x, r);
  let ly = localize_y(y, r);
  let lz = localize_z(z, r);
  let si = section_i(~lx, ~ly, ~lz);
  let bi = block_i(~lx, ~ly, ~lz);
  r.sections[si][bi] = material;
};

let set_block_opt = (material, ~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    set_block(material, ~x, ~y, ~z, r);
  };

let get_block = (~x, ~y, ~z, r) => {
  assert_within(~x, ~y, ~z, r);
  let lx = localize_x(x, r);
  let ly = localize_y(y, r);
  let lz = localize_z(z, r);
  let si = section_i(~lx, ~ly, ~lz);
  let bi = block_i(~lx, ~ly, ~lz);
  r.sections[si][bi];
};

let get_block_opt = (~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    Some(get_block(~x, ~y, ~z, r));
  } else {
    None;
  };

/* Biomes */

let biomes_in_xzy_order = (~cx, ~cz, r) => {
  let ci = chunk_i_of_c(~cx, ~cz);
  Array.to_list(r.biomes[ci]);
};

/** sets the biome of the 4x4x4 cube containing the given block */
let set_biome = (~x, ~y, ~z, biome, r) => {
  assert_within(~x, ~y, ~z, r);
  let lx = localize_x(x, r);
  let ly = localize_y(y, r);
  let lz = localize_z(z, r);
  let ci = chunk_i_of_l(~lx, ~lz);
  let bmi = biome_i_of_l(~lx, ~ly, ~lz);
  r.biomes[ci][bmi] = biome;
};

/** sets the full vertical column of biomes containing the given block */
let set_biome_column = (~x, ~z, biome, r) => {
  assert_within(~x, ~y=0, ~z, r);
  for (y in 0 to block_per_region_vertical - 1) {
    set_biome(~x, ~y, ~z, biome, r);
  };
};

/* Entities */

/** adds the entity to the region in-place */
let add_entity = (entity: Entity.t, r) => {
  open Mg_util.Floats;
  let Entity.{x, y, z, _} = entity;
  let x = ~~x;
  let y = ~~y;
  let z = ~~z;
  assert_within(~x, ~y, ~z, r);
  r.entities = [entity, ...r.entities];
};

/** returns all entities in this region */
let all_entities = r => r.entities;

/* Utils */

let rec highest_such_block = (~x, ~y, ~z, r, predicate) =>
  if (y > 0) {
    let here = get_block(~x, ~y, ~z, r);
    if (predicate(here)) {
      Some(y);
    } else {
      highest_such_block(~x, ~y=y - 1, ~z, r, predicate);
    };
  } else {
    None;
  };
let highest_such_block =
    (~x, ~y=block_per_region_vertical - 1, ~z, r, predicate) =>
  highest_such_block(~x, ~y, ~z, r, predicate);

let is_not_air =
  fun
  | Block.Air => false
  | _ => true;

/** height_at is the y-coord of the highest non-Air block */
let height_at = (~x, ~y=?, ~z, r) => {
  switch (highest_such_block(~x, ~y?, ~z, r, is_not_air)) {
  | Some(y) => y
  | None => 0
  };
};

/* Chunks and sections */

/** calculates the global offset in number of blocks of the region's min corner */
let region_offset = r => {
  (r.rx * block_per_region_side, r.rz * block_per_region_side);
};

/** calculates the global offset in number of blocks of the given chunk's min corner */
let chunk_offset = (~cx, ~cz, r) => {
  let z_off = cz * block_per_chunk_side + r.rz * block_per_region_side;
  let x_off = cx * block_per_chunk_side + r.rx * block_per_region_side;
  (x_off, z_off);
};

let section_offset = (~sy, _r) => {
  sy * block_per_section_vertical;
};

/* Iteration helpers */

let iter_region_xz = (~f, r) => {
  let (x_off, z_off) = chunk_offset(~cx=0, ~cz=0, r);
  for (z in 0 to pred(block_per_region_side)) {
    for (x in 0 to pred(block_per_region_side)) {
      f(~x=x + x_off, ~z=z + z_off);
    };
  };
};
