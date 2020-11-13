open Core_kernel;

/** stores blocks in [[z][x][y]] order */
type section = array(Block.material);

type t = {
  mutable rx: int,
  mutable rz: int,
  /* dimensions are z x y */
  sections: array(section),
  mutable entities: list(Entity.t),
};

/*
 Coord naming standards used internally within this module:

 - x,y,z block global coords
 - lx,ly,lz block region-local coords
 - si section index within region
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

let create = (~rx, ~rz) => {
  let sections =
    Array.init(section_per_region_volume, ~f=_i =>
      Array.create(~len=block_per_section_volume, Block.Air)
    );
  {rx, rz, sections, entities: []};
};

let reset = (~rx, ~rz, r) => {
  Array.iter(r.sections, ~f=s => {
    Array.fill(s, ~pos=0, ~len=Array.length(s), Block.Air)
  });
  r.rx = rx;
  r.rz = rz;
  r.entities = [];
  ();
};

let local_of_region_coords = (~x, ~y, ~z, r) => {
  let x = x - r.rx * block_per_region_side;
  let z = z - r.rz * block_per_region_side;
  (x, y, z);
};

let is_within = (~x, ~y, ~z, r) => {
  let (lx, ly, lz) = local_of_region_coords(~x, ~y, ~z, r);
  0 <= lx
  && lx < block_per_region_side
  && 0 <= ly
  && ly < block_per_region_vertical
  && 0 <= lz
  && lz < block_per_region_side;
};

let assert_and_localize = (~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    local_of_region_coords(~x, ~y, ~z, r);
  } else {
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

/** converts a local coord to indices within sections and section */
let section_i_block_i = (~lx, ~ly, ~lz) => {
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
  if (!(0 <= si && si < section_per_region_volume)) {
    Tale.logf("%d %d %d", lx, ly, lz);
  };
  let bx = lx - cx * block_per_chunk_side;
  let by = ly - sy * block_per_section_vertical;
  let bz = lz - cz * block_per_chunk_side;
  let bi =
    bz
    * block_per_chunk_side
    * block_per_section_vertical
    + bx
    * block_per_section_vertical
    + by;
  assert(0 <= bi && bi < block_per_section_volume);
  (si, bi);
};

let set_block = (material, ~x, ~y, ~z, r) => {
  let (lx, ly, lz) = assert_and_localize(~x, ~y, ~z, r);
  let (si, bi) = section_i_block_i(~lx, ~ly, ~lz);
  r.sections[si][bi] = material;
};

let set_block_opt = (material, ~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    set_block(material, ~x, ~y, ~z, r);
  };

let get_block = (~x, ~y, ~z, r) => {
  let (lx, ly, lz) = assert_and_localize(~x, ~y, ~z, r);
  let (si, bi) = section_i_block_i(~lx, ~ly, ~lz);
  r.sections[si][bi];
};

let get_block_opt = (~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    Some(get_block(~x, ~y, ~z, r));
  } else {
    None;
  };

/* Entities */

/** adds the entity to the region in-place */
let add_entity = (entity: Entity.t, r) => {
  open Mg_util.Floats;
  let Entity.{x, y, z, _} = entity;
  let x = ~~x;
  let y = ~~y;
  let z = ~~z;
  ignore(assert_and_localize(~x, ~y, ~z, r));
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