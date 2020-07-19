type t = {
  mutable rx: int,
  mutable rz: int,
  /* dimensions are z x y {material, data} */
  blocks:
    Bigarray.Genarray.t(int, Bigarray.int8_unsigned_elt, Bigarray.c_layout),
  mutable entities: list(Entity.t),
};

let block_per_chunk_side = 16;
let block_per_section_vertical = 16;
let section_volume =
  block_per_chunk_side * block_per_chunk_side * block_per_section_vertical;
let section_per_chunk_vertical = 16;
let block_per_chunk_vertical =
  block_per_section_vertical * section_per_chunk_vertical;
let chunk_per_region_side = 32;
let block_per_region_side = chunk_per_region_side * block_per_chunk_side;
let block_per_region_vertical =
  section_per_chunk_vertical * block_per_chunk_vertical;
let num_channels = 2;

let create = (~rx, ~rz) => {
  open Bigarray;
  let blocks =
    Genarray.create(
      Int8_unsigned,
      C_layout,
      [|
        block_per_region_side,
        block_per_region_side,
        block_per_region_vertical,
        num_channels,
      |],
    );
  {rx, rz, blocks, entities: []};
};

let reset = (~rx, ~rz, r) => {
  Bigarray.Genarray.fill(r.blocks, 0);
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
  let (x, y, z) = local_of_region_coords(~x, ~y, ~z, r);
  0 <= x
  && x < block_per_region_side
  && 0 <= y
  && y < block_per_region_vertical
  && 0 <= z
  && z < block_per_region_side;
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

let set_block = (material, ~x, ~y, ~z, r) => {
  let (x, y, z) = assert_and_localize(~x, ~y, ~z, r);
  (r.blocks).{z, x, y, 0} = Block.id(material);
  (r.blocks).{z, x, y, 1} = Block.data(material);
};

let set_block_opt = (material, ~x, ~y, ~z, r) =>
  if (is_within(~x, ~y, ~z, r)) {
    set_block(material, ~x, ~y, ~z, r);
  };

let get_block = (~x, ~y, ~z, r) => {
  let (x, y, z) = assert_and_localize(~x, ~y, ~z, r);
  let id = (r.blocks).{z, x, y, 0};
  let data = (r.blocks).{z, x, y, 1};
  Block.parse(id, data);
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
  open Floats;
  let Entity.{x, y, z, _} = entity;
  ignore(assert_and_localize(~x=~~x, ~y=~~y, ~z=~~z, r));
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
      highest_such_block(~x, ~y=y + 1, ~z, r, predicate);
    };
  } else {
    None;
  };
let highest_such_block =
    (~x, ~y=block_per_region_vertical - 1, ~z, r, predicate) =>
  highest_such_block(~x, ~y, ~z, r, predicate);

/** height_at is the y-coord of the highest non-Air block */
let height_at = (~x, ~y=?, ~z, r) => {
  switch (highest_such_block(~x, ~y?, ~z, r, b => b != Block.Air)) {
  | Some(y) => y
  | None => 0
  };
};

/* Chunks and sections */

let chunk_offset = (~cx, ~cz, r) => {
  let z_off = cz * block_per_chunk_side + r.rz + block_per_region_side;
  let x_off = cx * block_per_chunk_side + r.rx + block_per_region_side;
  (x_off, z_off);
};

let section_offset = (~sy, _r) => {
  sy * block_per_section_vertical;
};