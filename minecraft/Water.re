/**
  max_level is the maximum data that can be attached to water. It also
  determines the maximum distance that water will flow horizontally.
 */
let max_level = 7;

let non_empty =
  fun
  | [] => false
  | _ => true;

let rec next_in_queue' = (array, i) =>
  if (i < Array.length(array)) {
    switch (array[i]) {
    | [next, ...rest] =>
      array[i] = rest;
      Some(next);
    | [] => next_in_queue'(array, i + 1)
    };
  } else {
    None;
  };
let next_in_queue = next_in_queue'(_, 0);

let flow_to_block = (region, need_updates, x, y, z, level) => {
  switch (Block_tree.get_block_opt(region, x, y, z)) {
  | Some(Flowing_water(old_level)) when old_level <= level => ()
  | Some(Flowing_water(_))
  | Some(Air) =>
    Block_tree.set_block(region, x, y, z, Flowing_water(level));
    need_updates[level] = [(x, y, z, level), ...need_updates[level]];
  | Some(_)
  | None => ()
  };
};

let on_solid_ground = (region, x, y, z) => {
  switch (Block_tree.get_block_opt(region, x, y - 1, z)) {
  /* Negative cases: air, end of world, or any flowing water */
  | Some(Air)
  | Some(Flowing_water(_))
  | None => false
  /* Any other block is solid */
  | Some(_) => true
  };
};

let flow_single_water = (region, need_updates, should_be_level, x, y, z) => {
  switch (Block_tree.get_block(region, x, y, z)) {
  | Flowing_water(here_level)
      when here_level == should_be_level && here_level < max_level =>
    /* If below is air or flowing water, try setting with level = 1 */
    flow_to_block(region, need_updates, x, y - 1, z, 1);
    /*
      If here can spread horizontally, and NESW are air or flowing water, try
      setting with level = here + 1
     */
    if (here_level == 0 || on_solid_ground(region, x, y, z)) {
      let horiz_level = here_level + 1;
      flow_to_block(region, need_updates, x + 1, y, z, horiz_level);
      flow_to_block(region, need_updates, x - 1, y, z, horiz_level);
      flow_to_block(region, need_updates, x, y, z + 1, horiz_level);
      flow_to_block(region, need_updates, x, y, z - 1, horiz_level);
    };
  | _ => ()
  };
};

let rec flow_water_loop = (region, need_updates) => {
  switch (next_in_queue(need_updates)) {
  | Some((x, y, z, level)) =>
    flow_single_water(region, need_updates, level, x, y, z);
    flow_water_loop(region, need_updates);
  | None => ()
  };
};

/**
  flow_water updates a region so that water flows as necessary. Minecraft water follows these rules:

  - Water sources have value 0
  - Each tick, all blocks that are below any water take a value of 1.
  - Each tick, all blocks that are cardinally adjacent to water with value =
    0 or with solid ground beneath them adopt its value + 1,* unless their own value
    is >= that.
  - Value must be <= max_level

  This only considers Flowing_water. Water (the stationary kind) is ignored.
 */
let flow_water = region => {
  /*
    Cheap priority queue, since we know all values are between 0 and max_level.

    Each index contains the list of blocks with that value that need updating.
   */
  let need_updates = Array.make(max_level + 1, []);

  /* First, populate need_updates[0] with all sources */
  for (y in 0 to pred(Block_tree.block_per_region_vertical)) {
    for (z in 0 to pred(Block_tree.block_per_region)) {
      for (x in 0 to pred(Block_tree.block_per_region)) {
        switch (Block_tree.get_block(region, x, y, z)) {
        | Flowing_water(0) =>
          need_updates[0] = [(x, y, z, 0), ...need_updates[0]]
        | _ => ()
        };
      };
    };
  };
  /* Starting with value 0 and moving up, update the next block until all are update */
  flow_water_loop(region, need_updates);
};