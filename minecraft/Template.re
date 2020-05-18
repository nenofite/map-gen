/**
  t is a templated structure, tree, etc. that can be pasted into the block
  tree
 */
type t = {blocks: list((int, int, int, Block.material))};

/**
  combine puts addition into base. addition has the given offset from base.

  No collision checking is performed. If two blocks have the same coords,
  addition will overwrite base.
 */
let combine = (base, addition, x, y, z) => {
  /* Apply offset to addition's coords */
  let addition_blocks =
    List.map(
      ((dx, dy, dz, material)) => (x + dx, y + dy, z + dz, material),
      addition.blocks,
    );
  /* Then concat the lists */
  {blocks: base.blocks @ addition_blocks};
};

/**
  stack is a convenience over combine. It finds the highest block in base,
  then puts addition one above that.
 */
let stack = (base, addition) => {
  let highest_y =
    List.fold_left((max_y, (_, y, _, _)) => max(y, max_y), 0, base.blocks);
  combine(base, addition, 0, highest_y + 1, 0);
};

/**
  check_collision is true if the template would hit a non-air block or go out
  of the region
 */
let check_collision = (template, tree, x, y, z) => {
  List.exists(
    ((dx, dy, dz, _)) => {
      switch (Block_tree.get_block_opt(tree, x + dx, y + dy, z + dz)) {
      | Some(Air) => false
      | None => true
      | Some(_not_air) => true
      }
    },
    template.blocks,
  );
};

/**
  place attempts to paste a template at the given coordinate. If any of the
  blocks are not air or are outside the region, it fails and returns false
 */
let place = (template, tree, x, y, z) =>
  if (!check_collision(template, tree, x, y, z)) {
    List.iter(
      ((dx, dy, dz, material)) => {
        Block_tree.set_block(tree, dx + x, dy + y, dz + z, material)
      },
      template.blocks,
    );
    true;
  } else {
    false;
  };