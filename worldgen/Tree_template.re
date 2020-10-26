open Minecraft.Block;

let trunk = height => {
  let blocks = List.init(height, y => (0, y, 0, Oak_log(Y)));
  Minecraft_template.of_blocks(blocks);
};

let leaves = {
  /* TODO move to mapgen and use template parser */
  let blocks = [
    (0, 0, 0, Oak_log(Y)),
    (0, 1, 0, Oak_log(Y)),
    (0, 2, 0, Oak_log(Y)),
    (0, 0, (-1), Oak_leaves),
    (1, 0, (-1), Oak_leaves),
    (1, 0, 0, Oak_leaves),
    (1, 0, 1, Oak_leaves),
    (0, 0, 1, Oak_leaves),
    ((-1), 0, 1, Oak_leaves),
    ((-1), 0, 0, Oak_leaves),
    ((-1), 0, (-1), Oak_leaves),
    (0, 1, (-1), Oak_leaves),
    (1, 1, (-1), Oak_leaves),
    (1, 1, 0, Oak_leaves),
    (1, 1, 1, Oak_leaves),
    (0, 1, 1, Oak_leaves),
    ((-1), 1, 1, Oak_leaves),
    ((-1), 1, 0, Oak_leaves),
    ((-1), 1, (-1), Oak_leaves),
    (0, 2, (-1), Oak_leaves),
    (1, 2, (-1), Oak_leaves),
    (1, 2, 0, Oak_leaves),
    (1, 2, 1, Oak_leaves),
    (0, 2, 1, Oak_leaves),
    ((-1), 2, 1, Oak_leaves),
    ((-1), 2, 0, Oak_leaves),
    ((-1), 2, (-1), Oak_leaves),
    (0, 3, (-1), Oak_leaves),
    (1, 3, (-1), Oak_leaves),
    (1, 3, 0, Oak_leaves),
    (1, 3, 1, Oak_leaves),
    (0, 3, 1, Oak_leaves),
    ((-1), 3, 1, Oak_leaves),
    ((-1), 3, 0, Oak_leaves),
    ((-1), 3, (-1), Oak_leaves),
    (0, 3, 0, Oak_leaves),
  ];
  Minecraft_template.of_blocks(blocks);
};

let tree = () => {
  let height = Random.int(5) + 3;
  Minecraft_template.stack(trunk(height), leaves);
};