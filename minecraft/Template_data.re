open Block;
open Template;

let trunk = height => {
  {blocks: List.init(height, y => (0, y, 0, Log))};
};

let leaves = {
  blocks: [
    (0, 0, 0, Log),
    (0, 1, 0, Log),
    (0, 2, 0, Log),
    (0, 0, (-1), Leaves),
    (1, 0, (-1), Leaves),
    (1, 0, 0, Leaves),
    (1, 0, 1, Leaves),
    (0, 0, 1, Leaves),
    ((-1), 0, 1, Leaves),
    ((-1), 0, 0, Leaves),
    ((-1), 0, (-1), Leaves),
    (0, 1, (-1), Leaves),
    (1, 1, (-1), Leaves),
    (1, 1, 0, Leaves),
    (1, 1, 1, Leaves),
    (0, 1, 1, Leaves),
    ((-1), 1, 1, Leaves),
    ((-1), 1, 0, Leaves),
    ((-1), 1, (-1), Leaves),
    (0, 2, (-1), Leaves),
    (1, 2, (-1), Leaves),
    (1, 2, 0, Leaves),
    (1, 2, 1, Leaves),
    (0, 2, 1, Leaves),
    ((-1), 2, 1, Leaves),
    ((-1), 2, 0, Leaves),
    ((-1), 2, (-1), Leaves),
    (0, 3, (-1), Leaves),
    (1, 3, (-1), Leaves),
    (1, 3, 0, Leaves),
    (1, 3, 1, Leaves),
    (0, 3, 1, Leaves),
    ((-1), 3, 1, Leaves),
    ((-1), 3, 0, Leaves),
    ((-1), 3, (-1), Leaves),
    (0, 3, 0, Leaves),
  ],
};

let tree = () => {
  let height = Random.int(5) + 3;
  stack(trunk(height), leaves);
};