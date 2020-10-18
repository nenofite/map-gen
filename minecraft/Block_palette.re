/**
  provides a data structure and helper functions to construct a palette of
  materials and pack a section's blocks into a long array using said palette.
 */

type lookup = list((Block.material, int));

type packer = {
  lookup,
  block_states: list(int64),
  block_states_len: int,
  bits_per_block: int,
  blocks_per_long: int,
};

/** fewest bits to use per block index. This is determined by Minecraft, not us */
let min_bits_per_block = 4;

let empty_lookup: lookup = [];

let construct_lookup = (block: Block.material, lookup: lookup) =>
  if (!List.mem_assoc(block, lookup)) {
    let n = List.length(lookup);
    [(block, n), ...lookup];
  } else {
    lookup;
  };

let index_of_material = (block: Block.material, lookup: lookup) =>
  List.assoc(block, lookup);

let packer_of_lookup = (lookup: lookup) => {
  let bits_per_block =
    Mg_util.Floats.(~~ceil(log(~.List.length(lookup)) /. log(2.)))
    |> max(min_bits_per_block);
  let blocks_per_long = 64 / bits_per_block;
  {
    lookup,
    block_states: [],
    block_states_len: 0,
    bits_per_block,
    blocks_per_long,
  };
};

let palette = (lookup: lookup) =>
  List.rev_map(((block, _i)) => block, lookup);

let block_states = (packer: packer) => List.rev(packer.block_states);

let pack_block = (block: Block.material, packer: packer) => {
  let (block_states, long) =
    switch (packer.block_states) {
    | block_states when packer.block_states_len mod packer.blocks_per_long == 0 => (
        block_states,
        0L,
      )
    | [long, ...block_states] => (block_states, long)
    | [] =>
      raise(
        Invalid_argument(
          "packer block_states_len does not match block_states list",
        ),
      )
    };
  let bits_before =
    packer.block_states_len mod packer.blocks_per_long * packer.bits_per_block;
  let block_bits =
    Int64.(
      shift_left(
        of_int(index_of_material(block, packer.lookup)),
        bits_before,
      )
    );

  let block_states = [Int64.logor(long, block_bits), ...block_states];
  {...packer, block_states, block_states_len: packer.block_states_len + 1};
};

let utop_test_a = () => {
  /* 9 states, 4 bits per block */
  let lookup =
    empty_lookup
    |> construct_lookup(Block.Air)  /* 0x0 */
    |> construct_lookup(Block.Grass)  /* 0x1 */
    |> construct_lookup(Block.Stone)  /* 0x2 */
    |> construct_lookup(Block.Flowing_water(0))  /* 0x3 */
    |> construct_lookup(Block.Flowing_water(1))  /* 0x4 */
    |> construct_lookup(Block.Flowing_water(2))  /* 0x5 */
    |> construct_lookup(Block.Flowing_water(0))  /* already seen */
    |> construct_lookup(Block.Flowing_water(1))  /* already seen */
    |> construct_lookup(Block.Flowing_water(2))  /* already seen */
    |> construct_lookup(Block.Flowing_water(0))  /* already seen */
    |> construct_lookup(Block.Flowing_water(3))  /* 0x6 */
    |> construct_lookup(Block.Flowing_water(4))  /* 0x7 */
    |> construct_lookup(Block.Flowing_water(5)); /* 0x8 */
  /* 26 blocks */
  let packer =
    packer_of_lookup(lookup)
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1))
    |> pack_block(Block.Air)
    |> pack_block(Block.Flowing_water(1));
  let longs = block_states(packer);

  let b = Buffer.create(16);
  /* Printf.bprintf(b, "Length %d\n", List.length(longs)); */
  List.iteri((i, n) => Printf.bprintf(b, "[%d] 0x%016Lx\n", i, n), longs);

  let exp = "[0] 0x4040404040404040\n" ++ "[1] 0x0000004040404040\n";
  let obs = Buffer.contents(b);
  (obs, exp, obs == exp);
};

let utop_test_b = () => {
  /* 64 states, 6 bits per block */
  let lookup =
    empty_lookup
    |> construct_lookup(Block.Air)  /* 0x0 */
    |> construct_lookup(Block.Grass)  /* 0x1 */
    |> construct_lookup(Block.Stone)  /* 0x2 */
    |> Mg_util.Range.fold(3, 63, _, (lookup, i) =>
         construct_lookup(Block.Flowing_water(i), lookup)
       )  /* 3 to 63 */
    |> construct_lookup(Block.Air); /* already seen */
  /* 26 blocks */
  let packer =
    packer_of_lookup(lookup)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4))
    |> pack_block(Block.Grass)
    |> pack_block(Block.Flowing_water(4));
  let longs = block_states(packer);

  let b = Buffer.create(16);
  /* Printf.bprintf(b, "Length %d\n", List.length(longs)); */
  List.iteri((i, n) => Printf.bprintf(b, "[%d] 0o%022Lo\n", i, n), longs);

  let exp =
    "[0] 0o0004010401040104010401\n"
    ++ "[1] 0o0004010401040104010401\n"
    ++ "[2] 0o0000000000040104010401\n";
  let obs = Buffer.contents(b);
  (obs, exp, obs == exp);
};

let utop_test_c = () => {
  /* 1 state, 4 bits per block */
  let lookup = empty_lookup |> construct_lookup(Block.Grass); /* 0x0 */
  /* 26 blocks */
  let packer =
    packer_of_lookup(lookup)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass)
    |> pack_block(Block.Grass);
  let longs = block_states(packer);

  let b = Buffer.create(16);
  /* Printf.bprintf(b, "Length %d\n", List.length(longs)); */
  List.iteri((i, n) => Printf.bprintf(b, "[%d] 0x%016Lx\n", i, n), longs);

  let expected = "[0] 0x0000000000000000\n" ++ "[1] 0x0000000000000000\n";
  let obs = Buffer.contents(b);
  (obs, expected, obs == expected);
};