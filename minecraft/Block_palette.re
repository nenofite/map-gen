/**
 * provides a data structure and helper functions to construct a palette of
 * materials and pack a section's blocks into a long array using said palette.
 */

type lookup = list((Block.material, int));

type packer = {
  lookup,
  block_states: list(int64),
  block_states_len: int,
  bits_per_block: int,
  blocks_per_long: int,
};

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
    Mg_util.Floats.(~~ceil(log(~.List.length(lookup)) /. log(2.)));
  let blocks_per_long = 64 / bits_per_block;
  {
    lookup,
    block_states: [],
    block_states_len: 0,
    bits_per_block,
    blocks_per_long,
  };
};

let palette = (lookup: lookup) => List.map(((block, _i)) => block, lookup);

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

let%expect_test "block packing" = {
  let lookup =
    empty_lookup
    |> construct_lookup(Block.Air)
    |> construct_lookup(Block.Grass)
    |> construct_lookup(Block.Stone)
    |> construct_lookup(Block.Flowing_water(0))
    |> construct_lookup(Block.Flowing_water(1))
    |> construct_lookup(Block.Flowing_water(2))
    |> construct_lookup(Block.Flowing_water(3));
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

  Printf.printf("Length %d\n", List.length(longs));
  Printf.printf("[0] %xd\n", longs.(0));
  Printf.printf("[1] %xd\n", longs.(1));

  %expect {|
    Length 2
    [0] 
|};
};