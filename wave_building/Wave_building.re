open! Core_kernel;

let palette = [
  (".", Minecraft.Block.Air),
  (",", Minecraft.Block.Dirt),
  ("O", Minecraft.Block.Log(Oak_log, Y)),
  ("#", Minecraft.Block.Oak_planks),
  ("D", Minecraft.Block.Oak_door(N, Upper)),
  ("d", Minecraft.Block.Oak_door(N, Lower)),
  ("E", Minecraft.Block.Oak_door(S, Upper)),
  ("e", Minecraft.Block.Oak_door(S, Lower)),
  ("^", Minecraft.Block.Stairs(Oak_stairs, N)),
  (">", Minecraft.Block.Stairs(Oak_stairs, E)),
  ("v", Minecraft.Block.Stairs(Oak_stairs, S)),
  ("<", Minecraft.Block.Stairs(Oak_stairs, W)),
];

let flip = block => Minecraft.Block.rotate_cw(~times=2, block);

let flip_symbol = symbol => {
  let (_, block) =
    List.find_exn(palette, ~f=((s, _)) => String.(s == symbol));
  let block_f = flip(block);
  let (symbol_f, _) =
    List.find_exn(palette, ~f=((_, b)) =>
      Minecraft.Block.equal_material(b, block_f)
    );
  symbol_f;
};

let ts =
  Wave_collapse.(
    create_tileset(
      ~tilesize=2,
      ~flip_x=flip_symbol,
      ~flip_z=flip_symbol,
      [
        // Empty dirt
        tile(
          ~weight=0.01,
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|",", ","|], /* */
              [|",", ","|] /* */
            |],
          |],
        ),
        // Empty air
        tile(
          ~weight=0.01,
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
          |],
        ),
        // Empty interior
        tile([|
          [|
            [|".", "."|], /* */
            [|".", "."|] /* */
          |],
          [|
            [|"#", "#"|], /* */
            [|"#", "#"|] /* */
          |],
        |]),
        // Corner post
        tile(
          ~weight=0.1,
          ~flip_x=true,
          ~flip_z=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", "O", "#"|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|".", ".", "."|], /* */
              [|".", "O", "#"|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|",", ",", ","|], /* */
              [|",", "O", "#"|], /* */
              [|",", "#", "#"|] /* */
            |],
          |],
        ),
        // Inverted corner post
        tile(
          ~weight=0.025,
          ~flip_x=true,
          ~flip_z=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", "O", "#"|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|".", ".", "."|], /* */
              [|".", "O", "#"|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|"#", "#", "#"|], /* */
              [|"#", "O", "#"|], /* */
              [|"#", "#", ","|] /* */
            |],
          |],
        ),
        // Straight wall (X)
        tile(
          ~flip_z=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|"#", "#", "#"|], /* */
              [|".", ".", "."|] /* */
            |],
            [|
              [|".", ".", "."|], /* */
              [|"#", "#", "#"|], /* */
              [|".", ".", "."|] /* */
            |],
            [|
              [|",", ",", ","|], /* */
              [|"#", "#", "#"|], /* */
              [|"#", "#", "#"|] /* */
            |],
          |],
        ),
        // Straight wall with door (X)
        tile(
          ~weight=0.1,
          ~flip_z=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|"#", "D", "#"|], /* */
              [|".", ".", "."|] /* */
            |],
            [|
              [|".", ".", "."|], /* */
              [|"#", "d", "#"|], /* */
              [|".", ".", "."|] /* */
            |],
            [|
              [|",", ",", ","|], /* */
              [|"#", "#", "#"|], /* */
              [|"#", "#", "#"|] /* */
            |],
          |],
        ),
        // Ornate entrance (X)
        tile(
          ~flip_z=true,
          [|
            [|
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", "O", ".", ".", ".", "O", "."|], /* */
              [|".", "#", ".", ".", ".", "#", "."|], /* */
              [|"#", "O", "#", "D", "#", "O", "#"|], /* */
              [|".", ".", ".", ".", ".", ".", "."|] /* */
            |],
            [|
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", "O", ".", ".", ".", "O", "."|], /* */
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|"#", "O", "#", "d", "#", "O", "#"|], /* */
              [|".", ".", ".", ".", ".", ".", "."|] /* */
            |],
            [|
              [|",", ",", ",", ",", ",", ",", ","|], /* */
              [|",", "O", "#", "#", "#", "O", ","|], /* */
              [|",", "#", "#", "#", "#", "#", ","|], /* */
              [|"#", "O", "#", "#", "#", "O", "#"|], /* */
              [|"#", "#", "#", "#", "#", "#", "#"|] /* */
            |],
          |],
        ),
        // Straight wall (Z)
        tile(
          ~flip_x=true,
          [|
            [|
              [|".", "#", "."|], /* */
              [|".", "#", "."|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|".", "#", "."|], /* */
              [|".", "#", "."|], /* */
              [|".", "#", "."|] /* */
            |],
            [|
              [|",", "#", "#"|], /* */
              [|",", "#", "#"|], /* */
              [|",", "#", "#"|] /* */
            |],
          |],
        ),
      ],
    )
  );

module Test_helpers = {
  let print_items = (eval: Wave_collapse.wave_evaluator(string)) => {
    let ts = eval.tileset.tilesize;
    let xs = eval.xs * (ts - 1) + 1;
    let ys = eval.ys * (ts - 1) + 1;
    let zs = eval.zs * (ts - 1) + 1;
    for (y in ys - 1 downto 0) {
      for (z in 0 to zs - 1) {
        for (x in 0 to xs - 1) {
          let t = Wave_collapse.item_at(eval, ~x, ~y, ~z, ~default="?");
          Printf.printf("%s ", t);
        };
        Out_channel.newline(stdout);
      };
      Out_channel.newline(stdout);
    };
  };
};

let%expect_test "yay" = {
  let wave = Wave_collapse.make_blank_wave(ts, ~xs=20, ~ys=2, ~zs=20);
  for (x in 0 to 19) {
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=0, 0);
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=19, 0);
  };
  for (z in 0 to 19) {
    Wave_collapse.force_and_propagate(wave, ~x=0, ~y=0, ~z, 0);
    Wave_collapse.force_and_propagate(wave, ~x=19, ~y=0, ~z, 0);
  };
  // Wave_collapse.collapse_all(wave);
  // Test_helpers.print_items(wave);
  while (Wave_collapse.try_collapse_next_lowest_entropy(wave)) {
    Test_helpers.print_items(wave);
  };
  %expect
  {|
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # # # # # # # O . . . O # # # O . .
    . . # . . . . . . . # . . . # . . . # . .
    . . # . . O # O . . # . O # O . . . # . .
    . . # . . # . # . . # . # . . . . . # . .
    . . # . . # . # . . # . # . O # O . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . O # O . .
    . . # . . # . # . . # . # . # . . . . . .
    . . # . . # . # . . # . # . O # O . . . .
    . . # . . # . # . . # . # . . . # . . . .
    . . # . . # . # . . # . # . . . O # O . .
    . . # . . # . # . . # . # . . . . . # . .
    . . # . . O # O . . # . # . O # O . # . .
    . . # . . . . . . . # . # . # . # . # . .
    . . O # # # # # # # O . O # O . O # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # # # # # # # O . . . O # # # O . .
    . . # . . . . . . . # . . . # . . . # . .
    . . # . . O # O . . # . O # O . . . # . .
    . . # . . # . # . . # . # . . . . . # . .
    . . # . . # . # . . # . # . O # O . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . # . # . .
    . . # . . # . # . . # . # . # . O # O . .
    . . # . . # . # . . # . # . # . . . . . .
    . . # . . # . # . . # . # . O # O . . . .
    . . # . . # . # . . # . # . . . # . . . .
    . . # . . # . # . . # . # . . . O # O . .
    . . # . . # . # . . # . # . . . . . # . .
    . . # . . O # O . . # . # . O # O . # . .
    . . # . . . . . . . # . # . # . # . # . .
    . . O # # # # # # # O . O # O . O # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , O # # # # # # # O , , , O # # # O , ,
    , , # # # # # # # # # , , , # # # # # , ,
    , , # # # O # O # # # , O # O # # # # , ,
    , , # # # # , # # # # , # # # # # # # , ,
    , , # # # # , # # # # , # # O # O # # , ,
    , , # # # # , # # # # , # # # , # # # , ,
    , , # # # # , # # # # , # # # , # # # , ,
    , , # # # # , # # # # , # # # , # # # , ,
    , , # # # # , # # # # , # # # , O # O , ,
    , , # # # # , # # # # , # # # , , , , , ,
    , , # # # # , # # # # , # # O # O , , , ,
    , , # # # # , # # # # , # # # # # , , , ,
    , , # # # # , # # # # , # # # # O # O , ,
    , , # # # # , # # # # , # # # # # # # , ,
    , , # # # O # O # # # , # # O # O # # , ,
    , , # # # # # # # # # , # # # , # # # , ,
    , , O # # # # # # # O , O # O , O # O , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
  |};
};