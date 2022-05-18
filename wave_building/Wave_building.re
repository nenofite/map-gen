open! Core;

let palette = [
  (".", Minecraft.Block.Air),
  (",", Minecraft.Block.Dirt),
  ("O", Minecraft.Block.Log(Oak_log, Y)),
  ("#", Minecraft.Block.Oak_planks),
  ("D", Minecraft.Block.Oak_door(N, Upper)),
  ("d", Minecraft.Block.Oak_door(N, Lower)),
  ("D", Minecraft.Block.Oak_door(E, Upper)),
  ("d", Minecraft.Block.Oak_door(E, Lower)),
  ("D", Minecraft.Block.Oak_door(S, Upper)),
  ("d", Minecraft.Block.Oak_door(S, Lower)),
  ("D", Minecraft.Block.Oak_door(W, Upper)),
  ("d", Minecraft.Block.Oak_door(W, Lower)),
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

let rotate_symbol = symbol => {
  let (_, block) =
    List.find_exn(palette, ~f=((s, _)) => String.(s == symbol));
  let block_f = Minecraft.Block.rotate_cw(block, ~times=1);
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
      ~rotate_cw=rotate_symbol,
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
        // Underground
        tile(
          ~weight=0.0,
          [|
            [|
              [|",", ","|], /* */
              [|",", ","|] /* */
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
        // Stairs (N)
        tile(
          ~weight=0.01,
          ~rotate=true,
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|"#", "#"|], /* */
              [|"^", "^"|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|",", ","|], /* */
              [|"#", "#"|], /* */
              [|"#", "#"|] /* */
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
        // Walls to air
        tile(
          ~weight=0.0,
          ~rotate=true,
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|".", "."|], /* */
              [|"#", "#"|] /* */
            |],
          |],
        ),
        tile(
          ~weight=0.0,
          ~rotate=true,
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|".", "."|], /* */
              [|"O", "#"|] /* */
            |],
          |],
        ),
        // Corner post
        tile(
          ~weight=0.1,
          ~rotate=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", ".", "."|], /* */
              [|".", ".", "."|] /* */
            |],
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
          ~rotate=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", ".", "."|], /* */
              [|".", ".", "."|] /* */
            |],
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
          ~rotate=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", ".", "."|], /* */
              [|".", ".", "."|] /* */
            |],
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
          ~rotate=true,
          [|
            [|
              [|".", ".", "."|], /* */
              [|".", ".", "."|], /* */
              [|".", ".", "."|] /* */
            |],
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
          ~rotate=true,
          [|
            [|
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", ".", ".", ".", ".", ".", "."|], /* */
              [|".", ".", ".", ".", ".", ".", "."|] /* */
            |],
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
          let t =
            switch (Wave_collapse.item_or_entropy_at(eval, ~x, ~y, ~z)) {
            | Ok(s) => s
            | Error(e) => Int.to_string(e)
            };
          Printf.printf("%s ", t);
        };
        Out_channel.newline(stdout);
      };
      Out_channel.newline(stdout);
    };
  };
};

let%expect_test "yay" = {
  let xs = 20;
  let zs = 20;
  let wave = Wave_collapse.make_blank_wave(ts, ~xs, ~ys=4, ~zs);
  for (x in 0 to xs - 1) {
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=0, 0);
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=zs - 1, 0);
  };
  for (z in 0 to zs - 1) {
    Wave_collapse.force_and_propagate(wave, ~x=0, ~y=0, ~z, 0);
    Wave_collapse.force_and_propagate(wave, ~x=xs - 1, ~y=0, ~z, 0);
  };
  // try(Wave_collapse.force_edges(~x0=0, ~x1=0, ~z0=0, ~z1=0, wave)) {
  // | Wave_collapse.Contradiction(_) => ()
  // };
  // Test_helpers.print_items(wave);
  Test_helpers.print_items(wave);
  Wave_collapse.collapse_all(wave);
  Test_helpers.print_items(wave);
  %expect
  {||};
};