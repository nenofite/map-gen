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
          ~name="dirt",
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
          ~name="underground",
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
          ~name="air",
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
        // Stairs (N) TODO
        // tile(
        //   ~name="stairs",
        //   ~weight=0.01,
        //   ~rotate=true,
        //   [|
        //     [|
        //       [|".", "."|], /* */
        //       [|".", "."|], /* */
        //       [|".", "."|] /* */
        //     |],
        //     [|
        //       [|"#", "#"|], /* */
        //       [|"^", "^"|], /* */
        //       [|".", "."|] /* */
        //     |],
        //     [|
        //       [|",", ","|], /* */
        //       [|"#", "#"|], /* */
        //       [|"#", "#"|] /* */
        //     |],
        //   |],
        // ),
        // Empty interior
        tile(
          ~name="interior",
          [|
            [|
              [|".", "."|], /* */
              [|".", "."|] /* */
            |],
            [|
              [|"#", "#"|], /* */
              [|"#", "#"|] /* */
            |],
          |],
        ),
        tile(
          ~name="int2u",
          [|
            [|
              [|"#", "#"|], /* */
              [|"#", "#"|] /* */
            |],
            [|
              [|",", ","|], /* */
              [|",", ","|] /* */
            |],
          |],
        ),
        // Walls to air
        tile(
          ~name="w2a",
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
          ~name="w2u",
          ~weight=0.0,
          ~rotate=true,
          [|
            [|
              [|".", "."|], /* */
              [|"#", "#"|] /* */
            |],
            [|
              [|",", ","|], /* */
              [|",", ","|] /* */
            |],
          |],
        ),
        tile(
          ~name="post2a",
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
        tile(
          ~name="post2u",
          ~weight=0.0,
          ~rotate=true,
          [|
            [|
              [|".", "."|], /* */
              [|"O", "#"|] /* */
            |],
            [|
              [|",", ","|], /* */
              [|",", ","|] /* */
            |],
          |],
        ),
        // Corner post
        tile(
          ~name="corner",
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
          ~name="invcorner",
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
          ~name="wall",
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
          ~name="door",
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
          ~name="entrance",
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

let%expect_test "tileset" = {
  Wave_collapse.Test_helpers.dump_tileset(
    ~only_impossible=true,
    ~show_item=Fn.id,
    ts,
  );
  %expect
  {|
    Tile post2a.r3: IMPOSSIBLE!
      . .
      . .

      . #
      . O

      air w2a.r1 post2a.r1 corner.r2.8 corner.r2.9 corner.r2.10 corner.r2.11 corner.r1.8 corner.r1.9 corner.r1.10 corner.r1.11 invcorner.r2.8 invcorner.r2.9 invcorner.r2.10 invcorner.r2.11 invcorner.r1.8 invcorner.r1.9 invcorner.r1.10 invcorner.r1.11 wall.r3.8 wall.r3.9 wall.r3.10 wall.r3.11 wall.r1.8 wall.r1.9 wall.r1.10 wall.r1.11 door.r3.8 door.r3.9 door.r3.10 door.r3.11 door.r1.8 door.r1.9 door.r1.10 door.r1.11 entrance.r3.60 entrance.r3.61 entrance.r3.62 entrance.r3.63 entrance.r3.64 entrance.r3.65 entrance.r3.66 entrance.r3.67 entrance.r3.68 entrance.r3.69 entrance.r3.70 entrance.r3.71 entrance.r2.66 entrance.r2.67 entrance.r2.70 entrance.r2.71 entrance.r1.60 entrance.r1.61 entrance.r1.62 entrance.r1.63 entrance.r1.64 entrance.r1.65 entrance.r1.66 entrance.r1.67 entrance.r1.68 entrance.r1.69 entrance.r1.70 entrance.r1.71 entrance.64 entrance.65 entrance.68 entrance.69 -X+
      post2u.r3 -Y+ air
      w2a.r3 corner.r1.5 corner.5 invcorner.r1.5 invcorner.5 wall.r3.5 wall.r1.5 door.r3.5 door.r1.5 entrance.r3.53 entrance.r1.17 -Z+
    ----------
    Tile post2a.r2: IMPOSSIBLE!
      . .
      . .

      # O
      . .

      w2a.r2 corner.r3.11 corner.11 invcorner.r3.11 invcorner.11 wall.r2.11 wall.11 door.r2.11 door.11 entrance.r2.69 entrance.71 -X+
      post2u.r2 -Y+ air
      -Z+ air w2a post2a corner.r1.2 corner.r1.4 corner.r1.8 corner.r1.10 corner.2 corner.4 corner.8 corner.10 invcorner.r1.2 invcorner.r1.4 invcorner.r1.8 invcorner.r1.10 invcorner.2 invcorner.4 invcorner.8 invcorner.10 wall.r2.2 wall.r2.4 wall.r2.8 wall.r2.10 wall.2 wall.4 wall.8 wall.10 door.r2.2 door.r2.4 door.r2.8 door.r2.10 door.2 door.4 door.8 door.10 entrance.r3.6 entrance.r3.12 entrance.r3.24 entrance.r3.30 entrance.r2.4 entrance.r2.8 entrance.r2.16 entrance.r2.20 entrance.r2.28 entrance.r2.32 entrance.r2.40 entrance.r2.44 entrance.r2.52 entrance.r2.56 entrance.r2.64 entrance.r2.68 entrance.r1.42 entrance.r1.48 entrance.r1.60 entrance.r1.66 entrance.4 entrance.8 entrance.16 entrance.20 entrance.28 entrance.32 entrance.40 entrance.44 entrance.52 entrance.56 entrance.64 entrance.68
    ----------
    Tile post2a.r1: IMPOSSIBLE!
      . .
      . .

      O .
      # .

      -X+ air w2a.r3 post2a.r3 corner.r3.2 corner.r3.3 corner.r3.4 corner.r3.5 corner.2 corner.3 corner.4 corner.5 invcorner.r3.2 invcorner.r3.3 invcorner.r3.4 invcorner.r3.5 invcorner.2 invcorner.3 invcorner.4 invcorner.5 wall.r3.2 wall.r3.3 wall.r3.4 wall.r3.5 wall.r1.2 wall.r1.3 wall.r1.4 wall.r1.5 door.r3.2 door.r3.3 door.r3.4 door.r3.5 door.r1.2 door.r1.3 door.r1.4 door.r1.5 entrance.r3.6 entrance.r3.7 entrance.r3.8 entrance.r3.9 entrance.r3.10 entrance.r3.11 entrance.r3.12 entrance.r3.13 entrance.r3.14 entrance.r3.15 entrance.r3.16 entrance.r3.17 entrance.r2.6 entrance.r2.7 entrance.r2.10 entrance.r2.11 entrance.r1.6 entrance.r1.7 entrance.r1.8 entrance.r1.9 entrance.r1.10 entrance.r1.11 entrance.r1.12 entrance.r1.13 entrance.r1.14 entrance.r1.15 entrance.r1.16 entrance.r1.17 entrance.4 entrance.5 entrance.8 entrance.9
      post2u.r1 -Y+ air
      -Z+ w2a.r1 corner.r3.10 corner.r2.10 invcorner.r3.10 invcorner.r2.10 wall.r3.10 wall.r1.10 door.r3.10 door.r1.10 entrance.r3.66 entrance.r1.30
    ----------
    Tile post2a: IMPOSSIBLE!
      . .
      . .

      . .
      O #

      -X+ w2a corner.r2.4 corner.r1.4 invcorner.r2.4 invcorner.r1.4 wall.r2.4 wall.4 door.r2.4 door.4 entrance.r2.8 entrance.10
      post2u -Y+ air
      air w2a.r2 post2a.r2 corner.r3.3 corner.r3.5 corner.r3.9 corner.r3.11 corner.r2.3 corner.r2.5 corner.r2.9 corner.r2.11 invcorner.r3.3 invcorner.r3.5 invcorner.r3.9 invcorner.r3.11 invcorner.r2.3 invcorner.r2.5 invcorner.r2.9 invcorner.r2.11 wall.r2.3 wall.r2.5 wall.r2.9 wall.r2.11 wall.3 wall.5 wall.9 wall.11 door.r2.3 door.r2.5 door.r2.9 door.r2.11 door.3 door.5 door.9 door.11 entrance.r3.11 entrance.r3.17 entrance.r3.29 entrance.r3.35 entrance.r2.7 entrance.r2.11 entrance.r2.19 entrance.r2.23 entrance.r2.31 entrance.r2.35 entrance.r2.43 entrance.r2.47 entrance.r2.55 entrance.r2.59 entrance.r2.67 entrance.r2.71 entrance.r1.47 entrance.r1.53 entrance.r1.65 entrance.r1.71 entrance.7 entrance.11 entrance.19 entrance.23 entrance.31 entrance.35 entrance.43 entrance.47 entrance.55 entrance.59 entrance.67 entrance.71 -Z+
    ----------
    Tile post2u.r3: IMPOSSIBLE!
      . #
      . O

      , ,
      , ,

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+
      underground -Y+ post2a.r3
      w2u.r3 -Z+
    ----------
    Tile post2u.r2: IMPOSSIBLE!
      # O
      . .

      , ,
      , ,

      w2u.r2 -X+
      underground -Y+ post2a.r2
      -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile post2u.r1: IMPOSSIBLE!
      O .
      # .

      , ,
      , ,

      -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      underground -Y+ post2a.r1
      -Z+ w2u.r1
    ----------
    Tile post2u: IMPOSSIBLE!
      . .
      O #

      , ,
      , ,

      -X+ w2u
      underground -Y+ post2a
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+
    ----------
    Tile corner.r3.0: IMPOSSIBLE!
      . #
      . O

      , #
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ corner.r3.6
      -Y+ corner.r3.2
      corner.1 invcorner.r1.1 wall.r3.1 door.r3.1 entrance.r3.41 -Z+ corner.r3.1
    ----------
    Tile corner.r3.1: IMPOSSIBLE!
      . O
      . .

      , O
      , ,

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ corner.r3.7
      -Y+ corner.r3.3
      corner.r3.0 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile corner.r3.6: IMPOSSIBLE!
      # .
      O #

      # #
      O #

      corner.r3.0 -X+ corner.r2.0 invcorner.r1.0 wall.r2.0 door.r2.0 entrance.r2.0
      -Y+ corner.r3.8
      corner.7 invcorner.r1.7 wall.r3.7 door.r3.7 entrance.r3.59 -Z+ corner.r3.7
    ----------
    Tile corner.r3.7: IMPOSSIBLE!
      O #
      . .

      O #
      , ,

      corner.r3.1 -X+ corner.r2.1 invcorner.r1.1 wall.r2.1 door.r2.1 entrance.r2.1
      -Y+ corner.r3.9
      corner.r3.6 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile corner.r2.0: IMPOSSIBLE!
      . #
      # O

      # #
      # O

      corner.r3.6 invcorner.6 wall.r2.6 door.r2.6 entrance.r2.60 -X+ corner.r2.6
      -Y+ corner.r2.2
      corner.r1.1 invcorner.1 wall.r1.1 door.r1.1 entrance.r1.5 -Z+ corner.r2.1
    ----------
    Tile corner.r2.1: IMPOSSIBLE!
      # O
      . .

      # O
      , ,

      corner.r3.7 invcorner.7 wall.r2.7 door.r2.7 entrance.r2.61 -X+ corner.r2.7
      -Y+ corner.r2.3
      corner.r2.0 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile corner.r2.6: IMPOSSIBLE!
      # .
      O .

      # ,
      O ,

      corner.r2.0 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ corner.r2.8
      corner.r1.7 invcorner.7 wall.r1.7 door.r1.7 entrance.r1.23 -Z+ corner.r2.7
    ----------
    Tile corner.r2.7: IMPOSSIBLE!
      O .
      . .

      O ,
      , ,

      corner.r2.1 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ corner.r2.9
      corner.r2.6 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile corner.r1.0: IMPOSSIBLE!
      . .
      # O

      , ,
      # O

      corner.6 invcorner.r3.6 wall.6 door.6 entrance.62 -X+ corner.r1.6
      -Y+ corner.r1.2
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ corner.r1.1
    ----------
    Tile corner.r1.1: IMPOSSIBLE!
      # O
      . #

      # O
      # #

      corner.7 invcorner.r3.7 wall.7 door.7 entrance.63 -X+ corner.r1.7
      -Y+ corner.r1.3
      corner.r1.0 -Z+ corner.r2.0 invcorner.r3.0 wall.r1.0 door.r1.0 entrance.r1.0
    ----------
    Tile corner.r1.6: IMPOSSIBLE!
      . .
      O .

      , ,
      O ,

      corner.r1.0 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ corner.r1.8
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ corner.r1.7
    ----------
    Tile corner.r1.7: IMPOSSIBLE!
      O .
      # .

      O ,
      # ,

      corner.r1.1 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ corner.r1.9
      corner.r1.6 -Z+ corner.r2.6 invcorner.r3.6 wall.r1.6 door.r1.6 entrance.r1.18
    ----------
    Tile corner.0: IMPOSSIBLE!
      . .
      . O

      , ,
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ corner.6
      -Y+ corner.2
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ corner.1
    ----------
    Tile corner.1: IMPOSSIBLE!
      . O
      . #

      , O
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ corner.7
      -Y+ corner.3
      corner.0 -Z+ corner.r3.0 invcorner.r2.0 wall.r3.0 door.r3.0 entrance.r3.36
    ----------
    Tile corner.6: IMPOSSIBLE!
      . .
      O #

      , ,
      O #

      corner.0 -X+ corner.r1.0 invcorner.r2.0 wall.0 door.0 entrance.2
      -Y+ corner.8
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ corner.7
    ----------
    Tile corner.7: IMPOSSIBLE!
      O #
      # .

      O #
      # #

      corner.1 -X+ corner.r1.1 invcorner.r2.1 wall.1 door.1 entrance.3
      -Y+ corner.9
      corner.6 -Z+ corner.r3.6 invcorner.r2.6 wall.r3.6 door.r3.6 entrance.r3.54
    ----------
    Tile invcorner.r3.0: IMPOSSIBLE!
      . #
      . O

      # #
      # O

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ invcorner.r3.6
      -Y+ invcorner.r3.2
      corner.r1.1 invcorner.1 wall.r1.1 door.r1.1 entrance.r1.5 -Z+ invcorner.r3.1
    ----------
    Tile invcorner.r3.1: IMPOSSIBLE!
      . O
      . .

      # O
      # #

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ invcorner.r3.7
      -Y+ invcorner.r3.3
      invcorner.r3.0 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile invcorner.r3.6: IMPOSSIBLE!
      # .
      O #

      # ,
      O #

      invcorner.r3.0 -X+ corner.r1.0 invcorner.r2.0 wall.0 door.0 entrance.2
      -Y+ invcorner.r3.8
      corner.r1.7 invcorner.7 wall.r1.7 door.r1.7 entrance.r1.23 -Z+ invcorner.r3.7
    ----------
    Tile invcorner.r3.7: IMPOSSIBLE!
      O #
      . .

      O #
      # #

      invcorner.r3.1 -X+ corner.r1.1 invcorner.r2.1 wall.1 door.1 entrance.3
      -Y+ invcorner.r3.9
      invcorner.r3.6 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile invcorner.r2.0: IMPOSSIBLE!
      . #
      # O

      , #
      # O

      corner.6 invcorner.r3.6 wall.6 door.6 entrance.62 -X+ invcorner.r2.6
      -Y+ invcorner.r2.2
      corner.1 invcorner.r1.1 wall.r3.1 door.r3.1 entrance.r3.41 -Z+ invcorner.r2.1
    ----------
    Tile invcorner.r2.1: IMPOSSIBLE!
      # O
      . .

      # O
      # #

      corner.7 invcorner.r3.7 wall.7 door.7 entrance.63 -X+ invcorner.r2.7
      -Y+ invcorner.r2.3
      invcorner.r2.0 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile invcorner.r2.6: IMPOSSIBLE!
      # .
      O .

      # #
      O #

      invcorner.r2.0 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ invcorner.r2.8
      corner.7 invcorner.r1.7 wall.r3.7 door.r3.7 entrance.r3.59 -Z+ invcorner.r2.7
    ----------
    Tile invcorner.r2.7: IMPOSSIBLE!
      O .
      . .

      O #
      # #

      invcorner.r2.1 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ invcorner.r2.9
      invcorner.r2.6 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile invcorner.r1.0: IMPOSSIBLE!
      . .
      # O

      # #
      # O

      corner.r3.6 invcorner.6 wall.r2.6 door.r2.6 entrance.r2.60 -X+ invcorner.r1.6
      -Y+ invcorner.r1.2
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ invcorner.r1.1
    ----------
    Tile invcorner.r1.1: IMPOSSIBLE!
      # O
      . #

      # O
      , #

      corner.r3.7 invcorner.7 wall.r2.7 door.r2.7 entrance.r2.61 -X+ invcorner.r1.7
      -Y+ invcorner.r1.3
      invcorner.r1.0 -Z+ corner.r3.0 invcorner.r2.0 wall.r3.0 door.r3.0 entrance.r3.36
    ----------
    Tile invcorner.r1.6: IMPOSSIBLE!
      . .
      O .

      # #
      O #

      invcorner.r1.0 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ invcorner.r1.8
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ invcorner.r1.7
    ----------
    Tile invcorner.r1.7: IMPOSSIBLE!
      O .
      # .

      O #
      # #

      invcorner.r1.1 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ invcorner.r1.9
      invcorner.r1.6 -Z+ corner.r3.6 invcorner.r2.6 wall.r3.6 door.r3.6 entrance.r3.54
    ----------
    Tile invcorner.0: IMPOSSIBLE!
      . .
      . O

      # #
      # O

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ invcorner.6
      -Y+ invcorner.2
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ invcorner.1
    ----------
    Tile invcorner.1: IMPOSSIBLE!
      . O
      . #

      # O
      # #

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ invcorner.7
      -Y+ invcorner.3
      invcorner.0 -Z+ corner.r2.0 invcorner.r3.0 wall.r1.0 door.r1.0 entrance.r1.0
    ----------
    Tile invcorner.6: IMPOSSIBLE!
      . .
      O #

      # #
      O #

      invcorner.0 -X+ corner.r2.0 invcorner.r1.0 wall.r2.0 door.r2.0 entrance.r2.0
      -Y+ invcorner.8
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ invcorner.7
    ----------
    Tile invcorner.7: IMPOSSIBLE!
      O #
      # .

      O #
      # ,

      invcorner.1 -X+ corner.r2.1 invcorner.r1.1 wall.r2.1 door.r2.1 entrance.r2.1
      -Y+ invcorner.9
      invcorner.6 -Z+ corner.r2.6 invcorner.r3.6 wall.r1.6 door.r1.6 entrance.r1.18
    ----------
    Tile wall.r3.0: IMPOSSIBLE!
      . #
      . #

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ wall.r3.6
      -Y+ wall.r3.2
      corner.1 invcorner.r1.1 wall.r3.1 door.r3.1 entrance.r3.41 -Z+ wall.r3.1
    ----------
    Tile wall.r3.1: IMPOSSIBLE!
      . #
      . #

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ wall.r3.7
      -Y+ wall.r3.3
      wall.r3.0 -Z+ corner.r3.0 invcorner.r2.0 wall.r3.0 door.r3.0 entrance.r3.36
    ----------
    Tile wall.r2.1: IMPOSSIBLE!
      # #
      . .

      # #
      , ,

      corner.r3.7 invcorner.7 wall.r2.7 door.r2.7 entrance.r2.61 -X+ wall.r2.7
      -Y+ wall.r2.3
      wall.r2.0 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile wall.r2.7: IMPOSSIBLE!
      # #
      . .

      # #
      , ,

      wall.r2.1 -X+ corner.r2.1 invcorner.r1.1 wall.r2.1 door.r2.1 entrance.r2.1
      -Y+ wall.r2.9
      wall.r2.6 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile wall.r1.6: IMPOSSIBLE!
      # .
      # .

      # ,
      # ,

      wall.r1.0 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ wall.r1.8
      corner.r1.7 invcorner.7 wall.r1.7 door.r1.7 entrance.r1.23 -Z+ wall.r1.7
    ----------
    Tile wall.r1.7: IMPOSSIBLE!
      # .
      # .

      # ,
      # ,

      wall.r1.1 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ wall.r1.9
      wall.r1.6 -Z+ corner.r2.6 invcorner.r3.6 wall.r1.6 door.r1.6 entrance.r1.18
    ----------
    Tile wall.0: IMPOSSIBLE!
      . .
      # #

      , ,
      # #

      corner.6 invcorner.r3.6 wall.6 door.6 entrance.62 -X+ wall.6
      -Y+ wall.2
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ wall.1
    ----------
    Tile wall.6: IMPOSSIBLE!
      . .
      # #

      , ,
      # #

      wall.0 -X+ corner.r1.0 invcorner.r2.0 wall.0 door.0 entrance.2
      -Y+ wall.8
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ wall.7
    ----------
    Tile door.r3.0: IMPOSSIBLE!
      . #
      . d

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ door.r3.6
      -Y+ door.r3.2
      corner.1 invcorner.r1.1 wall.r3.1 door.r3.1 entrance.r3.41 -Z+ door.r3.1
    ----------
    Tile door.r3.1: IMPOSSIBLE!
      . d
      . #

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ door.r3.7
      -Y+ door.r3.3
      door.r3.0 -Z+ corner.r3.0 invcorner.r2.0 wall.r3.0 door.r3.0 entrance.r3.36
    ----------
    Tile door.r2.1: IMPOSSIBLE!
      # d
      . .

      # #
      , ,

      corner.r3.7 invcorner.7 wall.r2.7 door.r2.7 entrance.r2.61 -X+ door.r2.7
      -Y+ door.r2.3
      door.r2.0 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile door.r2.7: IMPOSSIBLE!
      d #
      . .

      # #
      , ,

      door.r2.1 -X+ corner.r2.1 invcorner.r1.1 wall.r2.1 door.r2.1 entrance.r2.1
      -Y+ door.r2.9
      door.r2.6 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile door.r1.6: IMPOSSIBLE!
      # .
      d .

      # ,
      # ,

      door.r1.0 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ door.r1.8
      corner.r1.7 invcorner.7 wall.r1.7 door.r1.7 entrance.r1.23 -Z+ door.r1.7
    ----------
    Tile door.r1.7: IMPOSSIBLE!
      d .
      # .

      # ,
      # ,

      door.r1.1 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ door.r1.9
      door.r1.6 -Z+ corner.r2.6 invcorner.r3.6 wall.r1.6 door.r1.6 entrance.r1.18
    ----------
    Tile door.0: IMPOSSIBLE!
      . .
      # d

      , ,
      # #

      corner.6 invcorner.r3.6 wall.6 door.6 entrance.62 -X+ door.6
      -Y+ door.2
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ door.1
    ----------
    Tile door.6: IMPOSSIBLE!
      . .
      d #

      , ,
      # #

      door.0 -X+ corner.r1.0 invcorner.r2.0 wall.0 door.0 entrance.2
      -Y+ door.8
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ door.7
    ----------
    Tile entrance.r3.0: IMPOSSIBLE!
      . .
      . O

      , ,
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.18
      -Y+ entrance.r3.6
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.r3.1
    ----------
    Tile entrance.r3.1: IMPOSSIBLE!
      . O
      . .

      , O
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.19
      -Y+ entrance.r3.7
      entrance.r3.0 -Z+ entrance.r3.2
    ----------
    Tile entrance.r3.2: IMPOSSIBLE!
      . .
      . .

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.20
      -Y+ entrance.r3.8
      entrance.r3.1 -Z+ entrance.r3.3
    ----------
    Tile entrance.r3.3: IMPOSSIBLE!
      . .
      . .

      , #
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.21
      -Y+ entrance.r3.9
      entrance.r3.2 -Z+ entrance.r3.4
    ----------
    Tile entrance.r3.4: IMPOSSIBLE!
      . .
      . O

      , #
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.22
      -Y+ entrance.r3.10
      entrance.r3.3 -Z+ entrance.r3.5
    ----------
    Tile entrance.r3.5: IMPOSSIBLE!
      . O
      . .

      , O
      , ,

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r3.23
      -Y+ entrance.r3.11
      entrance.r3.4 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r3.18: IMPOSSIBLE!
      . .
      O .

      , ,
      O #

      entrance.r3.0 -X+ entrance.r3.36
      -Y+ entrance.r3.24
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.r3.19
    ----------
    Tile entrance.r3.19: IMPOSSIBLE!
      O .
      . .

      O #
      # #

      entrance.r3.1 -X+ entrance.r3.37
      -Y+ entrance.r3.25
      entrance.r3.18 -Z+ entrance.r3.20
    ----------
    Tile entrance.r3.22: IMPOSSIBLE!
      . .
      O .

      # #
      O #

      entrance.r3.4 -X+ entrance.r3.40
      -Y+ entrance.r3.28
      entrance.r3.21 -Z+ entrance.r3.23
    ----------
    Tile entrance.r3.23: IMPOSSIBLE!
      O .
      . .

      O #
      , ,

      entrance.r3.5 -X+ entrance.r3.41
      -Y+ entrance.r3.29
      entrance.r3.22 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r3.36: IMPOSSIBLE!
      . #
      . O

      , #
      # O

      entrance.r3.18 -X+ entrance.r3.54
      -Y+ entrance.r3.42
      corner.1 invcorner.r1.1 wall.r3.1 door.r3.1 entrance.r3.41 -Z+ entrance.r3.37
    ----------
    Tile entrance.r3.37: IMPOSSIBLE!
      . O
      . #

      # O
      # #

      entrance.r3.19 -X+ entrance.r3.55
      -Y+ entrance.r3.43
      entrance.r3.36 -Z+ entrance.r3.38
    ----------
    Tile entrance.r3.40: IMPOSSIBLE!
      . #
      . O

      # #
      # O

      entrance.r3.22 -X+ entrance.r3.58
      -Y+ entrance.r3.46
      entrance.r3.39 -Z+ entrance.r3.41
    ----------
    Tile entrance.r3.41: IMPOSSIBLE!
      . O
      . #

      # O
      , #

      entrance.r3.23 -X+ entrance.r3.59
      -Y+ entrance.r3.47
      entrance.r3.40 -Z+ corner.r3.0 invcorner.r2.0 wall.r3.0 door.r3.0 entrance.r3.36
    ----------
    Tile entrance.r3.54: IMPOSSIBLE!
      # .
      O .

      # #
      O #

      entrance.r3.36 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ entrance.r3.60
      corner.7 invcorner.r1.7 wall.r3.7 door.r3.7 entrance.r3.59 -Z+ entrance.r3.55
    ----------
    Tile entrance.r3.55: IMPOSSIBLE!
      O .
      # .

      O #
      # #

      entrance.r3.37 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ entrance.r3.61
      entrance.r3.54 -Z+ entrance.r3.56
    ----------
    Tile entrance.r3.58: IMPOSSIBLE!
      # .
      O .

      # #
      O #

      entrance.r3.40 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ entrance.r3.64
      entrance.r3.57 -Z+ entrance.r3.59
    ----------
    Tile entrance.r3.59: IMPOSSIBLE!
      O .
      # .

      O #
      # #

      entrance.r3.41 -X+ interior w2a.r1 invcorner.r3.0 invcorner.r3.1 invcorner.0 invcorner.1 wall.r1.0 wall.r1.1 door.r1.0 door.r1.1 entrance.r1.0 entrance.r1.1 entrance.r1.2 entrance.r1.3 entrance.r1.4 entrance.r1.5
      -Y+ entrance.r3.65
      entrance.r3.58 -Z+ corner.r3.6 invcorner.r2.6 wall.r3.6 door.r3.6 entrance.r3.54
    ----------
    Tile entrance.r2.0: IMPOSSIBLE!
      . .
      # O

      # #
      # O

      corner.r3.6 invcorner.6 wall.r2.6 door.r2.6 entrance.r2.60 -X+ entrance.r2.12
      -Y+ entrance.r2.4
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ entrance.r2.1
    ----------
    Tile entrance.r2.1: IMPOSSIBLE!
      # O
      . .

      # O
      , #

      corner.r3.7 invcorner.7 wall.r2.7 door.r2.7 entrance.r2.61 -X+ entrance.r2.13
      -Y+ entrance.r2.5
      entrance.r2.0 -Z+ entrance.r2.2
    ----------
    Tile entrance.r2.2: IMPOSSIBLE!
      . .
      . O

      , #
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r2.14
      -Y+ entrance.r2.6
      entrance.r2.1 -Z+ entrance.r2.3
    ----------
    Tile entrance.r2.3: IMPOSSIBLE!
      . O
      . .

      , O
      , ,

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.r2.15
      -Y+ entrance.r2.7
      entrance.r2.2 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r2.12: IMPOSSIBLE!
      . .
      O #

      # #
      O #

      entrance.r2.0 -X+ entrance.r2.24
      -Y+ entrance.r2.16
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ entrance.r2.13
    ----------
    Tile entrance.r2.13: IMPOSSIBLE!
      O #
      . .

      O #
      # #

      entrance.r2.1 -X+ entrance.r2.25
      -Y+ entrance.r2.17
      entrance.r2.12 -Z+ entrance.r2.14
    ----------
    Tile entrance.r2.14: IMPOSSIBLE!
      . .
      O .

      # #
      O #

      entrance.r2.2 -X+ entrance.r2.26
      -Y+ entrance.r2.18
      entrance.r2.13 -Z+ entrance.r2.15
    ----------
    Tile entrance.r2.15: IMPOSSIBLE!
      O .
      . .

      O #
      , ,

      entrance.r2.3 -X+ entrance.r2.27
      -Y+ entrance.r2.19
      entrance.r2.14 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r2.27: IMPOSSIBLE!
      . .
      . .

      # #
      , ,

      entrance.r2.15 -X+ entrance.r2.39
      -Y+ entrance.r2.31
      entrance.r2.26 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r2.39: IMPOSSIBLE!
      . .
      . .

      # #
      , ,

      entrance.r2.27 -X+ entrance.r2.51
      -Y+ entrance.r2.43
      entrance.r2.38 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r2.48: IMPOSSIBLE!
      . .
      # O

      # #
      # O

      entrance.r2.36 -X+ entrance.r2.60
      -Y+ entrance.r2.52
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ entrance.r2.49
    ----------
    Tile entrance.r2.49: IMPOSSIBLE!
      # O
      . .

      # O
      # #

      entrance.r2.37 -X+ entrance.r2.61
      -Y+ entrance.r2.53
      entrance.r2.48 -Z+ entrance.r2.50
    ----------
    Tile entrance.r2.50: IMPOSSIBLE!
      . .
      . O

      # #
      # O

      entrance.r2.38 -X+ entrance.r2.62
      -Y+ entrance.r2.54
      entrance.r2.49 -Z+ entrance.r2.51
    ----------
    Tile entrance.r2.51: IMPOSSIBLE!
      . O
      . .

      # O
      , ,

      entrance.r2.39 -X+ entrance.r2.63
      -Y+ entrance.r2.55
      entrance.r2.50 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r2.60: IMPOSSIBLE!
      . .
      O #

      # #
      O #

      entrance.r2.48 -X+ corner.r2.0 invcorner.r1.0 wall.r2.0 door.r2.0 entrance.r2.0
      -Y+ entrance.r2.64
      interior w2a invcorner.r3.1 invcorner.r3.7 invcorner.r2.1 invcorner.r2.7 wall.1 wall.7 door.1 door.7 entrance.3 entrance.15 entrance.27 entrance.39 entrance.51 entrance.63 -Z+ entrance.r2.61
    ----------
    Tile entrance.r2.61: IMPOSSIBLE!
      O #
      . .

      O #
      # ,

      entrance.r2.49 -X+ corner.r2.1 invcorner.r1.1 wall.r2.1 door.r2.1 entrance.r2.1
      -Y+ entrance.r2.65
      entrance.r2.60 -Z+ entrance.r2.62
    ----------
    Tile entrance.r2.62: IMPOSSIBLE!
      . .
      O .

      # ,
      O ,

      entrance.r2.50 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r2.66
      entrance.r2.61 -Z+ entrance.r2.63
    ----------
    Tile entrance.r2.63: IMPOSSIBLE!
      O .
      . .

      O ,
      , ,

      entrance.r2.51 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r2.67
      entrance.r2.62 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r1.0: IMPOSSIBLE!
      . #
      . O

      # #
      # O

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ entrance.r1.18
      -Y+ entrance.r1.6
      corner.r1.1 invcorner.1 wall.r1.1 door.r1.1 entrance.r1.5 -Z+ entrance.r1.1
    ----------
    Tile entrance.r1.1: IMPOSSIBLE!
      . O
      . #

      # O
      # #

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ entrance.r1.19
      -Y+ entrance.r1.7
      entrance.r1.0 -Z+ entrance.r1.2
    ----------
    Tile entrance.r1.4: IMPOSSIBLE!
      . #
      . O

      # #
      # O

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ entrance.r1.22
      -Y+ entrance.r1.10
      entrance.r1.3 -Z+ entrance.r1.5
    ----------
    Tile entrance.r1.5: IMPOSSIBLE!
      . O
      . #

      # O
      # #

      interior w2a.r3 invcorner.r2.6 invcorner.r2.7 invcorner.r1.6 invcorner.r1.7 wall.r3.6 wall.r3.7 door.r3.6 door.r3.7 entrance.r3.54 entrance.r3.55 entrance.r3.56 entrance.r3.57 entrance.r3.58 entrance.r3.59 -X+ entrance.r1.23
      -Y+ entrance.r1.11
      entrance.r1.4 -Z+ corner.r2.0 invcorner.r3.0 wall.r1.0 door.r1.0 entrance.r1.0
    ----------
    Tile entrance.r1.18: IMPOSSIBLE!
      # .
      O .

      # ,
      O #

      entrance.r1.0 -X+ entrance.r1.36
      -Y+ entrance.r1.24
      corner.r1.7 invcorner.7 wall.r1.7 door.r1.7 entrance.r1.23 -Z+ entrance.r1.19
    ----------
    Tile entrance.r1.19: IMPOSSIBLE!
      O .
      # .

      O #
      # #

      entrance.r1.1 -X+ entrance.r1.37
      -Y+ entrance.r1.25
      entrance.r1.18 -Z+ entrance.r1.20
    ----------
    Tile entrance.r1.22: IMPOSSIBLE!
      # .
      O .

      # #
      O #

      entrance.r1.4 -X+ entrance.r1.40
      -Y+ entrance.r1.28
      entrance.r1.21 -Z+ entrance.r1.23
    ----------
    Tile entrance.r1.23: IMPOSSIBLE!
      O .
      # .

      O #
      # ,

      entrance.r1.5 -X+ entrance.r1.41
      -Y+ entrance.r1.29
      entrance.r1.22 -Z+ corner.r2.6 invcorner.r3.6 wall.r1.6 door.r1.6 entrance.r1.18
    ----------
    Tile entrance.r1.36: IMPOSSIBLE!
      . .
      . O

      , ,
      # O

      entrance.r1.18 -X+ entrance.r1.54
      -Y+ entrance.r1.42
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.r1.37
    ----------
    Tile entrance.r1.37: IMPOSSIBLE!
      . O
      . .

      # O
      # #

      entrance.r1.19 -X+ entrance.r1.55
      -Y+ entrance.r1.43
      entrance.r1.36 -Z+ entrance.r1.38
    ----------
    Tile entrance.r1.40: IMPOSSIBLE!
      . .
      . O

      # #
      # O

      entrance.r1.22 -X+ entrance.r1.58
      -Y+ entrance.r1.46
      entrance.r1.39 -Z+ entrance.r1.41
    ----------
    Tile entrance.r1.41: IMPOSSIBLE!
      . O
      . .

      # O
      , ,

      entrance.r1.23 -X+ entrance.r1.59
      -Y+ entrance.r1.47
      entrance.r1.40 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.r1.54: IMPOSSIBLE!
      . .
      O .

      , ,
      O ,

      entrance.r1.36 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.60
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.r1.55
    ----------
    Tile entrance.r1.55: IMPOSSIBLE!
      O .
      . .

      O ,
      # ,

      entrance.r1.37 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.61
      entrance.r1.54 -Z+ entrance.r1.56
    ----------
    Tile entrance.r1.56: IMPOSSIBLE!
      . .
      . .

      # ,
      # ,

      entrance.r1.38 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.62
      entrance.r1.55 -Z+ entrance.r1.57
    ----------
    Tile entrance.r1.57: IMPOSSIBLE!
      . .
      . .

      # ,
      # ,

      entrance.r1.39 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.63
      entrance.r1.56 -Z+ entrance.r1.58
    ----------
    Tile entrance.r1.58: IMPOSSIBLE!
      . .
      O .

      # ,
      O ,

      entrance.r1.40 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.64
      entrance.r1.57 -Z+ entrance.r1.59
    ----------
    Tile entrance.r1.59: IMPOSSIBLE!
      O .
      . .

      O ,
      , ,

      entrance.r1.41 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.r1.65
      entrance.r1.58 -Z+ dirt w2u post2u corner.r1.0 corner.r1.6 corner.0 corner.6 wall.0 wall.6 door.0 door.6 entrance.r3.0 entrance.r3.18 entrance.r1.36 entrance.r1.54 entrance.0 entrance.12 entrance.24 entrance.36 entrance.48 entrance.60
    ----------
    Tile entrance.0: IMPOSSIBLE!
      . .
      . O

      , ,
      , O

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.12
      -Y+ entrance.4
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.1
    ----------
    Tile entrance.1: IMPOSSIBLE!
      . O
      . .

      , O
      , #

      dirt w2u.r1 post2u.r1 corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r1.6 wall.r1.7 door.r1.6 door.r1.7 entrance.r2.62 entrance.r2.63 entrance.r1.54 entrance.r1.55 entrance.r1.56 entrance.r1.57 entrance.r1.58 entrance.r1.59 entrance.60 entrance.61 -X+ entrance.13
      -Y+ entrance.5
      entrance.0 -Z+ entrance.2
    ----------
    Tile entrance.2: IMPOSSIBLE!
      . .
      # O

      , #
      # O

      corner.6 invcorner.r3.6 wall.6 door.6 entrance.62 -X+ entrance.14
      -Y+ entrance.6
      entrance.1 -Z+ entrance.3
    ----------
    Tile entrance.3: IMPOSSIBLE!
      # O
      . .

      # O
      # #

      corner.7 invcorner.r3.7 wall.7 door.7 entrance.63 -X+ entrance.15
      -Y+ entrance.7
      entrance.2 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile entrance.12: IMPOSSIBLE!
      . .
      O .

      , ,
      O #

      entrance.0 -X+ entrance.24
      -Y+ entrance.16
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.13
    ----------
    Tile entrance.13: IMPOSSIBLE!
      O .
      . .

      O #
      # #

      entrance.1 -X+ entrance.25
      -Y+ entrance.17
      entrance.12 -Z+ entrance.14
    ----------
    Tile entrance.14: IMPOSSIBLE!
      . .
      O #

      # #
      O #

      entrance.2 -X+ entrance.26
      -Y+ entrance.18
      entrance.13 -Z+ entrance.15
    ----------
    Tile entrance.15: IMPOSSIBLE!
      O #
      . .

      O #
      # #

      entrance.3 -X+ entrance.27
      -Y+ entrance.19
      entrance.14 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile entrance.24: IMPOSSIBLE!
      . .
      . .

      , ,
      # #

      entrance.12 -X+ entrance.36
      -Y+ entrance.28
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.25
    ----------
    Tile entrance.36: IMPOSSIBLE!
      . .
      . .

      , ,
      # #

      entrance.24 -X+ entrance.48
      -Y+ entrance.40
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.37
    ----------
    Tile entrance.48: IMPOSSIBLE!
      . .
      . O

      , ,
      # O

      entrance.36 -X+ entrance.60
      -Y+ entrance.52
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.49
    ----------
    Tile entrance.49: IMPOSSIBLE!
      . O
      . .

      # O
      # #

      entrance.37 -X+ entrance.61
      -Y+ entrance.53
      entrance.48 -Z+ entrance.50
    ----------
    Tile entrance.50: IMPOSSIBLE!
      . .
      # O

      # #
      # O

      entrance.38 -X+ entrance.62
      -Y+ entrance.54
      entrance.49 -Z+ entrance.51
    ----------
    Tile entrance.51: IMPOSSIBLE!
      # O
      . .

      # O
      # #

      entrance.39 -X+ entrance.63
      -Y+ entrance.55
      entrance.50 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
    Tile entrance.60: IMPOSSIBLE!
      . .
      O .

      , ,
      O ,

      entrance.48 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.64
      dirt w2u.r2 post2u.r2 corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r2.1 wall.r2.7 door.r2.1 door.r2.7 entrance.r3.5 entrance.r3.23 entrance.r2.3 entrance.r2.15 entrance.r2.27 entrance.r2.39 entrance.r2.51 entrance.r2.63 entrance.r1.41 entrance.r1.59 -Z+ entrance.61
    ----------
    Tile entrance.61: IMPOSSIBLE!
      O .
      . .

      O ,
      # ,

      entrance.49 -X+ dirt w2u.r3 post2u.r3 corner.r3.0 corner.r3.1 corner.0 corner.1 wall.r3.0 wall.r3.1 door.r3.0 door.r3.1 entrance.r3.0 entrance.r3.1 entrance.r3.2 entrance.r3.3 entrance.r3.4 entrance.r3.5 entrance.r2.2 entrance.r2.3 entrance.0 entrance.1
      -Y+ entrance.65
      entrance.60 -Z+ entrance.62
    ----------
    Tile entrance.62: IMPOSSIBLE!
      . .
      O #

      # ,
      O #

      entrance.50 -X+ corner.r1.0 invcorner.r2.0 wall.0 door.0 entrance.2
      -Y+ entrance.66
      entrance.61 -Z+ entrance.63
    ----------
    Tile entrance.63: IMPOSSIBLE!
      O #
      . .

      O #
      # #

      entrance.51 -X+ corner.r1.1 invcorner.r2.1 wall.1 door.1 entrance.3
      -Y+ entrance.67
      entrance.62 -Z+ interior w2a.r2 invcorner.r1.0 invcorner.r1.6 invcorner.0 invcorner.6 wall.r2.0 wall.r2.6 door.r2.0 door.r2.6 entrance.r2.0 entrance.r2.12 entrance.r2.24 entrance.r2.36 entrance.r2.48 entrance.r2.60
    ----------
  |};
};

// let%expect_test "yay" = {
//   let xs = 20;
//   let zs = 20;
//   let wave = Wave_collapse.make_blank_wave(ts, ~xs, ~ys=4, ~zs);
//   for (x in 0 to xs - 1) {
//     Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=0, 0);
//     Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=zs - 1, 0);
//   };
//   for (z in 0 to zs - 1) {
//     Wave_collapse.force_and_propagate(wave, ~x=0, ~y=0, ~z, 0);
//     Wave_collapse.force_and_propagate(wave, ~x=xs - 1, ~y=0, ~z, 0);
//   };
//   // try(Wave_collapse.force_edges(~x0=0, ~x1=0, ~z0=0, ~z1=0, wave)) {
//   // | Wave_collapse.Contradiction(_) => ()
//   // };
//   // Test_helpers.print_items(wave);
//   Test_helpers.print_items(wave);
//   Wave_collapse.collapse_all(wave);
//   Test_helpers.print_items(wave);
//   %expect
//   {|
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .
//     . . . . . . . . . . . . . . . . . . . . .

//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , ,
//     , , , , , , , , , , , , , , , , , , , , , |};
