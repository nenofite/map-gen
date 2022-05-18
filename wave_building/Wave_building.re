open! Core;

module Tag = {
  [@deriving sexp_of]
  type t =
    | Edge // Can be placed on edge of grid
    | Bottom // Can be placed on bottom edge of grid
    | Building; // Anything within a building
};

module Blocks = {
  include Minecraft.Block;
  let post = Log(Oak_log, Y);
  let planks = Oak_planks;

  let palette = [
    (".", Air),
    (",", Dirt),
    ("O", post),
    ("#", planks),
    ("D", Oak_door(N, Upper)),
    ("d", Oak_door(N, Lower)),
    ("D", Oak_door(E, Upper)),
    ("d", Oak_door(E, Lower)),
    ("D", Oak_door(S, Upper)),
    ("d", Oak_door(S, Lower)),
    ("D", Oak_door(W, Upper)),
    ("d", Oak_door(W, Lower)),
    ("^", Stairs(Oak_stairs, N)),
    (">", Stairs(Oak_stairs, E)),
    ("v", Stairs(Oak_stairs, S)),
    ("<", Stairs(Oak_stairs, W)),
  ];
};

let ts =
  Wave_collapse.(
    create_tileset(
      ~tilesize=2,
      ~rotate_cw=Minecraft.Block.rotate_cw(~times=1),
      Blocks.[
        // Empty dirt
        tile(
          ~name="dirt2a",
          ~tags=Tag.[Edge],
          [|
            [|
              [|Air, Air|], /* */
              [|Air, Air|] /* */
            |],
            [|
              [|Dirt, Dirt|], /* */
              [|Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Underground
        tile(
          ~name="underground",
          ~tags=Tag.[Bottom, Edge],
          ~weight=0.1,
          [|
            [|
              [|Dirt, Dirt|], /* */
              [|Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt|], /* */
              [|Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Empty air
        tile(
          ~name="air",
          ~tags=Tag.[Edge],
          ~weight=0.01,
          [|
            [|
              [|Air, Air|], /* */
              [|Air, Air|] /* */
            |],
            [|
              [|Air, Air|], /* */
              [|Air, Air|] /* */
            |],
          |],
        ),
        // Corner
        tile(
          ~name="corner",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, post, planks|], /* */
              [|Dirt, planks, planks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Wall
        tile(
          ~name="wall",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, planks, planks|], /* */
              [|Dirt, planks, planks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Interior
        tile(
          ~name="interior",
          ~tags=Tag.[Building],
          [|
            [|
              [|Air, Air|], /* */
              [|Air, Air|] /* */
            |],
            [|
              [|planks, planks|], /* */
              [|planks, planks|] /* */
            |],
            [|
              [|Dirt, Dirt|], /* */
              [|Dirt, Dirt|] /* */
            |],
          |],
        ),
      ],
    )
  );

module Test_helpers = {
  let show_item = b => {
    switch (
      List.find(Blocks.palette, ~f=((_sym, mat)) =>
        Minecraft.Block.equal_material(b, mat)
      )
    ) {
    | Some((sym, _)) => sym
    | None => "!" // ? is used for unobserved tiles
    };
  };

  let show_tag = tag => Sexp.to_string_hum(Tag.sexp_of_t(tag));

  let print_items = eval => {
    Wave_collapse.Test_helpers.print_items(~show_item, eval);
  };
};

let%expect_test "tileset" = {
  Wave_collapse.Test_helpers.dump_tileset(
    // ~only_impossible=true,
    ~show_tag=Test_helpers.show_tag,
    ~show_item=Test_helpers.show_item,
    ts,
  );
  %expect
  {|
    Tile dirt2a:
      Edge
      . .
      . .

      , ,
      , ,

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      underground -Y+ air
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile underground:
      Bottom Edge
      , ,
      , ,

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ dirt2a underground corner.r3.0 corner.r3.1 corner.r3.6 corner.r3.7 corner.r2.0 corner.r2.1 corner.r2.6 corner.r2.7 corner.r1.0 corner.r1.1 corner.r1.6 corner.r1.7 corner.0 corner.1 corner.6 corner.7 wall.r3.0 wall.r3.1 wall.r2.0 wall.r2.3 wall.r1.0 wall.r1.1 wall.0 wall.3 interior.0
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile air:
      Edge
      . .
      . .

      . .
      . .

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      dirt2a air corner.r3.4 corner.r3.5 corner.r3.10 corner.r3.11 corner.r2.4 corner.r2.5 corner.r2.10 corner.r2.11 corner.r1.4 corner.r1.5 corner.r1.10 corner.r1.11 corner.4 corner.5 corner.10 corner.11 wall.r3.4 wall.r3.5 wall.r2.2 wall.r2.5 wall.r1.4 wall.r1.5 wall.2 wall.5 interior.1 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile corner.r3.0:
      Building
      , #
      , O

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ corner.r3.6
      underground -Y+ corner.r3.2
      corner.1 wall.0 -Z+ corner.r3.1
    ----------
    Tile corner.r3.1:
      Building
      , O
      , ,

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ corner.r3.7
      underground -Y+ corner.r3.3
      corner.r3.0 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile corner.r3.2:
      Building
      . #
      . O

      , #
      , O

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ corner.r3.8
      corner.r3.0 -Y+ corner.r3.4
      corner.3 wall.1 -Z+ corner.r3.3
    ----------
    Tile corner.r3.3:
      Building
      . O
      . .

      , O
      , ,

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ corner.r3.9
      corner.r3.1 -Y+ corner.r3.5
      corner.r3.2 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile corner.r3.4:
      Building
      . .
      . .

      . #
      . O

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ corner.r3.10
      corner.r3.2 -Y+ air
      corner.r1.5 corner.5 wall.r2.2 wall.2 -Z+ corner.r3.5
    ----------
    Tile corner.r3.5:
      Building
      . .
      . .

      . O
      . .

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ corner.r3.11
      corner.r3.3 -Y+ air
      corner.r3.4 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile corner.r3.6:
      Building
      # #
      O #

      , ,
      , ,

      corner.r3.0 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ corner.r3.8
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ corner.r3.7
    ----------
    Tile corner.r3.7:
      Building
      O #
      , ,

      , ,
      , ,

      corner.r3.1 -X+ corner.r2.1 wall.r3.1
      underground -Y+ corner.r3.9
      corner.r3.6 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile corner.r3.8:
      Building
      # .
      O #

      # #
      O #

      corner.r3.2 -X+ corner.r2.2 wall.r3.2
      corner.r3.6 -Y+ corner.r3.10
      corner.9 wall.4 -Z+ corner.r3.9
    ----------
    Tile corner.r3.9:
      Building
      O #
      . .

      O #
      , ,

      corner.r3.3 -X+ corner.r2.3 wall.r3.3
      corner.r3.7 -Y+ corner.r3.11
      corner.r3.8 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile corner.r3.10:
      Building
      . .
      . .

      # .
      O #

      corner.r3.4 -X+ corner.r2.4 corner.r1.4 wall.r3.4 wall.r1.4
      corner.r3.8 -Y+ air
      corner.r1.11 corner.11 wall.r2.5 wall.5 -Z+ corner.r3.11
    ----------
    Tile corner.r3.11:
      Building
      . .
      . .

      O #
      . .

      corner.r3.5 -X+ corner.r2.5 corner.r1.5 wall.r3.5 wall.r1.5
      corner.r3.9 -Y+ air
      corner.r3.10 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile corner.r2.0:
      Building
      # #
      # O

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ corner.r2.6
      underground -Y+ corner.r2.2
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ corner.r2.1
    ----------
    Tile corner.r2.1:
      Building
      # O
      , ,

      , ,
      , ,

      corner.r3.7 wall.r3.1 -X+ corner.r2.7
      underground -Y+ corner.r2.3
      corner.r2.0 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile corner.r2.2:
      Building
      . #
      # O

      # #
      # O

      corner.r3.8 wall.r3.2 -X+ corner.r2.8
      corner.r2.0 -Y+ corner.r2.4
      corner.r1.3 wall.r2.1 -Z+ corner.r2.3
    ----------
    Tile corner.r2.3:
      Building
      # O
      . .

      # O
      , ,

      corner.r3.9 wall.r3.3 -X+ corner.r2.9
      corner.r2.1 -Y+ corner.r2.5
      corner.r2.2 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile corner.r2.4:
      Building
      . .
      . .

      . #
      # O

      corner.r3.10 corner.10 wall.r3.4 wall.r1.4 -X+ corner.r2.10
      corner.r2.2 -Y+ air
      corner.r1.5 corner.5 wall.r2.2 wall.2 -Z+ corner.r2.5
    ----------
    Tile corner.r2.5:
      Building
      . .
      . .

      # O
      . .

      corner.r3.11 corner.11 wall.r3.5 wall.r1.5 -X+ corner.r2.11
      corner.r2.3 -Y+ air
      corner.r2.4 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile corner.r2.6:
      Building
      # ,
      O ,

      , ,
      , ,

      corner.r2.0 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ corner.r2.8
      corner.r1.7 wall.r2.3 -Z+ corner.r2.7
    ----------
    Tile corner.r2.7:
      Building
      O ,
      , ,

      , ,
      , ,

      corner.r2.1 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ corner.r2.9
      corner.r2.6 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile corner.r2.8:
      Building
      # .
      O .

      # ,
      O ,

      corner.r2.2 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      corner.r2.6 -Y+ corner.r2.10
      corner.r1.9 wall.r2.4 -Z+ corner.r2.9
    ----------
    Tile corner.r2.9:
      Building
      O .
      . .

      O ,
      , ,

      corner.r2.3 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      corner.r2.7 -Y+ corner.r2.11
      corner.r2.8 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile corner.r2.10:
      Building
      . .
      . .

      # .
      O .

      corner.r2.4 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      corner.r2.8 -Y+ air
      corner.r1.11 corner.11 wall.r2.5 wall.5 -Z+ corner.r2.11
    ----------
    Tile corner.r2.11:
      Building
      . .
      . .

      O .
      . .

      corner.r2.5 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      corner.r2.9 -Y+ air
      corner.r2.10 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile corner.r1.0:
      Building
      , ,
      # O

      , ,
      , ,

      corner.6 wall.r1.0 -X+ corner.r1.6
      underground -Y+ corner.r1.2
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ corner.r1.1
    ----------
    Tile corner.r1.1:
      Building
      # O
      # #

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ corner.r1.7
      underground -Y+ corner.r1.3
      corner.r1.0 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile corner.r1.2:
      Building
      . .
      # O

      , ,
      # O

      corner.8 wall.r1.2 -X+ corner.r1.8
      corner.r1.0 -Y+ corner.r1.4
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ corner.r1.3
    ----------
    Tile corner.r1.3:
      Building
      # O
      . #

      # O
      # #

      corner.9 wall.r1.3 -X+ corner.r1.9
      corner.r1.1 -Y+ corner.r1.5
      corner.r1.2 -Z+ corner.r2.2 wall.r2.1
    ----------
    Tile corner.r1.4:
      Building
      . .
      . .

      . .
      # O

      corner.r3.10 corner.10 wall.r3.4 wall.r1.4 -X+ corner.r1.10
      corner.r1.2 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ corner.r1.5
    ----------
    Tile corner.r1.5:
      Building
      . .
      . .

      # O
      . #

      corner.r3.11 corner.11 wall.r3.5 wall.r1.5 -X+ corner.r1.11
      corner.r1.3 -Y+ air
      corner.r1.4 -Z+ corner.r3.4 corner.r2.4 wall.r2.2 wall.2
    ----------
    Tile corner.r1.6:
      Building
      , ,
      O ,

      , ,
      , ,

      corner.r1.0 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ corner.r1.8
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ corner.r1.7
    ----------
    Tile corner.r1.7:
      Building
      O ,
      # ,

      , ,
      , ,

      corner.r1.1 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ corner.r1.9
      corner.r1.6 -Z+ corner.r2.6 wall.r2.3
    ----------
    Tile corner.r1.8:
      Building
      . .
      O .

      , ,
      O ,

      corner.r1.2 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      corner.r1.6 -Y+ corner.r1.10
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ corner.r1.9
    ----------
    Tile corner.r1.9:
      Building
      O .
      # .

      O ,
      # ,

      corner.r1.3 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      corner.r1.7 -Y+ corner.r1.11
      corner.r1.8 -Z+ corner.r2.8 wall.r2.4
    ----------
    Tile corner.r1.10:
      Building
      . .
      . .

      . .
      O .

      corner.r1.4 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      corner.r1.8 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ corner.r1.11
    ----------
    Tile corner.r1.11:
      Building
      . .
      . .

      O .
      # .

      corner.r1.5 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      corner.r1.9 -Y+ air
      corner.r1.10 -Z+ corner.r3.10 corner.r2.10 wall.r2.5 wall.5
    ----------
    Tile corner.0:
      Building
      , ,
      , O

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ corner.6
      underground -Y+ corner.2
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ corner.1
    ----------
    Tile corner.1:
      Building
      , O
      , #

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ corner.7
      underground -Y+ corner.3
      corner.0 -Z+ corner.r3.0 wall.0
    ----------
    Tile corner.2:
      Building
      . .
      . O

      , ,
      , O

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ corner.8
      corner.0 -Y+ corner.4
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ corner.3
    ----------
    Tile corner.3:
      Building
      . O
      . #

      , O
      , #

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ corner.9
      corner.1 -Y+ corner.5
      corner.2 -Z+ corner.r3.2 wall.1
    ----------
    Tile corner.4:
      Building
      . .
      . .

      . .
      . O

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ corner.10
      corner.2 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ corner.5
    ----------
    Tile corner.5:
      Building
      . .
      . .

      . O
      . #

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ corner.11
      corner.3 -Y+ air
      corner.4 -Z+ corner.r3.4 corner.r2.4 wall.r2.2 wall.2
    ----------
    Tile corner.6:
      Building
      , ,
      O #

      , ,
      , ,

      corner.0 -X+ corner.r1.0 wall.r1.0
      underground -Y+ corner.8
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ corner.7
    ----------
    Tile corner.7:
      Building
      O #
      # #

      , ,
      , ,

      corner.1 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ corner.9
      corner.6 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile corner.8:
      Building
      . .
      O #

      , ,
      O #

      corner.2 -X+ corner.r1.2 wall.r1.2
      corner.6 -Y+ corner.10
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ corner.9
    ----------
    Tile corner.9:
      Building
      O #
      # .

      O #
      # #

      corner.3 -X+ corner.r1.3 wall.r1.3
      corner.7 -Y+ corner.11
      corner.8 -Z+ corner.r3.8 wall.4
    ----------
    Tile corner.10:
      Building
      . .
      . .

      . .
      O #

      corner.4 -X+ corner.r2.4 corner.r1.4 wall.r3.4 wall.r1.4
      corner.8 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ corner.11
    ----------
    Tile corner.11:
      Building
      . .
      . .

      O #
      # .

      corner.5 -X+ corner.r2.5 corner.r1.5 wall.r3.5 wall.r1.5
      corner.9 -Y+ air
      corner.10 -Z+ corner.r3.10 corner.r2.10 wall.r2.5 wall.5
    ----------
    Tile wall.r3.0:
      Building
      # #
      # #

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ wall.r3.2
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ wall.r3.1
    ----------
    Tile wall.r3.1:
      Building
      # #
      , ,

      , ,
      , ,

      corner.r3.7 wall.r3.1 -X+ corner.r2.1 wall.r3.1
      underground -Y+ wall.r3.3
      wall.r3.0 -Z+ underground corner.r1.0 corner.r1.6 corner.0 corner.6 wall.r1.0
    ----------
    Tile wall.r3.2:
      Building
      . .
      # #

      # #
      # #

      corner.r3.8 wall.r3.2 -X+ corner.r2.2 wall.r3.2
      wall.r3.0 -Y+ wall.r3.4
      wall.r1.3 interior.1 -Z+ wall.r3.3
    ----------
    Tile wall.r3.3:
      Building
      # #
      . .

      # #
      , ,

      corner.r3.9 wall.r3.3 -X+ corner.r2.3 wall.r3.3
      wall.r3.1 -Y+ wall.r3.5
      wall.r3.2 -Z+ dirt2a corner.r1.2 corner.r1.8 corner.2 corner.8 wall.r1.2
    ----------
    Tile wall.r3.4:
      Building
      . .
      . .

      . .
      # #

      corner.r3.10 corner.10 wall.r3.4 wall.r1.4 -X+ corner.r2.4 corner.r1.4 wall.r3.4 wall.r1.4
      wall.r3.2 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ wall.r3.5
    ----------
    Tile wall.r3.5:
      Building
      . .
      . .

      # #
      . .

      corner.r3.11 corner.11 wall.r3.5 wall.r1.5 -X+ corner.r2.5 corner.r1.5 wall.r3.5 wall.r1.5
      wall.r3.3 -Y+ air
      wall.r3.4 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile wall.r2.0:
      Building
      # #
      # #

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ wall.r2.3
      underground -Y+ wall.r2.1
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile wall.r2.1:
      Building
      . #
      . #

      # #
      # #

      wall.4 interior.1 -X+ wall.r2.4
      wall.r2.0 -Y+ wall.r2.2
      corner.r1.3 wall.r2.1 -Z+ corner.r2.2 wall.r2.1
    ----------
    Tile wall.r2.2:
      Building
      . .
      . .

      . #
      . #

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ wall.r2.5
      wall.r2.1 -Y+ air
      corner.r1.5 corner.5 wall.r2.2 wall.2 -Z+ corner.r3.4 corner.r2.4 wall.r2.2 wall.2
    ----------
    Tile wall.r2.3:
      Building
      # ,
      # ,

      , ,
      , ,

      wall.r2.0 -X+ underground corner.r3.0 corner.r3.1 corner.0 corner.1 wall.0
      underground -Y+ wall.r2.4
      corner.r1.7 wall.r2.3 -Z+ corner.r2.6 wall.r2.3
    ----------
    Tile wall.r2.4:
      Building
      # .
      # .

      # ,
      # ,

      wall.r2.1 -X+ dirt2a corner.r3.2 corner.r3.3 corner.2 corner.3 wall.1
      wall.r2.3 -Y+ wall.r2.5
      corner.r1.9 wall.r2.4 -Z+ corner.r2.8 wall.r2.4
    ----------
    Tile wall.r2.5:
      Building
      . .
      . .

      # .
      # .

      wall.r2.2 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      wall.r2.4 -Y+ air
      corner.r1.11 corner.11 wall.r2.5 wall.5 -Z+ corner.r3.10 corner.r2.10 wall.r2.5 wall.5
    ----------
    Tile wall.r1.0:
      Building
      , ,
      # #

      , ,
      , ,

      corner.6 wall.r1.0 -X+ corner.r1.0 wall.r1.0
      underground -Y+ wall.r1.2
      underground corner.r3.1 corner.r3.7 corner.r2.1 corner.r2.7 wall.r3.1 -Z+ wall.r1.1
    ----------
    Tile wall.r1.1:
      Building
      # #
      # #

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ wall.r1.3
      wall.r1.0 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile wall.r1.2:
      Building
      . .
      # #

      , ,
      # #

      corner.8 wall.r1.2 -X+ corner.r1.2 wall.r1.2
      wall.r1.0 -Y+ wall.r1.4
      dirt2a corner.r3.3 corner.r3.9 corner.r2.3 corner.r2.9 wall.r3.3 -Z+ wall.r1.3
    ----------
    Tile wall.r1.3:
      Building
      # #
      . .

      # #
      # #

      corner.9 wall.r1.3 -X+ corner.r1.3 wall.r1.3
      wall.r1.1 -Y+ wall.r1.5
      wall.r1.2 -Z+ wall.r3.2 interior.1
    ----------
    Tile wall.r1.4:
      Building
      . .
      . .

      . .
      # #

      corner.r3.10 corner.10 wall.r3.4 wall.r1.4 -X+ corner.r2.4 corner.r1.4 wall.r3.4 wall.r1.4
      wall.r1.2 -Y+ air
      air corner.r3.5 corner.r3.11 corner.r2.5 corner.r2.11 wall.r3.5 wall.r1.5 -Z+ wall.r1.5
    ----------
    Tile wall.r1.5:
      Building
      . .
      . .

      # #
      . .

      corner.r3.11 corner.11 wall.r3.5 wall.r1.5 -X+ corner.r2.5 corner.r1.5 wall.r3.5 wall.r1.5
      wall.r1.3 -Y+ air
      wall.r1.4 -Z+ air corner.r1.4 corner.r1.10 corner.4 corner.10 wall.r3.4 wall.r1.4
    ----------
    Tile wall.0:
      Building
      , #
      , #

      , ,
      , ,

      underground corner.r2.6 corner.r2.7 corner.r1.6 corner.r1.7 wall.r2.3 -X+ wall.3
      underground -Y+ wall.1
      corner.1 wall.0 -Z+ corner.r3.0 wall.0
    ----------
    Tile wall.1:
      Building
      . #
      . #

      , #
      , #

      dirt2a corner.r2.8 corner.r2.9 corner.r1.8 corner.r1.9 wall.r2.4 -X+ wall.4
      wall.0 -Y+ wall.2
      corner.3 wall.1 -Z+ corner.r3.2 wall.1
    ----------
    Tile wall.2:
      Building
      . .
      . .

      . #
      . #

      air corner.r2.10 corner.r2.11 corner.r1.10 corner.r1.11 wall.r2.5 wall.5 -X+ wall.5
      wall.1 -Y+ air
      corner.r1.5 corner.5 wall.r2.2 wall.2 -Z+ corner.r3.4 corner.r2.4 wall.r2.2 wall.2
    ----------
    Tile wall.3:
      Building
      # #
      # #

      , ,
      , ,

      wall.0 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ wall.4
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile wall.4:
      Building
      # .
      # .

      # #
      # #

      wall.1 -X+ wall.r2.1 interior.1
      wall.3 -Y+ wall.5
      corner.9 wall.4 -Z+ corner.r3.8 wall.4
    ----------
    Tile wall.5:
      Building
      . .
      . .

      # .
      # .

      wall.2 -X+ air corner.r3.4 corner.r3.5 corner.4 corner.5 wall.r2.2 wall.2
      wall.4 -Y+ air
      corner.r1.11 corner.11 wall.r2.5 wall.5 -Z+ corner.r3.10 corner.r2.10 wall.r2.5 wall.5
    ----------
    Tile interior.0:
      Building
      # #
      # #

      , ,
      , ,

      corner.r3.6 corner.7 wall.r3.0 wall.r1.1 wall.3 interior.0 -X+ corner.r2.0 corner.r1.1 wall.r3.0 wall.r2.0 wall.r1.1 interior.0
      underground -Y+ interior.1
      corner.r1.1 corner.7 wall.r2.0 wall.r1.1 wall.3 interior.0 -Z+ corner.r3.6 corner.r2.0 wall.r3.0 wall.r2.0 wall.3 interior.0
    ----------
    Tile interior.1:
      Building
      . .
      . .

      # #
      # #

      wall.4 interior.1 -X+ wall.r2.1 interior.1
      interior.0 -Y+ air
      wall.r1.3 interior.1 -Z+ wall.r3.2 interior.1
    ----------
  |};
};

let%expect_test "collapsing" = {
  let xs = 20;
  let ys = 4;
  let zs = 20;
  let wave = Wave_collapse.make_blank_wave(ts, ~xs, ~ys, ~zs);
  Wave_collapse.force_edges(
    ~x0=Tag.Edge,
    ~x1=Tag.Edge,
    ~y0=Tag.Bottom,
    ~y1=Tag.Edge,
    ~z0=Tag.Edge,
    ~z1=Tag.Edge,
    wave,
  );
  // Wave_collapse.force_at(~x=4, ~y=2, ~z=6, Tag.Bottom, wave);
  Wave_collapse.collapse_all(wave);
  // Wave_collapse.try_collapse_next_lowest_entropy(wave) |> ignore;
  Test_helpers.print_items(wave);
  %expect
  {|
    Collapsing 1 1 1 (opts=(underground dirt2a)) to dirt2a
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,

    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
  |};
};