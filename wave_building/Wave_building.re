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
          ~tags=Tag.[Bottom, Edge],
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
          ~name="corner_floor",
          ~tags=Tag.[Building],
          ~rotate=true,
          ~weight=0.1,
          [|
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
        tile(
          ~name="corner_mid",
          ~tags=Tag.[Building],
          ~rotate=true,
          ~weight=0.1,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        tile(
          ~name="corner_ceil",
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
              [|Air, planks, planks|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        // Inverted corner
        tile(
          ~name="inv_corner_floor",
          ~tags=Tag.[Building],
          ~rotate=true,
          ~weight=0.1,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|planks, planks, planks|], /* */
              [|planks, post, planks|], /* */
              [|planks, planks, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        tile(
          ~name="inv_corner_mid",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        tile(
          ~name="inv_corner_ceil",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|planks, planks, planks|], /* */
              [|planks, post, planks|], /* */
              [|planks, planks, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        // Straight wall
        tile(
          ~name="wall_floor",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
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
        tile(
          ~name="wall_mid",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        tile(
          ~name="wall_ceil",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, planks, planks|], /* */
              [|Air, planks, planks|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
          |],
        ),
        // Door
        tile(
          ~name="door_floor",
          ~tags=Tag.[Building],
          ~rotate=true,
          [|
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, Oak_door(W, Upper), Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, Oak_door(W, Lower), Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, planks, planks|], /* */
              [|Dirt, planks, planks|], /* */
              [|Dirt, planks, planks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Interior ceiling
        tile(
          ~name="intceil",
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
              [|Air, Air|], /* */
              [|Air, Air|] /* */
            |],
          |],
        ),
        tile(
          ~name="intceil_skylight",
          ~tags=Tag.[Building],
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|planks, planks, planks|], /* */
              [|planks, Glass, planks|], /* */
              [|planks, planks, planks|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
          |],
        ),
        // Interior floor
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

let prepare_wave = (~xs, ~ys, ~zs) => {
  assert(ys >= 8);
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
  Wave_collapse.force_at(~x=xs / 2, ~y=1, ~z=zs / 2, Tag.Building, wave);
  Wave_collapse.collapse_all(wave);
  wave;
};

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
    ~only_impossible=true,
    ~show_tag=Test_helpers.show_tag,
    ~show_item=Test_helpers.show_item,
    ts,
  );
  %expect
  {| |};
};

let%expect_test "collapsing" = {
  let xs = 20;
  let ys = 10;
  let zs = 20;
  let wave = prepare_wave(~xs, ~ys, ~zs);
  Test_helpers.print_items(wave);
  %expect
  {|
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
    . . O # O . O # O . O # # # # # O . . . .
    . . # # # . # # # . # # # # # # # . . . .
    . . # # # . O # O . O # O # # # # . . . .
    . . O # O . . . . . . . # # ! # O # O . .
    . . . . . . . . . O # # O # # # # # # . .
    . . . O # # # O . # # # # # # # O # O . .
    . . . # # # # # . O # O # ! # # # . . . .
    . . . O # # # O . . . # # # # # O # O . .
    . . . . . . . . . O # O # # # # # # # . .
    . . O # # O . . . # # # # # # # # # # . .
    . . # # # # . O # O # ! # ! # # O # O . .
    . . O # # O . # # # # # # # # # # . . . .
    . . . . . . . # # # O # # O # # O # O . .
    . . O # # O . # # # # . . # # # # # # . .
    . . # # # # . # # # # . . # # # # # # . .
    . . O # # O . O # # O . . O # # # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # O . O # O . O # # # # # O . . . .
    . . # . # . # . # . # . . . . . # . . . .
    . . # . # . O # O . O # O . . . # . . . .
    . . O # O . . . . . . . # . . . O # O . .
    . . . . . . . . . O # # O . . . . . # . .
    . . . O # # # O . # . . . . . . O # O . .
    . . . # . . . # . O # O . . . . # . . . .
    . . . O # # # O . . . # . . . . O # O . .
    . . . . . . . . . O # O . . . . . . # . .
    . . O # # O . . . # . . . . . . . . # . .
    . . # . . # . O # O . . . . . . O # O . .
    . . O # # O . # . . . . . . . . # . . . .
    . . . . . . . # . . O # # O . . O # O . .
    . . O # # O . # . . # . . # . . . . # . .
    . . # . . # . # . . # . . # . . . . # . .
    . . O # # O . O # # O . . O # # # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # O . O # O . O # # # # # O . . . .
    . . # . # . # . # . # . . . . . # . . . .
    . . # . # . O # O . O # O . . . # . . . .
    . . O # O . . . . . . . # . . . O # O . .
    . . . . . . . . . O # # O . . . . . # . .
    . . . O # # # O . # . . . . . . O # O . .
    . . . # . . . # . O # O . . . . # . . . .
    . . . O # # # O . . . # . . . . O # O . .
    . . . . . . . . . O # O . . . . . . # . .
    . . O # # O . . . # . . . . . . . . # . .
    . . # . . # . O # O . . . . . . O # O . .
    . . O # # O . # . . . . . . . . # . . . .
    . . . . . . . # . . O # # O . . O # O . .
    . . O # # O . # . . # . . # . . . . # . .
    . . # . . # . # . . # . . # . . . . # . .
    . . O # # O . O # # O . . O # # # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # O . O # O . O # # # # # O . . . .
    . . # . # . # . # . # . . . . . # . . . .
    . . # . # . O # O . O # O . . . # . . . .
    . . O # O . . . . . . . # . . . O # O . .
    . . . . . . . . . O # # O . . . . . # . .
    . . . O # # # O . # . . . . . . O # O . .
    . . . # . . . # . O # O . . . . # . . . .
    . . . O # # # O . . . # . . . . O # O . .
    . . . . . . . . . O # O . . . . . . # . .
    . . O # # O . . . # . . . . . . . . # . .
    . . # . . # . O # O . . . . . . O # O . .
    . . O # # O . # . . . . . . . . # . . . .
    . . . . . . . # . . O # # O . . O # O . .
    . . O # # O . # . . # . . # . . . . # . .
    . . # . . # . # . . # . . # . . . . # . .
    . . O # # O . O # # O . . O # # # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # O . O # O . O # D # D # O . . . .
    . . # . # . # . # . # . . . . . # . . . .
    . . # . # . O # O . O # O . . . # . . . .
    . . O # O . . . . . . . # . . . O # O . .
    . . . . . . . . . O # # O . . . . . # . .
    . . . O # # # O . # . . . . . . O # O . .
    . . . # . . . # . O # O . . . . # . . . .
    . . . O # D # O . . . # . . . . O # O . .
    . . . . . . . . . O # O . . . . . . # . .
    . . O # # O . . . # . . . . . . . . # . .
    . . # . . # . O # O . . . . . . O # O . .
    . . O # # O . # . . . . . . . . # . . . .
    . . . . . . . # . . O # # O . . O # O . .
    . . O # # O . D . . # . . # . . . . # . .
    . . # . . # . # . . # . . # . . . . # . .
    . . O # # O . O # # O . . O # D # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . O # O . O # O . O # d # d # O . . . .
    . . # . # . # . # . # . . . . . # . . . .
    . . # . # . O # O . O # O . . . # . . . .
    . . O # O . . . . . . . # . . . O # O . .
    . . . . . . . . . O # # O . . . . . # . .
    . . . O # # # O . # . . . . . . O # O . .
    . . . # . . . # . O # O . . . . # . . . .
    . . . O # d # O . . . # . . . . O # O . .
    . . . . . . . . . O # O . . . . . . # . .
    . . O # # O . . . # . . . . . . . . # . .
    . . # . . # . O # O . . . . . . O # O . .
    . . O # # O . # . . . . . . . . # . . . .
    . . . . . . . # . . O # # O . . O # O . .
    . . O # # O . d . . # . . # . . . . # . .
    . . # . . # . # . . # . . # . . . . # . .
    . . O # # O . O # # O . . O # d # # O . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . .

    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , O # O , O # O , O # # # # # O , , , ,
    , , # # # , # # # , # # # # # # # , , , ,
    , , # # # , O # O , O # O # # # # , , , ,
    , , O # O , , , , , , , # # # # O # O , ,
    , , , , , , , , , O # # O # # # # # # , ,
    , , , O # # # O , # # # # # # # O # O , ,
    , , , # # # # # , O # O # # # # # , , , ,
    , , , O # # # O , , , # # # # # O # O , ,
    , , , , , , , , , O # O # # # # # # # , ,
    , , O # # O , , , # # # # # # # # # # , ,
    , , # # # # , O # O # # # # # # O # O , ,
    , , O # # O , # # # # # # # # # # , , , ,
    , , , , , , , # # # O # # O # # O # O , ,
    , , O # # O , # # # # , , # # # # # # , ,
    , , # # # # , # # # # , , # # # # # # , ,
    , , O # # O , O # # O , , O # # # # O , ,
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
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
    , , , , , , , , , , , , , , , , , , , , ,
  |};
};