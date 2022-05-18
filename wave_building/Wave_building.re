open! Core;

module Tag = {
  [@deriving sexp_of]
  type t =
    | Edge // Can be placed on edge of grid
    | Bottom; // Can be placed on bottom edge of grid
};

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

let ts =
  Wave_collapse.(
    create_tileset(
      ~tilesize=2,
      ~rotate_cw=Minecraft.Block.rotate_cw(~times=1),
      Minecraft.Block.[
        // Empty dirt
        tile(
          ~name="dirt2a",
          ~tags=Tag.[Edge],
          ~weight=0.01,
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
          ~weight=0.0,
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
      ],
    )
  );

module Test_helpers = {
  let show_item = b => {
    switch (
      List.find(palette, ~f=((_sym, mat)) =>
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
  let ys = 4;
  let zs = 20;
  let wave = Wave_collapse.make_blank_wave(ts, ~xs, ~ys, ~zs);
  Wave_collapse.force_edges(
    ~x0=Tag.Edge,
    ~x1=Tag.Edge,
    ~y0=Tag.Edge,
    ~y1=Tag.Bottom,
    ~z0=Tag.Edge,
    ~z1=Tag.Edge,
    wave,
  );
  Wave_collapse.collapse_all(wave);
  Test_helpers.print_items(wave);
  %expect
  {|
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