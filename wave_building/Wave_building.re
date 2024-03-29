open! Core;

module Tag = {
  [@deriving sexp_of]
  type t =
    | Edge // Can be placed on edge of grid
    | Bottom // Can be placed on bottom edge of grid
    | Building // Anything within a building
    | Door
    | Corner; // Convex corner of a building
};

module Blocks = {
  include Minecraft.Block;
  let post = Log(Oak_log, Y);
  let planks = Oak_planks;

  let palette = [
    (".", Air),
    (",", Dirt),
    (";", Stone_bricks),
    ("O", post),
    ("|", Log(Oak_log, Z)),
    ("=", Log(Oak_log, X)),
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
      ~tilesize=3,
      ~rotate_cw=Minecraft.Block.rotate_cw(~times=1),
      Blocks.[
        // Empty dirt
        tile(
          ~name="dirt2a",
          ~tags=Tag.[Bottom, Edge],
          Irrelevant,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Underground
        tile(
          ~name="underground",
          ~tags=Tag.[Bottom, Edge],
          Irrelevant,
          [|
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Empty air
        tile(
          ~name="air",
          ~tags=Tag.[Edge],
          Irrelevant,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
          |],
        ),
        // Corner
        tile(
          ~name="corner",
          ~tags=Tag.[Corner, Building],
          ~rotate=true,
          ~weight=0.1,
          ~full_auto=true,
          Boundary,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, Log(Oak_log, X)|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|] /* */
            |],
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
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        tile(
          ~name="inv_corner",
          ~tags=Tag.[Building],
          ~rotate=true,
          ~weight=0.01,
          ~full_auto=true,
          Boundary,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, post, Log(Oak_log, X)|], /* */
              [|Oak_slab, Log(Oak_log, Z), Air|] /* */
            |],
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
            [|
              [|Air, Air, Air|], /* */
              [|Air, post, planks|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Stone_bricks, Stone_bricks, Stone_bricks|], /* */
              [|Stone_bricks, Stone_bricks, Stone_bricks|], /* */
              [|Stone_bricks, Stone_bricks, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        // Straight wall
        tile(
          ~name="wall",
          ~tags=Tag.[Building],
          ~rotate=true,
          ~full_auto=true,
          Boundary,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        tile(
          ~name="wall_window",
          ~tags=Tag.[Building],
          ~weight=0.5,
          ~rotate=true,
          ~full_auto=true,
          Boundary,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|
                Air,
                Fence(
                  Oak_fence,
                  make_fence_extends(~north=true, ~south=true, ()),
                  Dry,
                ),
                Air,
              |], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|], /* */
              [|Air, planks, Air|] /* */
            |],
            [|
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        tile(
          ~name="door",
          ~tags=Tag.[Door, Building],
          ~weight=0.1,
          ~rotate=true,
          Walkable,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|], /* */
              [|Air, Log(Oak_log, Z), Oak_slab|] /* */
            |],
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
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|], /* */
              [|Dirt, Stone_bricks, Stone_bricks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
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
          ~full_auto=true,
          Walkable,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
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
          ~weight=10.0,
          Walkable,
          [|
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|], /* */
              [|Air, Air, Air|] /* */
            |],
            [|
              [|Stone_bricks, Stone_bricks, Stone_bricks|], /* */
              [|Stone_bricks, Stone_bricks, Stone_bricks|], /* */
              [|Stone_bricks, Stone_bricks, Stone_bricks|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt|] /* */
            |],
          |],
        ),
        tile(
          ~name="pillar",
          ~tags=Tag.[Building],
          ~weight=0.2,
          Walkable,
          [|
            [|
              [|Oak_slab, Oak_slab, Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab, Oak_slab, Oak_slab|], /* */
              [|Oak_slab, Oak_slab, Oak_slab, Oak_slab, Oak_slab|] /* */
            |],
            [|
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Wall_torch(N), Air, Air|], /* */
              [|Air, Wall_torch(W), Oak_planks, Wall_torch(E), Air|], /* */
              [|Air, Air, Wall_torch(S), Air, Air|], /* */
              [|Air, Air, Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Stone_brick_wall, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|] /* */
            |],
            [|
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Stone_brick_wall, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|], /* */
              [|Air, Air, Air, Air, Air|] /* */
            |],
            [|
              [|
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
              |], /* */
              [|
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
              |], /* */
              [|
                Stone_bricks,
                Stone_bricks,
                Chiseled_stone_bricks,
                Stone_bricks,
                Stone_bricks,
              |], /* */
              [|
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
              |], /* */
              [|
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
                Stone_bricks,
              |] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|] /* */
            |],
            [|
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|], /* */
              [|Dirt, Dirt, Dirt, Dirt, Dirt|] /* */
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
    ~walkable=true,
    ~unwalkable=true,
    ~x0=Tag.Edge,
    ~x1=Tag.Edge,
    ~y0=Tag.Bottom,
    ~y1=Tag.Edge,
    ~z0=Tag.Edge,
    ~z1=Tag.Edge,
    wave,
  );
  Wave_collapse.force_at(
    ~walkability=Walkable,
    ~x=xs / 2,
    ~y=1,
    ~z=zs - 2,
    Tag.Door,
    wave,
  );
  // Wave_collapse.force_at(~x=xs / 2, ~y=1, ~z=zs / 2, Tag.Building, wave);
  // Wave_collapse.force_at(~x=xs / 2, ~y=1, ~z=1, Tag.Building, wave);
  // Wave_collapse.force_at(~x=xs - 2, ~y=1, ~z=zs / 2, Tag.Building, wave);
  // Wave_collapse.force_at(~x=1, ~y=1, ~z=zs / 2, Tag.Building, wave);
  // Wave_collapse.collapse_all(wave);
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
  let xs = 10;
  let ys = 8;
  let zs = 10;
  let wave = prepare_wave(~xs, ~ys, ~zs);
  Wave_collapse.collapse_all(~peek=Test_helpers.print_items, wave);
  Test_helpers.print_items(wave);
  %expect
  {|
  |};
};