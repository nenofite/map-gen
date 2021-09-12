open! Core_kernel;

let pillar = {
  open Minecraft_template;
  let base = rect(Stone_bricks, ~xs=1, ~ys=2, ~zs=1);
  let cap = rect(Chiseled_stone_bricks, ~xs=1, ~ys=1, ~zs=1);
  stack(base, cap);
};

let short_endpiece_going_north = {
  open Minecraft_template;
  let stairs = rect(Stairs(Stone_brick_stairs, N), ~xs=3, ~ys=1, ~zs=1);
  stairs |> align_with_origin'(~x=center, ~y=min, ~z=shift(max, ~by=-1));
};

let long_endpiece_going_north = {
  open Minecraft_template;
  let stairs = rect(Stairs(Stone_brick_stairs, N), ~xs=3, ~ys=1, ~zs=1);
  let inv_stairs = rect(Stairs(Stone_brick_stairs, Sd), ~xs=3, ~ys=1, ~zs=1);
  let stairs0 =
    stairs |> align_with_origin'(~x=center, ~y=min, ~z=shift(max, ~by=-1));
  let inv_stairs0 =
    inv_stairs
    |> align_with'(
         ~other=stairs0,
         ~x=(min, min),
         ~y=(min, min),
         ~z=(max, min),
       );
  let stairs1 =
    stairs
    |> align_with'(
         ~other=inv_stairs0,
         ~x=(min, min),
         ~y=(min, max),
         ~z=(min, min),
       );
  let inv_stairs1 =
    inv_stairs
    |> align_with'(
         ~other=stairs1,
         ~x=(min, min),
         ~y=(min, min),
         ~z=(max, min),
       );
  let stairs2 =
    stairs
    |> align_with'(
         ~other=inv_stairs1,
         ~x=(min, min),
         ~y=(min, max),
         ~z=(min, min),
       );
  let left_pillar =
    pillar
    |> align_with'(
         ~other=stairs0,
         ~x=(max, min),
         ~y=(min, min),
         ~z=(max, max),
       );
  let right_pillar =
    pillar
    |> align_with'(
         ~other=stairs0,
         ~x=(min, max),
         ~y=(min, min),
         ~z=(max, max),
       );
  combine_all([
    stairs0,
    inv_stairs0,
    stairs1,
    inv_stairs1,
    stairs2,
    left_pillar,
    right_pillar,
  ]);
};

let connector = length => {
  Minecraft_template.(rect(Stone_bricks, ~xs=3, ~ys=1, ~zs=length));
};

let short_bridge = (~length) => {
  open Minecraft_template;
  let start_piece = short_endpiece_going_north;
  let connector_length = max(length - 2, 0);
  let connector =
    connector(connector_length)
    |> align_with'(
         ~other=start_piece,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  let end_piece =
    short_endpiece_going_north
    |> rotate_90_cw(~times=2)
    |> align_with'(
         ~other=connector,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  combine_all([start_piece, connector, end_piece]);
};

let long_bridge = (~mid_length) => {
  open Minecraft_template;
  let start_piece = long_endpiece_going_north;
  let connector =
    connector(mid_length)
    |> align_with'(
         ~other=start_piece,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  let end_piece =
    long_endpiece_going_north
    |> rotate_90_cw(~times=2)
    |> align_with'(
         ~other=connector,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  combine_all([start_piece, connector, end_piece]);
};

let bridge = (~length, ~rotation) => {
  open Minecraft_template;
  let mid_length = length - 2 * z_size_of(long_endpiece_going_north);
  let t =
    if (mid_length > 0) {
      long_bridge(~mid_length);
    } else {
      short_bridge(~length);
    };
  rotate_90_cw(t, ~times=rotation);
};

module Test_helpers = {
  include Test_helpers;

  let show_block = (b: Minecraft.Block.material) =>
    switch (b) {
    | Air => Some(".")
    | Stone_bricks => Some("#")
    | Chiseled_stone_bricks => Some("O")
    | Stairs(Stone_brick_stairs, dir) =>
      switch (dir) {
      | N => Some("^")
      | E => Some(">")
      | S => Some("v")
      | W => Some("<")
      | Nd
      | Ed
      | Sd
      | Wd => Some("x")
      }
    | _ => Some("?")
    };
};

let%expect_test "creating a bridge template" = {
  open Test_helpers;
  let b = bridge(~length=10, ~rotation=1);

  Printf.printf("%d\n", Minecraft_template.x_size_of(b));
  %expect
  "10";

  show_template_top_down(~show_block, b) |> print_grid;
  %expect
  "
    O                 O
    > > > # # # # < < <
    > > > # # # # < < <
    > > > # # # # < < <
    O                 O
  ";

  show_template_south_north(~show_block, b) |> print_grid;
  %expect
  "
    O   > # # # # <   O
    # > x         x < #
    # x             x #
  ";
};

let%expect_test "short bridges" = {
  open Test_helpers;
  let b = bridge(~length=4, ~rotation=2);

  Printf.printf("%d\n", Minecraft_template.z_size_of(b));
  %expect
  "4";

  show_template_top_down(~show_block, b) |> print_grid;
  %expect
  "
    v v v
    # # #
    # # #
    ^ ^ ^
  ";

  show_template_south_north(~show_block, b) |> print_grid;
  %expect
  "
    ^ ^ ^
  ";
};