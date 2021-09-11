open! Core_kernel;

let pillar = {
  open Minecraft_template;
  let base = rect(Stone_bricks, ~xs=1, ~ys=2, ~zs=1);
  let cap = rect(Chiseled_stone_bricks, ~xs=1, ~ys=1, ~zs=1);
  stack(base, cap);
};

let endpiece_going_north = {
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

let bridge = (~length, ~rotation) => {
  open Minecraft_template;
  let length = length - 2 * z_size_of(endpiece_going_north);
  assert(length > 0);
  let start_piece = endpiece_going_north;
  let connector =
    connector(length)
    |> align_with'(
         ~other=start_piece,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  let end_piece =
    endpiece_going_north
    |> rotate_90_cw(~times=2)
    |> align_with'(
         ~other=connector,
         ~x=(center, center),
         ~y=(max, max),
         ~z=(max, min),
       );
  combine_all([start_piece, connector, end_piece])
  |> rotate_90_cw(~times=rotation);
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