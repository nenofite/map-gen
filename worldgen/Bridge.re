open! Core_kernel;

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
  combine_all([stairs0, inv_stairs0, stairs1, inv_stairs1, stairs2]);
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
         ~x=(min, min),
         ~y=(max, max),
         ~z=(max, min),
       );
  let end_piece =
    endpiece_going_north
    |> rotate_90_cw(~times=2)
    |> align_with'(
         ~other=connector,
         ~x=(min, min),
         ~y=(max, max),
         ~z=(max, min),
       );
  let debug_marker = rect(Glowstone, ~xs=1, ~ys=1, ~zs=1);
  combine_all([start_piece, connector, end_piece, debug_marker])
  //   |> rotate_90_cw(~times=rotation);
  |> rotate_90_cw(~times=rotation + 2 /* TODO idk why but this fixes it */);
};
