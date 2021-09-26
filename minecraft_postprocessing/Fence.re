open! Core_kernel;
open Minecraft;

let should_extend_to = (~x, ~y, ~z, r: Region.t) => {
  switch (Region.get_block_opt(~x, ~y, ~z, r)) {
  | Some(Fence(_) | Fence_gate(_)) => true
  | Some(_)
  | None => false
  };
};

let is_fence = (b: Block.material) =>
  switch (b) {
  | Fence(_) => true
  | _ => false
  };

let connect_single_fence = (~x, ~y, ~z, r: Region.t): unit => {
  let north = should_extend_to(~x, ~y, ~z=z - 1, r);
  let east = should_extend_to(~x=x + 1, ~y, ~z, r);
  let south = should_extend_to(~x, ~y, ~z=z + 1, r);
  let west = should_extend_to(~x=x - 1, ~y, ~z, r);
  if (north || east || south || west) {
    let here = Region.get_block(~x, ~y, ~z, r);
    let new_fence =
      switch (here) {
      | Fence(mat, _extends, waterlogged) =>
        Block.(Fence(mat, {north, east, south, west}, waterlogged))
      | _ => failwith("Expected fence")
      };
    Region.set_block(new_fence, ~x, ~y, ~z, r);
  };
};

/** Adjusts all fences in the region so they extend to adjacent fences and gates */
let connect_fences = (r: Region.t): unit => {
  Region.iter_region_xyz(r, ~f=(~x, ~y, ~z) =>
    if (is_fence(Region.get_block(~x, ~y, ~z, r))) {
      connect_single_fence(~x, ~y, ~z, r);
    }
  );
};

module Test_helpers = {
  include Test_helpers;

  let show_block = (b: Block.material) =>
    switch (b) {
    | Fence(_, extends, _) =>
      Some(
        switch (extends) {
        | {north: false, east: false, south: false, west: false} => ":"
        | {north: true, east: false, south: false, west: false} => "^"
        | {north: false, east: true, south: false, west: false} => ">"
        | {north: false, east: false, south: true, west: false} => "v"
        | {north: false, east: false, south: false, west: true} => "<"
        | {north: true, east: false, south: true, west: false} => "|"
        | {north: false, east: true, south: false, west: true} => "-"
        | {north: true, east: true, south: false, west: false} => "L"
        | _ =>
          Sexp.to_string([%sexp_of: Minecraft.Block.fence_extends](extends))
        },
      )
    | _ => None
    };

  let show = r => show_region_top_down(~show_block, r);
};

let%expect_test "connects fences" = {
  open Test_helpers;

  let fence_post =
    Minecraft.Block.(Fence(Oak_fence, fence_extends_nowhere, Dry));

  let r = build_test_region();
  let y = Region.height_at(~x=10, ~z=10, r);
  Region.set_block(fence_post, ~x=10, ~y, ~z=9, r);
  Region.set_block(fence_post, ~x=10, ~y, ~z=10, r);
  Region.set_block(fence_post, ~x=11, ~y, ~z=10, r);
  Region.set_block(fence_post, ~x=12, ~y, ~z=10, r);

  show(r) |> print_grid;
  %expect
  {|
    :
    : : :
  |};

  connect_fences(r);

  show(r) |> print_grid;
  %expect
  {|
    v
    L - <
  |};
};