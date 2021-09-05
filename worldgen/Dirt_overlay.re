open Core_kernel;

module Dirt_grid = Grid.Make0(Int);

let max_depth = 9;

[@deriving bin_io]
type t = Grid.t(int);

let prepare = () => {
  Tale.block("Making dirt heights", ~f=() => {
    Tale.log("Init");
    let side = Overlay.Canon.require().side;
    let m =
      Grid.Mut.init(
        ~side=side / 32, ~alloc_side=side, 0, ~f=(~x as _, ~z as _) => {
        Random.int(max_depth + 1)
      });
    for (i in 1 to 3) {
      Tale.logf("Avg subdivide %d", i);
      Grid.Subdivide_mut.subdivide_with_fill(m, ~fill=Grid.Fill.random_avg);
    };
    let line_fill = Grid.Fill.(line(~eq=Int.(==), ()) **> random_avg);
    for (i in 1 to 2) {
      Tale.logf("Line subdivide %d", i);
      Grid.Subdivide_mut.subdivide_with_fill(m, ~fill=line_fill);
    };
    assert(Grid.Mut.side(m) == side);
    Dirt_grid.of_mut(m);
  });
};

let (require, prepare, apply) =
  Overlay.make_no_canon(
    "dirt height",
    prepare,
    (_, _) => (),
    bin_reader_t,
    bin_writer_t,
  );
