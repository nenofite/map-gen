open Core_kernel;

module Dirt_grid = Grid.Make0(Int);

let max_depth = 9;

[@deriving bin_io]
type t = Grid.t(int);

let prepare = () => {
  Tale.block("Making dirt heights", ~f=() => {
    Tale.log("Init");
    let side = Canonical_overlay.require().side;
    let m =
      Grid.Mut.init(
        ~side=side / 32, ~alloc_side=side, 0, ~f=(~x as _, ~z as _) => {
        Random.int(max_depth + 1)
      });
    for (i in 1 to 3) {
      Tale.logf("Avg subdivide %d", i);
      Subdivide_mut.subdivide_with_fill(m, ~fill=Fill.random_avg);
    };
    let line_fill = Fill.(line(~eq=Int.(==), ()) **> random_avg);
    for (i in 1 to 2) {
      Tale.logf("Line subdivide %d", i);
      Subdivide_mut.subdivide_with_fill(m, ~fill=line_fill);
    };
    assert(Grid.Mut.side(m) == side);
    Draw.draw_griddable(
      Grid.Mut.intf0(m),
      ~f=i => i * 20 * 0x010101,
      ~file="dirt.bmp",
      m,
    );
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
