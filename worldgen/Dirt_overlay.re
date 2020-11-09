open Core_kernel;

let max_depth = 9;

[@deriving bin_io]
type t = Grid.t(int);

let prepare = (side, ()) => {
  Tale.logf("Making dirt heights");
  Phase_chain.(
    run_all(
      phase("Init", () =>
        Grid_compat.init(side / 32, (_x, _y) =>
          /* TODO */ Caml.Random.int(max_depth + 1)
        )
      )
      @> phase_repeat(
           3,
           "Avg subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(random_avg)),
         )
      @> phase_repeat(
           2,
           "Line subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(line() **> random_avg)),
         )
      @> Draw.phase("dirt.png", i => i * 20 * 0x010101),
    )
  );
};

let overlay = side =>
  Overlay.make(
    "dirt height",
    prepare(side),
    (_, _) => (),
    bin_reader_t,
    bin_writer_t,
  );