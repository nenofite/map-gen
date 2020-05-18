let prepare = (side, ()) => {
  Printf.printf("Making dirt heights\n");
  Phase_chain.(
    run_all(
      phase("Init", () => Grid.init(side / 32, (_x, _y) => Random.int(10)))
      @> phase_repeat(
           3,
           "Avg subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(random_avg)),
         )
      @> phase_repeat(
           2,
           "Line subdivide",
           Subdivide.subdivide_with_fill(_, Fill.(line() **> random_avg)),
         ),
      /* @> Draw.phase("dirt.ppm", i => i * 20 * 0x010101), */
    )
  );
};

let overlay = side =>
  Overlay.make("dirt height", prepare(side), (_, _) => ());