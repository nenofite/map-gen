open! Core;
open Core_bench;

let prep_wave = size => {
  Random.init(123);
  let wave =
    Wave_collapse.make_blank_wave(
      Wave_building.ts,
      ~xs=size,
      ~ys=2,
      ~zs=size,
    );
  for (x in 0 to size - 1) {
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=0, 0);
    Wave_collapse.force_and_propagate(wave, ~x, ~y=0, ~z=size - 1, 0);
  };
  for (z in 0 to size - 1) {
    Wave_collapse.force_and_propagate(wave, ~x=0, ~y=0, ~z, 0);
    Wave_collapse.force_and_propagate(wave, ~x=size - 1, ~y=0, ~z, 0);
  };
  Wave_collapse.try_collapse_next_lowest_entropy(wave) |> ignore;
  Wave_collapse.try_collapse_next_lowest_entropy(wave) |> ignore;
  Wave_collapse.try_collapse_next_lowest_entropy(wave) |> ignore;
  wave;
};

let just_copy = size => {
  let wave = prep_wave(size);
  Staged.stage(() => {
    let wave = Wave_collapse.copy(wave);
    wave;
  });
};

let collapse = size => {
  let wave = prep_wave(size);
  Staged.stage(() => {
    let wave = Wave_collapse.copy(wave);
    Wave_collapse.try_collapse_next_lowest_entropy(wave) |> ignore;
    wave;
  });
};

Command_unix.run(
  Bench.make_command([
    Bench.Test.create_indexed(
      ~name="collapse",
      //   ~args=[5, 10, 15, 20],
      ~args=[10, 20],
      collapse,
    ),
    Bench.Test.create_indexed(
      ~name="just copy",
      //   ~args=[5, 10, 15, 20],
      ~args=[10, 20],
      just_copy,
    ),
  ]),
);