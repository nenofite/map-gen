open Core_kernel;
open Worldgen;

let default_seed = 123;
let get_seed = () => {
  switch (Sys.argv) {
  | [|_cmd, seed|] =>
    switch (int_of_string_opt(seed)) {
    | Some(seed) => seed
    | None => default_seed
    }
  | _ => default_seed
  };
};

Printexc.record_backtrace(true);
Stats.init();

/* let side = 16_384; */
/* let side = 8_192; */
let side = 4_096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind (base, canon) = Base_overlay.overlay(side);
  let%bind (biomes, canond) = Biome_overlay.overlay(canon, base, dirt);
  let canon = Canonical_overlay.apply_delta(canond, ~onto=canon);
  let%bind _ores = Ore_overlay.overlay(base);
  let%bind _cavern = Cavern_overlay.overlay(base);
  let%bind (_caves, canond) = Cave_overlay.overlay(canon);
  let canon = Canonical_overlay.apply_delta(canond, ~onto=canon);
  let%bind (towns, canond) = Town_overlay.overlay(canon, base);
  let canon = Canonical_overlay.apply_delta(canond, ~onto=canon);
  let%bind (_roads, canond) = Road_overlay.overlay(canon, towns);
  let canon = Canonical_overlay.apply_delta(canond, ~onto=canon);
  /* let%bind _sites = Site_overlay.overlay(canon, cavern); */
  let%bind _plants = Plant_overlay.overlay(biomes);
  let%bind _debug =
    Debug_overlay.overlay(
      canon,
      {
        glassify:
          fun
          /* | Air => false
             | Stone
             | Dirt
             | Grass_block => true */
          | _ => false,
        illuminate: false,
      },
    );
  let spawn_point = Mg_util.shuffle(canon.spawn_points) |> List.hd_exn;
  Overlay.return(spawn_point);
};

Progress_view.init();
let seed = get_seed();
Tale.logf("Using seed %d", seed);
let (spawn, apply_overlays) = Overlay.prepare(seed, overlays);

Minecraft_converter.save(~side, ~spawn, ~apply_overlays);

Stats.finalize();