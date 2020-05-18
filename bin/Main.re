open Worldgen;

Printexc.record_backtrace(true);
Random.init(135);

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind base = Base_overlay.overlay(dirt);
  let%bind biomes = Biome_overlay.overlay(base);
  Overlay.return(biomes);
};

let apply_overlays = Overlay.prepare(overlays);

Minecraft_converter.save(~side, ~apply_overlays);