open Worldgen;

Printexc.record_backtrace(true);

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind base = Base_overlay.overlay(dirt);
  let%bind cavern = Cavern_overlay.overlay(base);
  let%bind biomes = Biome_overlay.overlay(base);
  Overlay.return(biomes);
};

let apply_overlays = Overlay.prepare(135, overlays);

Minecraft_converter.save(~side, ~apply_overlays);