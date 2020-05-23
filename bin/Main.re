open Worldgen;

Printexc.record_backtrace(true);

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind base = Base_overlay.overlay;
  let%bind biomes = Biome_overlay.overlay(base, dirt);
  let%bind _ores = Ore_overlay.overlay(base);
  let%bind _cavern = Cavern_overlay.overlay(base);
  let%bind _roads = Road_overlay.overlay(base);
  let%bind _plants = Plant_overlay.overlay(biomes);
  /* let%bind debug =
     Debug_overlay.overlay({
       glassify:
         fun
         | Air => false
         | Stone
         | Dirt
         | Grass => true
         | _ => false,
     }); */
  Overlay.return();
};

let apply_overlays = Overlay.prepare(135, overlays);

Minecraft_converter.save(~side, ~apply_overlays);