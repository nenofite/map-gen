open Worldgen;

Printexc.record_backtrace(true);
Stats.init();

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind base = Base_overlay.overlay;
  let%bind biomes = Biome_overlay.overlay(base, dirt);
  let%bind _ores = Ore_overlay.overlay(base);
  let%bind cavern = Cavern_overlay.overlay(base);
  let%bind roads = Road_overlay.overlay(base);
  let%bind _towns = Town_overlay.overlay(base, roads);
  let%bind _sites = Site_overlay.overlay(base, cavern);
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

Progress_view.init();
let apply_overlays = Overlay.prepare(135, overlays);

Minecraft_converter.save(~side, ~apply_overlays);

Stats.finalize();