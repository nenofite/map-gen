open Worldgen;

Printexc.record_backtrace(true);

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind base = Base_overlay.overlay(dirt);
  let%bind ores = Ore_overlay.overlay(base);
  let%bind cavern = Cavern_overlay.overlay(base);
  let%bind roads = Road_overlay.overlay(base);
  let%bind biomes = Biome_overlay.overlay(base);
  let%bind debug =
    Debug_overlay.overlay({
      glassify:
        fun
        | Air
        | Iron_ore
        | Diamond_ore => false
        | _ => true,
    });
  Overlay.return();
};

let apply_overlays = Overlay.prepare(135, overlays);

Minecraft_converter.save(~side, ~apply_overlays);