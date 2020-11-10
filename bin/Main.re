open Worldgen;

Printexc.record_backtrace(true);
Stats.init();

let side = 4096;

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind dirt = Dirt_overlay.overlay(side);
  let%bind (base, canon) = Base_overlay.overlay;
  let%bind (biomes, canon) = Biome_overlay.overlay(canon, base, dirt);
  let%bind _ores = Ore_overlay.overlay(base);
  let%bind _cavern = Cavern_overlay.overlay(base);
  let%bind (_caves, canon) = Cave_overlay.overlay(canon);
  let%bind (towns, canon) = Town_overlay.overlay(canon, base);
  let%bind (_roads, _canon) = Road_overlay.overlay(canon, towns);
  /* let%bind _sites = Site_overlay.overlay(canon, cavern); */
  let%bind _plants = Plant_overlay.overlay(biomes);
  let%bind _debug =
    Debug_overlay.overlay(
      canon,
      {
        glassify: _ => false,
        /* fun
           | Air => false
           | Stone
           | Dirt
           | Grass => true
           | _ => false, */
        illuminate: true,
      },
    );
  Overlay.return();
};

Progress_view.init();
let apply_overlays = Overlay.prepare(135, overlays);

Minecraft_converter.save(~side, ~apply_overlays);

Stats.finalize();