open Worldgen;

Printexc.record_backtrace(true);
Random.init(135);

let world =
  Phase_chain.(
    run_all(
      Tectonic.phase
      @> Heightmap.phase
      @> Draw.phase("grid-height.ppm", Heightmap.colorize)
      /* @@> Erosion.phase
         @@> draw_phase("grid-erosion.ppm", Erosion.colorize), */
      @> River.phase
      @> Draw.phase("grid-river.ppm", River.colorize)
      @> Sites.phase
      @> Draw.phase("grid-sites.ppm", River.colorize),
    )
  );

let dirt = Dirt_height.make(world.side);

let overlays = {
  module Let_syntax = Overlay.Let_syntax;
  let%bind biomes = Biome_overlay.overlay(world);
  Overlay.return(biomes);
};

let apply_overlays = Overlay.prepare(overlays);

Minecraft_converter.save(world, ~dirt, ~apply_overlays);