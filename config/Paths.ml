open Core

let overlays_base = ref "overlays"

let world_level_base = ref "worlds/heightmap"

let overlay s = Filename.concat !overlays_base s

let drawing s = Filename.concat !overlays_base s

let world_level () = !world_level_base

let create_directories () =
  Mg_util.mkdir (overlay "") ;
  Mg_util.mkdir (world_level ())
