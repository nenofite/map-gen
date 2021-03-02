open Core_kernel

let overlays_base = ref "overlays"

let world_level_base = ref "worlds/heightmap"

let overlay s = Filename.concat !overlays_base s

let drawing s = Filename.concat !overlays_base s

let world_level () = !world_level_base
