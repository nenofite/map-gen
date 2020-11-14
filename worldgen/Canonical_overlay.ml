open Core_kernel

module Obstacles = Grid.Make0(Bool)

(**
   This is not a true overlay, but rather this defines the canonical data which
   any overlay can read or modify. This encourages looser coupling between
   overlays. For example, an overlay can just read canonical obstacles instead
   of depending on the site overlay.
*)

(**
   Surface elevation. This is the Y value of the highest solid block, ie. the
   dirt and not the grass entity, the ocean floor and not the water's surface
*)
type elevation = int Grid.t
[@@deriving bin_io]

(**
   Anything which cannot (or should not) be walked over or built on. For
   example oceans, rivers, ravines, buildings
*)
type obstacles = bool Grid.t
[@@deriving bin_io]

type t = {
  side: int;
  elevation: elevation;
  obstacles: obstacles;
} [@@deriving bin_io]

(** wherever there is an obstacle in a, it will be added to onto *)
let add_obstacles (a: obstacles) ~(onto: obstacles) = (
  Obstacles.zip_map a onto ~f:(||)
)