open Core_kernel


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
type obstacle = Clear | Bridgeable | Impassable
[@@deriving eq, ord, bin_io]

module Obstacles = Grid.Make0(struct
    type t = obstacle
    let (=) = equal_obstacle
  end)

type obstacles = obstacle Grid.t
[@@deriving bin_io]

type delta = {
  elevation: [`Unchanged | `Replace of elevation];
  obstacles: [`Unchanged | `Add of obstacles | `Replace of obstacles];
} [@@deriving bin_io]

type t = {
  side: int;
  elevation: elevation;
  obstacles: obstacles;
} [@@deriving bin_io]

let make_delta ?(elevation = `Unchanged) ?(obstacles = `Unchanged) () = { elevation; obstacles }

(** wherever there is an obstacle in a, it will be added to onto *)
let add_obstacles (a: obstacles) ~(onto: obstacles) = (
  let f a onto = if compare_obstacle a onto > 0 then a else onto in
  Obstacles.zip_map a onto ~f
)

(** applies the changes described by delta to get a new, full overlay *)
let apply_delta (delta: delta) ~(onto: t) = (
  {
    side = onto.side;
    elevation =
      (match delta.elevation with
       | `Unchanged -> onto.elevation
       | `Replace e -> e)
    ;
    obstacles = (match delta.obstacles with
        | `Unchanged -> onto.obstacles
        | `Add additions -> add_obstacles additions ~onto:onto.obstacles
        | `Replace o -> o)
    ;
  }
)