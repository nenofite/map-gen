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
type elevation = int Grid.t [@@deriving bin_io]

(**
   Anything which cannot (or should not) be walked over or built on. For
   example oceans, rivers, ravines, buildings
*)
module Obstacle = struct
  module T = struct
    type t = Clear | Bridgeable | Impassable
    [@@deriving eq, ord, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
end

type obstacle = Obstacle.t = Clear | Bridgeable | Impassable
[@@deriving eq, ord, bin_io]

module Obstacles = Grid.Make0 (Obstacle)

type obstacles = obstacle Grid.t [@@deriving bin_io]

type delta =
  { elevation: [`Unchanged | `Replace of elevation]
  ; obstacles: [`Unchanged | `Add of obstacles | `Replace of obstacles]
  ; spawn_points: [`Unchanged | `Add of (int * int * int) list] }
[@@deriving bin_io]

type t =
  { side: int
  ; elevation: elevation
  ; obstacles: obstacles
  ; spawn_points: (int * int * int) list }
[@@deriving bin_io]

let make_delta ?(elevation = `Unchanged) ?(obstacles = `Unchanged)
    ?(spawn_points = `Unchanged) () =
  {elevation; obstacles; spawn_points}

(** wherever there is an obstacle in a, it will be added to onto *)
let add_obstacles (a : obstacles) ~(onto : obstacles) =
  let f a onto = if compare_obstacle a onto > 0 then a else onto in
  Obstacles.zip_map a onto ~f

(** applies the changes described by delta to get a new, full overlay *)
let apply_delta (delta : delta) ~(onto : t) =
  { side= onto.side
  ; elevation=
      ( match delta.elevation with
      | `Unchanged ->
          onto.elevation
      | `Replace e ->
          e )
  ; obstacles=
      ( match delta.obstacles with
      | `Unchanged ->
          onto.obstacles
      | `Add additions ->
          add_obstacles additions ~onto:onto.obstacles
      | `Replace o ->
          o )
  ; spawn_points=
      ( match delta.spawn_points with
      | `Unchanged ->
          onto.spawn_points
      | `Add sp ->
          sp @ onto.spawn_points ) }

(* Obstacle helpers *)
let can_build_on = function Clear -> true | Bridgeable | Impassable -> false

let can_build_over = function Clear | Bridgeable -> true | Impassable -> false

let state = ref None

let init ~side =
  state :=
    Some
      { side
      ; elevation= Grid.make ~side 0
      ; obstacles= Grid.make ~side Clear
      ; spawn_points= [] } ;
  ()

let require () =
  match !state with
  | Some s ->
      s
  | None ->
      failwith "Canonical_overlay.init has not been called"

let restore s = state := Some s

let push_delta delta =
  let s = require () in
  state := Some (apply_delta delta ~onto:s)

let draw_obstacles () =
  let s = require () in
  let l = Progress_view.push_layer () in
  Progress_view.update
    ~draw_dense:(fun () x z ->
      if Grid.is_within x z s.obstacles then
        match Grid.get x z s.obstacles with
        | Impassable ->
            Some (255, 0, 0)
        | Bridgeable | Clear ->
            None
      else None)
    ~state:() l ;
  l
