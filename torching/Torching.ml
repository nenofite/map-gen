open Core_kernel
module Range = Mg_util.Range
module Geometry = Mg_util.Geometry

let torch_light_level = 14

(* Common blocks that take memory *)
let dry_fence = Minecraft.Block.Oak_fence Dry

let waterlogged_fence = Minecraft.Block.Oak_fence Waterlogged

(* Helpers *)
let is_air = Minecraft.Block.is_air

let is_air_opt = function Some block -> is_air block | _ -> false

let is_water = function
  | Minecraft.Block.Water | Flowing_water _ ->
      true
  | _ ->
      false

let is_water_opt = function Some block -> is_water block | _ -> false

let is_solid = Minecraft.Block.is_solid

let is_opaque = Minecraft.Block.is_opaque

let is_solid_opt = function Some b -> is_solid b | _ -> false

module Preference = struct
  type t = Wall | Torch_with_space | Torch_no_space | Water_post | Untorchable
  [@@deriving eq, ord, sexp]

  let is_wall_at ~x ~y ~z region =
    let open Minecraft.Region in
    is_solid_opt (get_block_opt ~x ~y ~z region)
    && is_solid_opt (get_block_opt ~x ~y:(y + 1) ~z region)

  (** tries preferences from most to least preferred and returns the first
      which can be applied here. The coord is that of the Air above a solid
      block. *)
  let highest_land_preference_at ~x ~y ~z region =
    let open Minecraft.Region in
    (*
        - X
        = X
        X
        *)
    if
      is_air_opt (get_block_opt ~x ~y:(y + 1) ~z region)
      && ( is_wall_at ~x ~y ~z:(z - 1) region
         || is_wall_at ~x:(x - 1) ~y ~z region
         || is_wall_at ~x ~y ~z:(z + 1) region
         || is_wall_at ~x:(x + 1) ~y ~z region )
    then Wall (*
    - = -
      X
    *)
    else if
      Range.for_all (z - 1) (z + 1) (fun z ->
          Range.for_all (x - 1) (x + 1) (fun x ->
              is_air_opt (get_block_opt ~x ~y ~z region) ) )
    then Torch_with_space (*
    =
    X
    *)
    else Torch_no_space

  let highest_water_preference_at ~x ~y ~z region =
    let open Minecraft.Region in
    (*
    =
    i
    ~
    X
    *)
    if
      is_air_opt (get_block_opt ~x ~y:(y + 1) ~z region)
      && is_air_opt (get_block_opt ~x ~y:(y + 2) ~z region)
    then Water_post
    else Untorchable

  let highest_preference_at ~x ~y ~z region =
    let open Minecraft.Region in
    let surface = get_block_opt ~x ~y ~z region in
    if is_water_opt surface then highest_water_preference_at ~x ~y ~z region
    else if is_air_opt surface then highest_land_preference_at ~x ~y ~z region
    else Untorchable

  (** attempts to apply this preference. Returns the coord that a torch was
      placed at, since sometimes it differs from the given coord *)
  let apply_at ~x ~y ~z region t =
    let open Minecraft.Region in
    match t with
    | Wall ->
        let torch_coord = Some (x, y + 1, z) in
        if is_wall_at ~x ~y ~z:(z - 1) region then (
          set_block_opt (Wall_torch S) ~x ~y:(y + 1) ~z region ;
          torch_coord )
        else if is_wall_at ~x:(x + 1) ~y ~z region then (
          set_block_opt (Wall_torch W) ~x ~y:(y + 1) ~z region ;
          torch_coord )
        else if is_wall_at ~x ~y ~z:(z + 1) region then (
          set_block_opt (Wall_torch N) ~x ~y:(y + 1) ~z region ;
          torch_coord )
        else if is_wall_at ~x:(x - 1) ~y ~z region then (
          set_block_opt (Wall_torch E) ~x ~y:(y + 1) ~z region ;
          torch_coord )
        else None
    | Torch_with_space | Torch_no_space ->
        set_block Torch ~x ~y ~z region ;
        Some (x, y, z)
    | Water_post ->
        set_block_opt waterlogged_fence ~x ~y ~z region ;
        set_block_opt dry_fence ~x ~y:(y + 1) ~z region ;
        set_block_opt Torch ~x ~y:(y + 2) ~z region ;
        Some (x, y + 2, z)
    | Untorchable ->
        None
end

module Priority = struct
  module T = struct
    type t = Preference.t * int [@@deriving eq, ord, sexp]
  end

  include T
  include Comparable.Make (T)

  let with_random pref = (pref, Random.bits ())
end

let spread_light_into ~x ~y ~z ~level light_levels region acc =
  let module Vi = Geometry.Vec3i in
  let xyz = (x, y, z) in
  if
    (not (is_solid_opt (Minecraft.Region.get_block_opt ~x ~y ~z region)))
    && Option.value (Vi.Table.find light_levels xyz) ~default:0 < level
  then (level, xyz) :: acc
  else acc

let mark_torch_light ~x ~y ~z light_levels region =
  let module Vi = Geometry.Vec3i in
  Grid_flood.flood_gen ~init:()
    ~live:[(torch_light_level, [(x, y, z)])]
    ~spread:(fun () ~level ((x, y, z) as xyz) ->
      if level > 0 then Vi.Table.set light_levels ~key:xyz ~data:level ;
      let next_level = level - 1 in
      let next_live =
        []
        |> (*u*)
        spread_light_into ~x ~y:(y + 1) ~z ~level:next_level light_levels region
        |> (*N*)
        spread_light_into ~x ~y ~z:(z - 1) ~level:next_level light_levels region
        |> (*E*)
        spread_light_into ~x:(x + 1) ~y ~z ~level:next_level light_levels region
        |> (*S*)
        spread_light_into ~x ~y ~z:(z + 1) ~level:next_level light_levels region
        |> (*W*)
        spread_light_into ~x:(x - 1) ~y ~z ~level:next_level light_levels region
        |> (*d*)
        spread_light_into ~x ~y:(y - 1) ~z ~level:next_level light_levels region
      in
      ((), next_live) )

let iter_with_progress ?every arr ~f =
  let total = Array.length arr in
  let every = match every with Some n -> n | None -> Int.max (total / 20) 1 in
  let prog i total =
    Printf.printf "\r%d of %d..." i total ;
    Out_channel.flush stdout
  in
  Array.iteri arr ~f:(fun i n ->
      if Int.(i % every = 0) then prog i total ;
      f n ) ;
  prog total total ;
  Out_channel.newline stdout

let illuminate ?(min_brightness = 8) ~volume region =
  Tale.block "Illuminating" ~f:(fun () ->
      let module Vi = Geometry.Vec3i in
      let add_surface_from_volume ((torches, surfaces) as acc) ((x, y, z) as xyz)
          =
        let open Minecraft.Region in
        let here = get_block ~x ~y ~z region in
        let below = get_block_opt ~x ~y:(y - 1) ~z region in
        match (here, below) with
        | (Torch | Wall_torch _), _ ->
            (xyz :: torches, surfaces)
        | here, Some below
          when is_solid below && is_opaque below
               && (is_air here || is_water here) ->
            let preference = Preference.highest_preference_at ~x ~y ~z region in
            let priority = Priority.with_random preference in
            (torches, (priority, xyz) :: surfaces)
        | _ ->
            acc
      in
      Tale.log "Adding torches and surfaces" ;
      let torches, surfaces =
        volume ~init:([], []) ~f:add_surface_from_volume
      in
      Tale.log "Marking pre-existing light levels" ;
      (* TODO tune initial size *)
      let light_levels = Vi.Table.create ~size:(List.length surfaces) () in
      List.iter torches ~f:(fun (x, y, z) ->
          mark_torch_light ~x ~y ~z light_levels region ) ;
      Tale.log "Sorting" ;
      let compare_by_first (a, _) (b, _) = Priority.compare a b in
      let sorted_surfaces = Array.of_list surfaces in
      Array.sort sorted_surfaces ~compare:compare_by_first ;
      Tale.log "Placing torches" ;
      iter_with_progress ~every:100 sorted_surfaces
        ~f:(fun ((preference, _random), ((x, y, z) as xyz)) ->
          if
            Option.value (Vi.Table.find light_levels xyz) ~default:0
            < min_brightness
          then
            match Preference.apply_at ~x ~y ~z region preference with
            | Some (x, y, z) ->
                mark_torch_light ~x ~y ~z light_levels region
            | None ->
                () ) )

let illuminate_bounds ?min_brightness ~x ~y ~z region =
  let xmin, xmax = x in
  let ymin, ymax = y in
  let zmin, zmax = z in
  let volume ~init ~f =
    let open Mg_util.Range in
    fold zmin zmax init (fun acc z ->
        fold xmin xmax acc (fun acc x ->
            fold ymin ymax acc (fun acc y -> f acc (x, y, z)) ) )
  in
  illuminate ?min_brightness ~volume region
