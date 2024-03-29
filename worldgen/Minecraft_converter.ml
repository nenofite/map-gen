open Core

let region_of_spawn (x, _y, z) = Minecraft.Region.region_containing ~x ~z

(**
  segment_grid_by_region divides the grid into squares such that each square
  becomes one Minecraft Anvil region. Then it calls f with the region
  coordinates.

  If the grid does not divide evenly into regions, then this raises
  Invalid_argument. To avoid this, the each grid dimension must divide evenly
  by 512.

  sub is an optional range of regions to limit the export, eg. ((2, 3), (1,
  1)) to export a 1x1 slice of regions starting with region r.2.3
  *)
let segment_grid_by_region ~(side : int) ~spawn ?sub () =
  let block_per_region = Minecraft.Region.block_per_region_side in
  (* The grid must split evenly into regions *)
  ( if side mod block_per_region <> 0 then
    let msg =
      Printf.sprintf "grid does not divide evenly into regions with grid %dx%d"
        side side
    in
    raise (Invalid_argument msg) ) ;
  (* Calculate how many tiles form the side of a region *)
  let regions_side = side / block_per_region in
  Tale.logf "Grid divided into %dx%d regions" regions_side regions_side ;
  let (min_rx, min_rz), (len_rx, len_rz) =
    Option.value sub ~default:((0, 0), (regions_side, regions_side))
  in
  (* List regions and sort by distance to spawn *)
  let spawn_rx, spawn_rz = region_of_spawn spawn in
  let dist_to_spawn (rx, rz) =
    let open Int in
    ((rx - spawn_rx) ** 2) + ((rz - spawn_rz) ** 2)
  in
  let compare_by_spawn_dist a b =
    Int.compare (dist_to_spawn a) (dist_to_spawn b)
  in
  let regions =
    Range.fold 0 (regions_side - 1) [] (fun acc rz ->
        Range.fold 0 (regions_side - 1) acc (fun acc rx -> (rx, rz) :: acc) )
    |> List.sort ~compare:compare_by_spawn_dist
  in
  (* Iterate over the grid *)
  List.filter regions ~f:(fun (rx, rz) ->
      min_rx <= rx
      && rx < min_rx + len_rx
      && min_rz <= rz
      && rz < min_rz + len_rz )

(** fills the region with blocks from the grid *)
let convert_region ~region ~apply_overlays : unit =
  apply_overlays region ;
  Tale.logf "Flowing water" ;
  Out_channel.flush stdout ;
  Minecraft_postprocessing.run_all region

(** save creates a Minecraft world with the given heightmap *)
let save ~(side : int) ~(spawn : int * int * int)
    ~(apply_overlays : Minecraft.Region.t -> unit) : unit =
  let builder = Minecraft.World.make "heightmap" ~spawn () in
  let all_regions = segment_grid_by_region ~side ~spawn () in
  let regions =
    match Config.Force.max_regions () with
    | None ->
        all_regions
    | Some n ->
        List.take all_regions n
  in
  let make_region (rx, rz) =
    try
      Minecraft.World.make_region ~rx ~rz builder (fun region ->
          let min_x, min_z = Minecraft.Region.region_offset region in
          Progress_view.fit
            ~title:(Printf.sprintf "region r.%d.%d" rx rz)
            (let open Minecraft.Region in
            ( min_x
            , min_x + block_per_region_side
            , min_z
            , min_z + block_per_region_side )) ;
          convert_region ~region ~apply_overlays )
    with exn ->
      Printexc.print_backtrace stderr ;
      raise exn
  in
  Parmap.pariter ~ncores:4 ~chunksize:1 make_region (L regions) ;
  Tale.log "Finished parallel apply" ;
  ()

let iter_blocks (r : Minecraft.Region.t) fn : unit =
  Minecraft.Region.iter_region_xz ~f:fn r

let template_within_region_boundaries t ~x ~z ~canon_side =
  let open Minecraft_template in
  let min_x, max_x = t.bounds_x in
  let min_z, max_z = t.bounds_z in
  Minecraft.Region.within_region_boundaries ~canon_side ~min_x:(min_x + x)
    ~max_x:(max_x + x) ~min_z:(min_z + z) ~max_z:(max_z + z)
