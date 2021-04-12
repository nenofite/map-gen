open! Core_kernel

type t = {rotation_cw: int; template: Minecraft_template.t} [@@deriving bin_io]

let max_stair_distance = 20

let random_gate () =
  let width = Random.int_incl 4 8 in
  let height = Random.int_incl 5 10 in
  let thickness = Random.int_incl 1 2 in
  let open Minecraft_template in
  let mat = Minecraft.Block.Obsidian in
  let beam =
    rect mat ~xs:(width + (thickness * 2)) ~ys:thickness ~zs:thickness
  in
  let post = rect mat ~xs:thickness ~ys:height ~zs:thickness in
  let base = beam |> align_with_origin ~my:(X center, Y max, Z center) in
  let left_post =
    post
    |> align_with ~other:base ~my:(X min, Y min, Z min)
         ~their:(X min, Y max, Z min)
  in
  let right_post =
    post
    |> align_with ~other:base ~my:(X max, Y min, Z min)
         ~their:(X max, Y max, Z min)
  in
  let top =
    beam
    |> align_with ~other:left_post ~my:(X min, Y min, Z min)
         ~their:(X min, Y max, Z min)
  in
  combine_all [base; left_post; right_post; top]
  |> Minecraft_template.Effects.(
       if thickness = 1 then eat ~blocks:(Random.int_incl 0 1)
       else eat_frac ~frac:(Random.float 0.25))

let building template =
  let open Building.Building_monad.Let_syntax in
  let minx, maxx = template.Minecraft_template.bounds_x in
  let minz, maxz = template.Minecraft_template.bounds_z in
  let%bind y = Building.Building_monad.height_at ~x:0 ~z:0 in
  let y = y + 3 in
  let%bind () =
    Building.Foundation.lay_stair_foundation
      ~foundation:Minecraft.Block.Smooth_quartz
      ~stair:(fun d -> Minecraft.Block.Quartz_stairs d)
      ~e:false ~w:false ~minx ~maxx ~y ~minz ~maxz ~max_stair:max_stair_distance
      ()
  in
  Building.Building_monad.place_template ~x:0 ~y ~z:0 template

let can_build_template template ~x ~z =
  let canon = Overlay.Canon.require () in
  let minx, maxx = template.Minecraft_template.bounds_x in
  let minz, maxz = template.bounds_z in
  Range.for_all (z + minz) (z + maxz) (fun z ->
      Range.for_all (x + minx) (x + maxx) (fun x ->
          Overlay.Canon.can_build_on (Grid.get x z canon.obstacles) ) )

let prepare ~x ~z =
  let rotation_cw = Random.int_incl 0 3 in
  let template = random_gate () in
  match
    Building.Building_monad.run_prepare (building template)
      ~pos:(Building.Shared.at ~x ~y:0 ~z ~rotation_cw)
  with
  | Ok ((), canond) ->
      (* TODO *)
      ignore canond ;
      Tale.logf "Placed gate at %d,%d" x z ;
      Some ({rotation_cw; template}, x, z)
  | Error _s ->
      (* let s = Lazy.force s in
         Tale.logf "Couldn't place gate: %s" s ; *)
      None

let put_obstacles (t : t) ~x ~z ~put =
  List.iter t.template.footprint ~f:(fun (fx, fz) ->
      put Overlay.Canon.Obstacle.Impassable ~x:(x + fx) ~z:(z + fz) ) ;
  ()

let stair_dirs ~rotation =
  if rotation mod 2 = 0 then (true, false, true, false)
  else (false, true, false, true)

let apply (t : t) ~x ~z ~region =
  let {rotation_cw; template} = t in
  Building.Building_monad.run_apply (building template) ~region
    ~pos:(Building.Shared.at ~x ~y:0 ~z ~rotation_cw)
