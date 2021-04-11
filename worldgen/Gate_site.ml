open! Core_kernel

type t = {rotation: int; template: Minecraft_template.t} [@@deriving bin_io]

let random_gate ~rotation =
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
  |> rotate_90_cw ~times:rotation
  |> Minecraft_template.Effects.(
       if thickness = 1 then eat ~blocks:(Random.int_incl 0 1)
       else eat_frac ~frac:(Random.float 0.25))

let can_build_template template ~x ~z =
  let canon = Overlay.Canon.require () in
  let minx, maxx = template.Minecraft_template.bounds_x in
  let minz, maxz = template.bounds_z in
  Range.for_all (z + minz) (z + maxz) (fun z ->
      Range.for_all (x + minx) (x + maxx) (fun x ->
          Overlay.Canon.can_build_on (Grid.get x z canon.obstacles) ) )

let prepare ~x ~z =
  let canon = Overlay.Canon.require () in
  let rotation = Random.int_incl 0 3 in
  let template = random_gate ~rotation in
  if
    Minecraft_converter.template_within_region_boundaries template ~x ~z
      ~canon_side:canon.side
    && can_build_template template ~x ~z
  then Some ({rotation; template}, x, z)
  else None

let put_obstacles (t : t) ~x ~z ~put =
  List.iter t.template.footprint ~f:(fun (fx, fz) ->
      put Overlay.Canon.Obstacle.Impassable ~x:(x + fx) ~z:(z + fz) ) ;
  ()

let stair_dirs ~rotation =
  if rotation mod 2 = 0 then (true, false, true, false)
  else (false, true, false, true)

let apply (t : t) ~x ~z ~region =
  let {rotation; template} = t in
  let minx, maxx = template.bounds_x in
  let minz, maxz = template.bounds_z in
  let y = 3 + Minecraft.Region.height_at ~x ~z region in
  let n, e, s, w = stair_dirs ~rotation in
  Building.stair_foundation ~rectangle_material:Minecraft.Block.Smooth_quartz
    ~stair_material:(fun d -> Minecraft.Block.Quartz_stairs d)
    ~n ~e ~s ~w ~minx:(minx + x) ~maxx:(maxx + x) ~y:(y - 1) ~minz:(minz + z)
    ~maxz:(maxz + z) region ;
  Minecraft_template.place_overwrite template ~x ~y ~z region ;
  ()
