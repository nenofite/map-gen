open! Core_kernel

type t = {width: int; height: int; thickness: int; rotation: int}
[@@deriving bin_io]

let template_of_t t =
  let {width; height; thickness; rotation} = t in
  Site_templates.gate ~width ~height ~thickness ~rotation

let prepare ~x ~z =
  let canon = Canonical_overlay.require () in
  let width = Random.int_incl 4 8 in
  let height = Random.int_incl 5 10 in
  let thickness = Random.int_incl 1 2 in
  let rotation = Random.int_incl 0 3 in
  let t = {width; height; thickness; rotation} in
  let template = template_of_t t in
  if
    Minecraft_converter.template_within_region_boundaries template
      ~canon_side:canon.side
  then Some (t, x, z)
  else None

let apply t ~x ~z ~args =
  let template = template_of_t t in
  let y = Minecraft.Region.height_at ~x ~z args.Minecraft_converter.region in
  Minecraft_template.place_overwrite template ~x ~y ~z args.region ;
  ()
