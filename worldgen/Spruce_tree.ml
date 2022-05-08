open! Core

let log = Minecraft.Block.(Log (Spruce_log, Y))

let leaves = Minecraft.Block.Spruce_leaves

let disc radius =
  let open Minecraft_template in
  let d = (radius * 2) + 1 in
  let l =
    rect leaves ~xs:d ~ys:1 ~zs:d
    |> clear_at ~x:0 ~y:0 ~z:0
    |> clear_at ~x:0 ~y:0 ~z:(d - 1)
    |> clear_at ~x:(d - 1) ~y:0 ~z:0
    |> clear_at ~x:(d - 1) ~y:0 ~z:(d - 1)
  in
  translate l (-radius) 0 (-radius)

let rec cone radius =
  if radius > 1 then Minecraft_template.stack (disc radius) (cone (radius - 1))
  else disc radius

let rec multicone count ~radius =
  if count > 1 then
    Minecraft_template.stack (cone radius) (multicone (count - 1) ~radius)
  else cone radius

let add_core t =
  let open Minecraft_template in
  combine t (rect log ~xs:1 ~ys:(height_of t - 1) ~zs:1)

let tree ~extra_trunk ~cone_count =
  let open Minecraft_template in
  let one_log = rect log ~xs:1 ~ys:1 ~zs:1 in
  let trunk = rect log ~xs:1 ~ys:(cone_count + extra_trunk) ~zs:1 in
  let cones = multicone cone_count ~radius:3 in
  let top_cone = cone 2 in
  let top = disc 1 in
  stack_all [trunk; cones; top_cone; one_log; top] |> add_core

let random_tree () =
  let extra_trunk = Random.int_incl 0 1 in
  let cone_count = Random.int_incl 1 3 in
  tree ~extra_trunk ~cone_count
