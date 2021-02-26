open Core_kernel
open Minecraft.Block

(**
   This module generates an oak tree, optionally with a bee nest in it.
   The tree has this structure, looking from the side:

    ###
    #|#
   ##|## } wide_leaves_height
   ##|## } 
     |O } <- optional bee nest
     |  }
     |  } trunk_height
*)

let trunk ~with_bees ~height =
  let height_before_bees = if with_bees then height - 1 else height in
  let log_before_bees =
    Minecraft_template.of_blocks
    @@ Range.map 0 (height_before_bees - 1) (fun y -> (0, y, 0, Oak_log Y))
  in
  (* TODO put bee nest on random side; vary amount of bees *)
  let log_with_bees =
    Minecraft_template.of_blocks [(0, 0, 0, Oak_log Y); (1, 0, 0, Bee_nest 3)]
  in
  if with_bees then Minecraft_template.stack log_before_bees log_with_bees
  else log_before_bees

let wide_leaves ~height =
  let layer ls y =
    Range.fold (-2) 2 ls (fun ls z ->
        Range.fold (-2) 2 ls (fun ls x ->
            ( if x = 0 && z = 0 then (x, y, z, Oak_log Y)
            else (x, y, z, Oak_leaves) )
            :: ls))
  in
  let blocks = Range.fold 0 (height - 1) [] layer in
  Minecraft_template.of_blocks blocks

let top =
  let l = Oak_leaves in
  let o = Oak_log Y in
  Minecraft_template.of_blocks
    [ (* empty ; *)
      (0, 1, -1, l)
    ; (* empty ; *)
      (-1, 1, 0, l)
    ; (0, 1, 0, l)
    ; (1, 1, 0, l)
    ; (* empty ; *)
      (0, 1, 1, l)
    ; (* empty ; *)
      (-1, 0, -1, l)
    ; (0, 0, -1, l)
    ; (1, 0, -1, l)
    ; (-1, 0, 0, l)
    ; (0, 0, 0, o)
    ; (1, 0, 0, l)
    ; (-1, 0, 1, l)
    ; (0, 0, 1, l)
    ; (1, 0, 1, l) ]

let tree ?(with_bees = false) ~trunk_height ~wide_leaves_height =
  (Minecraft_template.stack (trunk ~with_bees ~height:trunk_height))
    (Minecraft_template.stack (wide_leaves ~height:wide_leaves_height) top)

let random_tree () =
  let trunk_height = Random.int_incl 2 4 in
  let wide_leaves_height = Random.int_incl 2 3 in
  let with_bees = Random.int 100 < 5 in
  tree ~with_bees ~trunk_height ~wide_leaves_height
