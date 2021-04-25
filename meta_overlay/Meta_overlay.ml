open! Core_kernel

(**
The meta overlay handles "out of character" elements, ie. places walls around
the edge of the world
*)

type t = No_state [@@deriving bin_io]

let overlay = Overlay.make_overlay "meta" bin_reader_t bin_writer_t

let wall_thickness = 2

let wall_y = 69

let prepare () = No_state

let region_is_edge ~rx ~rz =
  let side = (Overlay.Canon.require ()).side in
  let max_rx, max_rz =
    Minecraft.Region.region_containing ~x:(side - 1) ~z:(side - 1)
  in
  rx = 0 || rx = max_rx || rz = 0 || rz = max_rz

let place_wall_column ~x ~z region =
  let mat = Minecraft.Block.Bedrock in
  if Minecraft.Region.is_within ~x ~y:0 ~z region then
    for y = 0 to wall_y do
      Minecraft.Region.set_block mat ~x ~y ~z region
    done ;
  ()

let apply No_state region =
  let side = (Overlay.Canon.require ()).side in
  let rx, rz = Minecraft.Region.get_region_coords region in
  if region_is_edge ~rx ~rz then
    for n = 0 to side - 1 do
      let th =
        if n / 10 mod 3 = 0 then wall_thickness + 1 else wall_thickness
      in
      (* N *)
      let x = n in
      for z = 0 to th - 1 do
        place_wall_column ~x ~z region
      done ;
      (* E *)
      let z = n in
      for x = side - th to side - 1 do
        place_wall_column ~x ~z region
      done ;
      (* S *)
      let x = n in
      for z = side - th to side - 1 do
        place_wall_column ~x ~z region
      done ;
      (* W *)
      let z = n in
      for x = 0 to th - 1 do
        place_wall_column ~x ~z region
      done
    done ;
  ()

let require, prepare, apply = Overlay.make_lifecycle ~prepare ~apply overlay
