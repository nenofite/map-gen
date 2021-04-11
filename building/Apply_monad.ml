open! Core_kernel

type region_args = Minecraft.Region.t

module T = struct
  type 'a t = Shared.pos -> region_args -> 'a

  let bind (t : 'a t) ~f : 'b t = fun pos region -> f (t pos region) pos region

  let return a : 'a t = fun _pos _args -> a

  let map (t : 'a t) ~f : 'b t = fun pos region -> f (t pos region)

  let map = `Custom map
end

include T
include Monad.Make (T)

let nop = return ()

let of_shared (shared : 'a Shared.t) : 'a t =
 fun pos region -> return (shared pos) pos region

let apply (t : 'a t) ~pos region = t pos region

let set_block mat ~x ~y ~z : unit t =
 fun pos region ->
  let x, y, z = Shared.apply_pos pos ~x ~y ~z in
  Minecraft.Region.set_block mat ~x ~y ~z region

let get_block ~x ~y ~z : Minecraft.Block.material t =
 fun pos region ->
  let x, y, z = Shared.apply_pos pos ~x ~y ~z in
  Minecraft.Region.get_block ~x ~y ~z region

let height_at ~x ~z : int t =
 fun pos region ->
  let x, _, z = Shared.apply_pos pos ~x ~y:0 ~z in
  Minecraft.Region.height_at ~x ~z region
