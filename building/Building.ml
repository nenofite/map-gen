open! Core_kernel

type pos = {origin: int * int * int; rotation: int}

let apply_pos pos ~x ~y ~z =
  let {origin= ox, oy, oz; rotation} = pos in
  (* TODO *)
  ignore rotation ;
  (x + ox, y + oy, z + oz)

module Shared = struct
  type 'a t = pos -> 'a

  let get_elevation ~x ~z pos =
    let canon = todo in
    let x, _, z = apply_pos pos ~x ~y:0 ~z in
    Grid.get x z canon.elevation
end

module Prepare_monad = struct
  type state = {obstacles: (int * int) list}

  let add_obstacle state ~x ~z = {obstacles= (x, z) :: state.obstacles}

  let append_state a b = {obstacles= b.obstacles @ a.obstacles}

  type 'a result = Ok of 'a * state | Collision

  module T = struct
    type 'a t = state -> pos -> 'a result

    let bind (t : 'a t) ~f state pos =
      match t state pos with
      | Ok (a, a_state) ->
          f a a_state pos
      | Collision ->
          Collision

    let return a state _pos = Ok (a, state)

    (* let map (t : 'a t) ~f state pos =
       match t state pos with
       | Ok (a, a_state) ->
           Ok (f a, a_state)
       | Collision ->
           Collision *)

    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)

  let of_shared shared : 'a t = fun state pos -> return (shared pos) state pos

  let prepare (t : 'a t) ~x ~y ~z ~rotation =
    let state = {obstacles= []} in
    let pos = {origin= (x, y, z); rotation} in
    t state pos

  let put_obstacle ~x ~z state pos =
    let x, _, z = apply_pos pos ~x ~y:0 ~z in
    let state = add_obstacle state ~x ~z in
    Ok ((), state)

  let collide_obstacle ~x ~z state pos =
    let canon = todo in
    let x, _, z = apply_pos pos ~x ~y:0 ~z in
    if Grid.get x z canon.obstacles then Collision
    else
      let state = add_obstacle state ~x ~z in
      Ok ((), state)
end

module Apply_monad = struct
  type region_args = Minecraft.Region.t

  module T = struct
    type 'a t = pos -> region_args -> 'a

    let bind (t : 'a t) ~f : 'b t = fun pos args -> f (t pos args) pos args

    let return a : 'a t = fun _pos _args -> a

    let map (t : 'a t) ~f : 'b t = fun pos args -> f (t pos args)

    let map = `Custom map
  end

  include T
  include Monad.Make (T)

  let nop = return ()

  let of_shared shared : 'a t = fun pos args -> return (shared pos) pos args

  let apply (t : 'a t) ~x ~y ~z ~rotation args =
    let pos = {origin= (x, y, z); rotation} in
    t pos args

  let set_block mat ~x ~y ~z : unit t =
   fun pos args ->
    let x, y, z = apply_pos pos ~x ~y ~z in
    Minecraft.Region.set_block mat ~x ~y ~z args

  let get_block ~x ~y ~z : Minecraft.Block.material t =
   fun pos args ->
    let x, y, z = apply_pos pos ~x ~y ~z in
    Minecraft.Region.get_block ~x ~y ~z args

  let height_at ~x ~z : int t =
   fun pos args ->
    let x, _, z = apply_pos pos ~x ~y:0 ~z in
    Minecraft.Region.height_at ~x ~z args
end

module Building_monad = struct
  module T = struct
    type 'a t = 'a Prepare_monad.t * 'a Apply_monad.t

    let bind ((p, a) : 'a t) ~f : 'b t =
      ( Prepare_monad.(p >>= fun n -> fst (f n))
      , Apply_monad.(a >>= fun n -> snd (f n)) )

    let return n : 'a t = (Prepare_monad.return n, Apply_monad.return n)

    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)

  let parallel ~prepare ~apply : 'a t = (prepare, apply)

  let of_shared shared : 'a t =
    parallel
      ~prepare:(Prepare_monad.of_shared shared)
      ~apply:(Apply_monad.of_shared shared)

  let set_block mat ~x ~y ~z : unit t =
    parallel
      ~prepare:(Prepare_monad.collide_obstacle ~x ~z)
      ~apply:(Apply_monad.set_block mat ~x ~y ~z)

  let elevation_at ~x ~z : int t = of_shared (Shared.get_elevation ~x ~z)

  let height_at ~x ~z : int t =
    parallel
      ~prepare:(Prepare_monad.of_shared (Shared.get_elevation ~x ~z))
      ~apply:(Apply_monad.height_at ~x ~z)
end
