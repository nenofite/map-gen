open! Core_kernel

type pos = {origin: int * int * int; rotation: int}

let apply_pos pos ~x ~y ~z =
  let {origin= ox, oy, oz; rotation} = pos in
  (* TODO *)
  ignore rotation ;
  (x + ox, y + oy, z + oz)

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

  let prepare (t : 'a t) ~x ~y ~z ~rotation =
    let state = {obstacles= []} in
    let pos = {origin= (x, y, z); rotation} in
    t state pos
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

  let set_block mat ~x ~y ~z : unit t =
   fun pos args ->
    let x, y, z = apply_pos pos ~x ~y ~z in
    Minecraft.Region.set_block mat ~x ~y ~z args

  let get_block ~x ~y ~z : Minecraft.Block.material t =
   fun pos args ->
    let x, y, z = apply_pos pos ~x ~y ~z in
    Minecraft.Region.get_block ~x ~y ~z args

  let apply (t : 'a t) ~x ~y ~z ~rotation args =
    let pos = {origin= (x, y, z); rotation} in
    t pos args
end

module Building_monad = struct
  type ('p, 'a) exec = Prepare of 'p | Apply of 'a

  type input_exec =
    (Prepare_monad.state * pos, pos * Apply_monad.region_args) exec

  module T = struct
    type 'a t = input_exec -> ('a Prepare_monad.t, 'a Apply_monad.t) exec

    let bind (t : 'a t) ~f : 'a t =
     fun exec ->
      match t exec with
      | Prepare m ->
          Prepare
            (Prepare_monad.map m ~f:(fun a -> f a exec) |> Prepare_monad.join)
      | Apply (pos, args) ->
          todo
  end

  let set_block mat ~x ~y ~z : unit t =
    Prepare_monad.collide_obstacle ~x ~z + Apply_monad.set_block mat ~x ~y ~z
end

(* module Building_monad = struct
  type ('p, 'a) exec = Prepare of 'p | Apply of 'a

  type input_exec =
    (Prepare_monad.state * pos, pos * Apply_monad.region_args) exec

  module T = struct
    type 'a t = input_exec -> ('a Prepare_monad.t, 'a Apply_monad.t) exec

    let bind (t : 'a t) ~f : 'a t =
     fun exec ->
      match t exec with
      | Prepare m ->
          Prepare
            (Prepare_monad.map m ~f:(fun a -> f a exec) |> Prepare_monad.join)
      | Apply (pos, args) ->
          todo
  end

  let set_block mat ~x ~y ~z : unit t =
    Prepare_monad.collide_obstacle ~x ~z + Apply_monad.set_block mat ~x ~y ~z
end *)
