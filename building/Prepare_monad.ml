open! Core_kernel

type state = {obstacles: (int * int) list}

let add_obstacle state ~x ~z = {obstacles= (x, z) :: state.obstacles}

let append_state a b = {obstacles= b.obstacles @ a.obstacles}

type 'a result = Ok of 'a * state | Collision

module T = struct
  type 'a t = state -> Shared.pos -> 'a result

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

let of_shared (shared : 'a Shared.t) : 'a t =
 fun state pos -> return (shared pos) state pos

let prepare (t : 'a t) ~pos =
  let state = {obstacles= []} in
  t state pos

let put_obstacle ~x ~z state pos =
  let x, _, z = Shared.apply_pos pos ~x ~y:0 ~z in
  let state = add_obstacle state ~x ~z in
  Ok ((), state)

let collide_obstacle ~x ~z state pos =
  let canon = Overlay.Canon.require () in
  let x, _, z = Shared.apply_pos pos ~x ~y:0 ~z in
  if Overlay.Canon.can_build_on (Grid.get x z canon.obstacles) then
    let state = add_obstacle state ~x ~z in
    Ok ((), state)
  else Collision
