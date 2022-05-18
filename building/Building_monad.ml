open! Core

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

let only_prepare prepare : unit t = (prepare, Apply_monad.nop)

let only_apply apply : unit t = (Prepare_monad.nop, apply)

let nop : unit t = parallel ~prepare:Prepare_monad.nop ~apply:Apply_monad.nop

let run_prepare (p, _) ~pos = Prepare_monad.run p ~pos

let run_apply (_, a) ~region ~pos = Apply_monad.run a ~pos ~region

let set_block mat ~x ~y ~z : unit t =
  parallel
    ~prepare:(Prepare_monad.collide_obstacle ~x ~z)
    ~apply:(Apply_monad.set_block mat ~x ~y ~z)

(* TODO check within region whenever coords are involved *)
let elevation_at ~x ~z : int t =
  let get_elev ~x ~y:_ ~z =
    let canon = Overlay.Canon.require () in
    Grid.get ~x ~z canon.elevation
  in
  parallel
    ~prepare:(Prepare_monad.with_pos_applied ~x ~y:0 ~z get_elev)
    ~apply:(Apply_monad.with_pos_applied ~x ~y:0 ~z get_elev)

let height_at ~x ~z : int t =
  let get_elev ~x ~y:_ ~z =
    let canon = Overlay.Canon.require () in
    Grid.get ~x ~z canon.elevation
  in
  parallel
    ~prepare:(Prepare_monad.with_pos_applied ~x ~y:0 ~z get_elev)
    ~apply:(Apply_monad.height_at ~x ~z)

let place_template (t : _ Minecraft_template.t) ~x ~y ~z : unit t =
  parallel
    ~prepare:(Prepare_monad.place_template t ~x ~y ~z)
    ~apply:(Apply_monad.place_template t ~x ~y ~z)
