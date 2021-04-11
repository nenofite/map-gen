open! Core_kernel

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

let of_shared (shared : 'a Shared.t) : 'a t =
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
