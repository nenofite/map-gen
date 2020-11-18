open Core_kernel

module Helpers = struct
  let is_within_side ~x ~z s = (
    0 <= x && x < s && 0 <= z && z < s
  )

  let assert_within_side ~x ~z s = (
    if not (is_within_side ~x ~z s) then
      invalid_argf "outside grid bounds: (%d, %d) but side=%d" x z s ()
  )

  (** four_directions is a list of the four cardinal direction offsets: N, E, S, W *)
  let four_directions = [(0, (-1)); (1, 0); (0, 1); ((-1), 0)]

  (** eight_directions is a list of the eight direction offsets: NW, N, NE, ..., SW, W *)
  let eight_directions = [
    ((-1), (-1));
    (0, (-1));
    (1, (-1));
    (1, 0);
    (1, 1);
    (0, 1);
    ((-1), 1);
    ((-1), 0);
  ]
end
include Helpers

module type Arg = sig
  type 'a t
  type 'a elt

  val side : 'a t -> int
  val get : x:int -> z:int -> 'a t -> 'a elt
  val set : x:int -> z:int -> 'a elt -> 'a t -> 'a t
  val update : x:int -> z:int -> f:('a elt -> 'a elt) -> 'a t -> 'a t
end

module type S = sig
  include Arg

  val is_within : x:int -> z:int -> 'a t -> bool
  val set_opt : x:int -> z:int -> 'a elt -> 'a t -> 'a t
  val get_opt : x:int -> z:int -> 'a t -> 'a elt option
  val update_opt : x:int -> z:int -> f:('a elt -> 'a elt) -> 'a t -> 'a t
  val set_wrap : x:int -> z:int -> 'a elt -> 'a t -> 'a t
  val get_wrap : x:int -> z:int -> 'a t -> 'a elt

  val fold : init:'b -> f:(x:int -> z:int -> 'b -> 'a elt -> 'b) -> 'a t -> 'b
  val map : f:(x:int -> z:int -> 'a elt -> 'a elt) -> 'a t -> 'a t

  val neighbors : x:int -> z:int -> 'a t -> 'a elt list
  val neighbors_coords : x:int -> z:int -> 'a t -> ('a elt * int * int) list
end

module type Arg0 = sig
  type t
  type elt
  include Arg with type _ t := t and type _ elt := elt
end

module type S0 = sig
  include Arg0
  include S with type _ t := t and type _ elt := elt
end

module Make(T: Arg) : S
  with type 'a t = 'a T.t and type 'a elt = 'a T.elt =
struct
  include T
  let is_within ~x ~z t = is_within_side ~x ~z (side t)

  let set_opt ~x ~z v t = (
    if is_within ~x ~z t
    then set ~x ~z v t
    else t
  )

  let get_opt ~x ~z t = (
    if is_within ~x ~z t
    then Some (get ~x ~z t)
    else None
  )

  let update_opt ~x ~z ~f t = (
    if is_within ~x ~z t
    then update ~x ~z ~f t
    else t
  )

  let get_wrap ~x ~z t = (
    let s = side t in
    get ~x:(x % s) ~z:(z % s) t
  )

  let set_wrap ~x ~z v t = (
    let s = side t in
    set ~x:(x % s) ~z:(z % s) v t
  )

  let fold ~init ~f t = (
    let s = side t in
    Mg_util.Range.fold 0 (s - 1) init (fun acc z ->
        Mg_util.Range.fold 0 (s - 1) acc (fun acc x ->
            f ~x ~z acc (get ~x ~z t)
          )
      )
  )

  let map ~f t = (
    let s = side t in
    Mg_util.Range.fold 0 (s - 1) t (fun acc z ->
        Mg_util.Range.fold 0 (s - 1) acc (fun acc x ->
            set ~x ~z (f ~x ~z (get ~x ~z acc)) acc
          )
      )
  )

  let neighbors ~x ~z t =
    List.map eight_directions ~f:(fun (dx, dz) -> get_wrap t ~x:(x + dx) ~z:(z + dz))

  let neighbors_coords ~x ~z t =
    List.map eight_directions ~f:(fun (dx, dz) ->
        let nx = x + dx in
        let nz = z + dz in
        (get_wrap t ~x:nx ~z:nz, nx, nz)
      )
end

module Make0(T: Arg0): S0 with type t = T.t and type elt = T.elt =
struct
  include T
  include (Make(struct
             include T
             type _ t = T.t
             type _ elt = T.elt
           end) :
             S with type 'a t := T.t and type 'a elt := T.elt)
end