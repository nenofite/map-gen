open Core_kernel

module Helpers = struct
  let is_within_side ~x ~z s = (
    0 <= x && x < s && 0 <= z && z < s
  )

  let assert_within_side ~x ~z s = (
    if not (is_within_side ~x ~z s) then
      invalid_argf "outside grid bounds: (%d, %d) but side=%d" x z s ()
  )
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