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
end

module type S = sig
  include Arg

  val is_within : x:int -> z:int -> 'a t -> bool
  val set_opt : x:int -> z:int -> 'a elt -> 'a t -> 'a t
  val get_opt : x:int -> z:int -> 'a t -> 'a elt option
end

module Make(T: Arg) : S
  with type 'a t := 'a T.t and type 'a elt := 'a T.elt =
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
end