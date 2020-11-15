(** Mutable grids backed by arrays, which can be pre-allocated to allow for
    fast subdivision *)

open Core_kernel

module T = struct
  type 'a t = {
    mutable side: int;
    data: 'a array;
  }
end
include T

let create ~side ?(alloc_side = side) value = (
  assert (alloc_side >= side);
  { side; data = Array.create ~len:(alloc_side * alloc_side) value }
)

let i_of_xz ~x ~z side = z * side + x

module Grid_ops = struct
  include T
  open Griddable.Helpers

  type 'a elt = 'a

  let side t = t.side

  let get ~x ~z t = (
    assert_within_side ~x ~z t.side;
    t.data.(i_of_xz ~x ~z t.side)
  )

  let set ~x ~z v t = (
    assert_within_side ~x ~z t.side;
    t.data.(i_of_xz ~x ~z t.side) <- v;
    t
  )
end

module Intf = Griddable.Make(Grid_ops)
include Intf

let set ~x ~z v t = ignore (set ~x ~z v t)

let expand_for_subdivide ~side t = (
  let old_side = t.side in
  assert (side * side <= Array.length t.data);
  t.side <- side;
  for z = old_side - 1 downto 0 do
    for x = old_side - 1 downto 0 do
      let old_i = i_of_xz ~x ~z old_side in
      let new_i = i_of_xz ~x:(x * 2) ~z:(z * 2) side in
      t.data.(new_i) <- t.data.(old_i)
      (* TODO set old index to a given "empty" value? *)
    done
  done
)