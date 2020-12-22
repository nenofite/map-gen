(** Mutable grids backed by arrays, which can be pre-allocated to allow for
    fast subdivision *)

open Core_kernel

module T = struct
  type 'a t = {mutable side: int; data: 'a array}
end

include T

let i_of_xz ~x ~z side = (z * side) + x

let x_of_i ~i side = i % side

let z_of_i ~i side = i / side

let create ~side ?(alloc_side = side) value =
  assert (alloc_side >= side) ;
  {side; data= Array.create ~len:(alloc_side * alloc_side) value}

let init ~side ?(alloc_side = side) ~f outside_value =
  assert (alloc_side >= side) ;
  let after_grid_i = side * side in
  let init_f i =
    if i < after_grid_i then f ~x:(x_of_i ~i side) ~z:(z_of_i ~i side)
    else outside_value
  in
  {side; data= Array.init (alloc_side * alloc_side) ~f:init_f}

module Grid_ops = struct
  include T
  open Griddable.Helpers

  type 'a elt = 'a

  let side t = t.side

  let get ~x ~z t =
    assert_within_side ~x ~z t.side ;
    t.data.(i_of_xz ~x ~z t.side)

  let set ~x ~z v t =
    assert_within_side ~x ~z t.side ;
    t.data.(i_of_xz ~x ~z t.side) <- v ;
    t

  let update ~x ~z ~f t =
    assert_within_side ~x ~z t.side ;
    let i = i_of_xz ~x ~z t.side in
    t.data.(i) <- f t.data.(i) ;
    t
end

module Intf = Griddable.Make (Grid_ops)
include Intf

module Intf0 (E : sig
  type elt
end) =
Griddable.Make0 (struct
  include Grid_ops

  type elt = E.elt

  type t = E.elt T.t
end)

let intf0 (type elt) (_t : elt t) =
  let module A = struct
    type nonrec elt = elt
  end in
  let module I = Intf0 (A) in
  (module I : Griddable.S0 with type t = elt t and type elt = elt)

let set ~x ~z v t = ignore (set ~x ~z v t)

let expand_for_subdivide t =
  let old_side = t.side in
  let new_side = t.side * 2 in
  assert (new_side * new_side <= Array.length t.data) ;
  t.side <- new_side ;
  for z = old_side - 1 downto 0 do
    for x = old_side - 1 downto 0 do
      let old_i = i_of_xz ~x ~z old_side in
      let new_i = i_of_xz ~x:(x * 2) ~z:(z * 2) new_side in
      t.data.(new_i) <- t.data.(old_i)
      (* TODO set old index to a given "empty" value? *)
    done
  done
