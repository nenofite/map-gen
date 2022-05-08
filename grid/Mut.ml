(** Mutable grids backed by arrays, which can be pre-allocated to allow for
    fast subdivision *)

open Core

module T = struct
  type 'a t = {mutable side: int; data: 'a array} [@@deriving bin_io]
end

include T

let i_of_xz ~x ~z side = (z * side) + x

let x_of_i ~i side = i % side

let z_of_i ~i side = i / side

let create ~side ?(alloc_side = side) value =
  assert (alloc_side >= side) ;
  {side; data= Array.create ~len:(alloc_side * alloc_side) value}

let make = create

let init_exact ~side ~f =
  let init_f i = f ~x:(x_of_i ~i side) ~z:(z_of_i ~i side) in
  {side; data= Array.init (side * side) ~f:init_f}

let init ~side ?(alloc_side = side) ~f outside_value =
  assert (alloc_side >= side) ;
  let after_grid_i = side * side in
  let init_f i =
    if i < after_grid_i then f ~x:(x_of_i ~i side) ~z:(z_of_i ~i side)
    else outside_value
  in
  {side; data= Array.init (alloc_side * alloc_side) ~f:init_f}

let get ~x ~z t =
  Griddable.Helpers.assert_within_side ~x ~z t.side ;
  t.data.(i_of_xz ~x ~z t.side)

let copy t = {side= t.side; data= Array.copy t.data}

module Grid_ops = struct
  include T
  open Griddable.Helpers

  type 'a elt = 'a

  let side t = t.side

  let get = get

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
include Griddable.Helpers

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

let copy_set ~x ~z v t =
  let t = copy t in
  set ~x ~z v t ; t

let raw_set_side t ~side =
  assert (side * side <= Array.length t.data) ;
  t.side <- side

let update ~x ~z ~f t = ignore (update ~x ~z ~f t)

let expand_for_subdivide t =
  let old_side = t.side in
  let new_side = t.side * 2 in
  raw_set_side t ~side:new_side ;
  for z = old_side - 1 downto 0 do
    for x = old_side - 1 downto 0 do
      let old_i = i_of_xz ~x ~z old_side in
      let new_i = i_of_xz ~x:(x * 2) ~z:(z * 2) new_side in
      t.data.(new_i) <- t.data.(old_i)
      (* TODO set old index to a given "empty" value? *)
    done
  done

let map_in_place ~f t = iter t ~f:(fun ~x ~z here -> set ~x ~z (f ~x ~z here) t)

let map ~f t =
  init_exact ~side:t.side ~f:(fun ~x ~z ->
      let from = get ~x ~z t in
      f ~x ~z from )

let zip_map ~f a b =
  if side a <> side b then
    failwithf "grid sides must match to zip: %d vs %d" a.side b.side () ;
  init_exact ~side:(side a) ~f:(fun ~x ~z ->
      let la = get ~x ~z a in
      let lb = get ~x ~z b in
      f ~x ~z la lb )