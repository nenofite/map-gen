open Core_kernel
include Graphics

module Coord = struct
  module T = struct
    type t = int * int [@@deriving eq, ord, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let draw_splat size =
  let width = size_x () in
  let height = size_y () in
  let marked = Coord.Hash_set.create () in
  let periphery =
    Pairing_heap.create ~cmp:(fun (a, _) (b, _) -> Int.compare a b) ()
  in
  let start = (width / 2, height / 2) in
  Pairing_heap.add periphery (0, start) ;
  set_color blue ;
  fill_rect 0 0 (width - 1) (height - 1) ;
  set_color green ;
  let last_syncd = ref 0 in
  while Hash_set.length marked < size do
    if Hash_set.length marked - !last_syncd > 10 then (
      last_syncd := Hash_set.length marked ;
      synchronize () ) ;
    let _, next = Pairing_heap.pop_exn periphery in
    let x, y = next in
    if not (Hash_set.mem marked next) then (
      Hash_set.add marked next ;
      plot x y ;
      for x = x - 1 to x + 1 do
        for y = y - 1 to y + 1 do
          let r = Random.bits () in
          Pairing_heap.add periphery (r, (x, y))
        done
      done )
  done ;
  synchronize () ;
  ()

let main () =
  open_graph "" ;
  resize_window 400 400 ;
  set_window_title "processing!" ;
  auto_synchronize false ;
  draw_splat 10_000 ;
  synchronize () ;
  (* while true do
       sleepf 1. ; ()
     done *)
  ()
