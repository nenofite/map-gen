open Core
open Tsdl

module Coord = struct
  module T = struct
    type t = int * int [@@deriving eq, ord, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

(* let draw_splat size =
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
   () *)

let ok_or_sdl r = match r with Ok a -> a | Error (`Msg a) -> failwith a

let draw_points pts ~r =
  Sdl.set_render_draw_color r 255 0 0 255 |> ok_or_sdl ;
  List.iter pts ~f:(fun (x, y) -> Sdl.render_draw_point r x y |> ok_or_sdl)

let draw_point =
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:10 ~h:10 in
  fun r ~x ~y ->
    Sdl.Rect.set_x rect x ;
    Sdl.Rect.set_y rect y ;
    Sdl.render_fill_rect r (Some rect) |> ok_or_sdl

let main () =
  let open Result.Let_syntax in
  let w, r =
    (let%bind () = Sdl.init Sdl.Init.video in
     let%bind w, r =
       Sdl.create_window_and_renderer ~w:400 ~h:400 Sdl.Window.opengl
     in
     Sdl.set_window_title w "Howdy!" ;
     return (w, r) )
    |> Result.map_error ~f:(fun (`Msg s) -> s)
    |> Result.ok_or_failwith
  in
  draw_points [(10, 20); (40, 50); (70, 80)] ~r ;
  draw_point r ~x:30 ~y:70 ;
  draw_point r ~x:70 ~y:300 ;
  Sdl.render_present r ;
  let continue = ref true in
  let ev = Sdl.Event.create () in
  while !continue do
    ( if Sdl.poll_event (Some ev) then
      Sdl.Event.(
        match get ev typ with t when t = quit -> continue := false | _ -> ()) ) ;
    Sdl.delay 10l
  done ;
  ignore w ;
  Sdl.quit () ;
  ()
