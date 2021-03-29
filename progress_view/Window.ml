open! Core_kernel
open Tsdl

type update_payload =
  { zoom: int
  ; center_x: int
  ; center_z: int
  ; title: string
  ; draw_tiles:
         zoom:int
      -> x:int * int
      -> z:int * int
      -> (int -> int -> int * int * int -> unit)
      -> unit }

type t =
  { pixels_per_tile: int
  ; width: int
  ; height: int
  ; ch: update_payload Event.channel }

let ok_or_sdl r = match r with Ok a -> a | Error (`Msg a) -> failwith a

let run_thread w =
  let {pixels_per_tile; width; height; ch} = w in
  Sdl.init Sdl.Init.video |> ok_or_sdl ;
  let window, renderer =
    Sdl.create_window_and_renderer ~w:width ~h:height Sdl.Window.opengl
    |> ok_or_sdl
  in
  Sdl.set_window_title window "Progress View" ;
  let ev = Sdl.Event.create () in
  while true do
    ( match Event.(poll (receive ch)) with
    | Some {zoom; center_x; center_z; title; draw_tiles} ->
        let view =
          View.calculate ~zoom ~center_x ~center_z ~width ~height
            ~pixels_per_tile
        in
        Sdl.set_render_draw_color renderer 0 0 0 255 |> ok_or_sdl ;
        Sdl.render_fill_rect renderer None |> ok_or_sdl ;
        let tile_rect =
          Sdl.Rect.create ~x:0 ~y:0 ~w:pixels_per_tile ~h:pixels_per_tile
        in
        let draw_tile =
          View.apply_to_draw view ~draw:(fun ~wx ~wy ~color:(r, g, b) ->
              Sdl.set_render_draw_color renderer r g b 255 |> ok_or_sdl ;
              Sdl.Rect.set_x tile_rect wx ;
              Sdl.Rect.set_y tile_rect wy ;
              Sdl.render_fill_rect renderer (Some tile_rect) |> ok_or_sdl)
        in
        draw_tiles ~zoom ~x:(view.min_x, view.max_x) ~z:(view.min_z, view.max_z)
          draw_tile ;
        Sdl.set_window_title window title ;
        Sdl.render_present renderer
    | None ->
        () ) ;
    ( if Sdl.poll_event (Some ev) then
      match Sdl.Event.get ev Sdl.Event.typ with
      | t when t = Sdl.Event.quit ->
          Sdl.quit () ; exit 0
      | _ ->
          () ) ;
    Thread.yield ()
  done

let make_window ?(pixels_per_tile = 2) ?(width = 400) ?(height = 400) () =
  let ch = Event.new_channel () in
  let w = {pixels_per_tile; width; height; ch} in
  Thread.create run_thread w |> ignore ;
  w

let close_window (_w : t) = Sdl.quit ()

let update ~(zoom : int) ~(center_x : int) ~(center_z : int) (title : string)
    (draw_tiles :
         zoom:int
      -> x:int * int
      -> z:int * int
      -> (int -> int -> int * int * int -> unit)
      -> unit) (w : t) =
  let payload = {zoom; center_x; center_z; title; draw_tiles} in
  Event.(sync (send w.ch payload))

(* let view =
     View.calculate ~zoom ~center_x ~center_z ~width:w.width ~height:w.height
       ~pixels_per_tile:w.pixels_per_tile
   in
   let {window; renderer; _} = w in
   Sdl.set_render_draw_color renderer 0 0 0 255 |> ok_or_sdl ;
   Sdl.render_fill_rect renderer None |> ok_or_sdl ;
   let tile_rect =
     Sdl.Rect.create ~x:0 ~y:0 ~w:w.pixels_per_tile ~h:w.pixels_per_tile
   in
   let draw_tile =
     View.apply_to_draw view ~draw:(fun ~wx ~wy ~color:(r, g, b) ->
         Sdl.set_render_draw_color renderer r g b 255 |> ok_or_sdl ;
         Sdl.Rect.set_x tile_rect wx ;
         Sdl.Rect.set_y tile_rect wy ;
         Sdl.render_fill_rect renderer (Some tile_rect) |> ok_or_sdl)
   in
   draw_tiles ~zoom ~x:(view.min_x, view.max_x) ~z:(view.min_z, view.max_z)
     draw_tile ;
   Sdl.set_window_title window title ;
   Sdl.render_present renderer ;
   flush_events w *)

let manual_test () =
  let w = make_window () in
  let draw ~zoom:_ ~x:(min_x, max_x) ~z:(min_z, max_z) set_coord =
    for z = min_z to max_z do
      for x = min_x to max_x do
        if x mod 3 = 0 then set_coord x z (0, 0, 255) ;
        if x = z then set_coord x z (255, 0, 0)
      done
    done
  in
  update ~zoom:1 ~center_x:200 ~center_z:200 "howdy" draw w ;
  ()
