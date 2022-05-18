open! Core
open Tsdl

type t =
  { pixels_per_tile: int
  ; width: int
  ; height: int
  ; renderer: Sdl.renderer
  ; window: Sdl.window }

let ok_or_sdl r = match r with Ok a -> a | Error (`Msg a) -> failwith a

let make_window ?(pixels_per_tile = 2) ?(width = 400) ?(height = 400) () =
  Sdl.init Sdl.Init.video |> ok_or_sdl ;
  Caml.Sys.(set_signal sigint Signal_default) ;
  let window, renderer =
    Sdl.create_window_and_renderer ~w:(width * pixels_per_tile)
      ~h:(height * pixels_per_tile) Sdl.Window.opengl
    |> ok_or_sdl
  in
  Sdl.set_window_title window "Progress View" ;
  Sdl.pump_events () ;
  Sdl.flush_events Sdl.Event.first_event Sdl.Event.last_event ;
  let w = {pixels_per_tile; width; height; renderer; window} in
  w

let close_window (_w : t) = Sdl.quit ()

let pump_events (_w : t) =
  Sdl.pump_events () ;
  Sdl.flush_events Sdl.Event.first_event Sdl.Event.last_event

let update ~(zoom : int) ~(center_x : int) ~(center_z : int) (title : string)
    (draw_tiles :
         zoom:int
      -> x:int * int
      -> z:int * int
      -> (int -> int -> color:int -> unit)
      -> unit ) (w : t) =
  let {pixels_per_tile; width; height; renderer; window} = w in
  let view =
    View.calculate ~zoom ~center_x ~center_z ~width ~height ~pixels_per_tile
  in
  Sdl.set_render_draw_color renderer 0 0 0 255 |> ok_or_sdl ;
  Sdl.render_fill_rect renderer None |> ok_or_sdl ;
  let tile_rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:pixels_per_tile ~h:pixels_per_tile
  in
  let draw_tile =
    View.apply_to_draw view ~draw:(fun ~wx ~wy ~color ->
        let r, g, b = Mg_util.Color.split_rgb color in
        Sdl.set_render_draw_color renderer r g b 255 |> ok_or_sdl ;
        Sdl.Rect.set_x tile_rect wx ;
        Sdl.Rect.set_y tile_rect wy ;
        Sdl.render_fill_rect renderer (Some tile_rect) |> ok_or_sdl )
  in
  draw_tiles ~zoom ~x:(view.min_x, view.max_x) ~z:(view.min_z, view.max_z)
    draw_tile ;
  Sdl.set_window_title window title ;
  Sdl.render_present renderer ;
  Sdl.pump_events () ;
  Sdl.flush_events Sdl.Event.first_event Sdl.Event.last_event

let manual_test () =
  let w = make_window () in
  let draw ~zoom:_ ~x:(min_x, max_x) ~z:(min_z, max_z) set_coord =
    for z = min_z to max_z do
      for x = min_x to max_x do
        if x mod 3 = 0 then set_coord x z ~color:0x0000FF ;
        if x = z then set_coord x z ~color:0xFF0000
      done
    done
  in
  update ~zoom:1 ~center_x:200 ~center_z:200 "howdy" draw w ;
  ()
