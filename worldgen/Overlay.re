/* module type OVERLAY = {
     type t;
     let title: string;
     let prepare: unit => t;
     let make_region:
       (
         t,
         ~region: Minecraft.Block_tree.t,
         ~rx: int,
         ~rz: int,
         ~gx_offset: int,
         ~gy_offset: int,
         ~gsize: int
       ) =>
       unit;
   }; */

type overlay('state) = {
  name: string,
  prepare: unit => 'state,
  apply_region:
    (
      'state,
      ~region: Minecraft.Block_tree.t,
      ~rx: int,
      ~rz: int,
      ~gx_offset: int,
      ~gy_offset: int,
      ~gsize: int
    ) =>
    unit,
};

type overlay_state =
  | Overlay_state(overlay('state), 'state): overlay_state;

let prepare_overlay = overlay => {
  let state =
    Util.print_progress(
      Printf.sprintf("Preparing %s overlay", overlay.name),
      overlay.prepare,
    );
  Overlay_state(overlay, state);
};

let apply_overlay =
    (
      overlay_state,
      ~region: Minecraft.Block_tree.t,
      ~rx: int,
      ~rz: int,
      ~gx_offset: int,
      ~gy_offset: int,
      ~gsize: int,
    ) => {
  let Overlay_state(overlay, state) = overlay_state;
  Util.print_progress(Printf.sprintf("Applying %s overlay", overlay.name), () =>
    overlay.apply_region(
      state,
      ~region,
      ~rx,
      ~rz,
      ~gx_offset,
      ~gy_offset,
      ~gsize,
    )
  );
};

/*
 type overlay_state =
   | Overlay_state((module OVERLAY with type t = 'a), 'a): overlay_state;

 let prepare = (overlay: (module OVERLAY)): overlay_state => {
   module Ov = (val overlay: OVERLAY);
   let state = Util.print_progress(Ov.title, Ov.prepare);
   Overlay_state((module Ov): (module OVERLAY with type t = Ov.t), state);
 };

 let make_region =
     (
       overlay_state: overlay_state,
       ~region: Minecraft.Block_tree.t,
       ~rx: int,
       ~rz: int,
       ~gx_offset: int,
       ~gy_offset: int,
       ~gsize: int,
     )
     : unit => {
   let Overlay_state(overlay, state) = overlay_state;
   module Ov = (val overlay: OVERLAY with type t = 'state);
   Ov.make_region(state, ~region, ~rx, ~rz, ~gx_offset, ~gy_offset, ~gsize);
 }; */