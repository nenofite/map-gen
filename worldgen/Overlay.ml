open Core_kernel

type global_state = {mutable seed: int}

let global_state = {seed= 0}

type 't overlay_state =
  { name: string
  ; reader: 't Bin_prot.Type_class.reader
  ; writer: 't Bin_prot.Type_class.writer
  ; mutable canon_before: Canonical_overlay.t option
  ; mutable layer_before: Progress_view.layer option
  ; mutable prepared_state: 't option }

let init seed =
  global_state.seed <- seed ;
  ()

let make_overlay name reader writer =
  { name
  ; reader
  ; writer
  ; canon_before= None
  ; layer_before= None
  ; prepared_state= None }

let require_overlay overlay =
  match overlay.prepared_state with
  | Some s ->
      s
  | None ->
      failwithf "required %s overlay, but it isn't prepared" overlay.name ()

let seed overlay = global_state.seed lxor Hash.Builtin.hash_string overlay.name

let cache_path name = Config.Paths.overlay (name ^ ".overlay")

let read_cache reader path =
  match In_channel.read_all path with
  | str -> (
    match
      let open Bigstring in
      read_bin_prot (of_string str) reader
    with
    | Ok (state, _) ->
        Some state
    | Error _ ->
        None )
  | exception Sys_error _ ->
      None

let save_cache name writer state =
  let open Out_channel in
  Tale.logf "Saving to cache..." ;
  flush stdout ;
  with_file (cache_path name) ~f:(fun f ->
      let buf = Bin_prot.Utils.bin_dump ~header:true writer state in
      output_bytes f (Bigstring.to_bytes buf)) ;
  Tale.logf "Done"

let before_prepare overlay =
  let seed = seed overlay in
  Caml.Random.init seed ;
  Random.init seed ;
  ( match overlay.canon_before with
  | None ->
      overlay.canon_before <- Some (Canonical_overlay.require ())
  | Some c ->
      Canonical_overlay.restore c ) ;
  ( match overlay.layer_before with
  | None ->
      overlay.layer_before <- Progress_view.last_layer ()
  | Some c ->
      Progress_view.remove_after_layer c ) ;
  ()

let finish_prepare ~state overlay =
  overlay.prepared_state <- Some state ;
  save_cache overlay.name overlay.writer state ;
  ()

let wrap_prepare ?(force = false) overlay f =
  before_prepare overlay ;
  let state =
    match
      if force then None
      else read_cache overlay.reader (cache_path overlay.name)
    with
    | Some s ->
        Tale.logf "Read %s overlay from cache" overlay.name ;
        overlay.prepared_state <- Some s ;
        s
    | None ->
        Tale.blockf "Preparing %s overlay" overlay.name ~f:(fun () ->
            let state = f () in
            finish_prepare ~state overlay ;
            state)
  in
  state

let make_lifecycle ?(before_prepare : unit -> unit = fun () -> ())
    ~(prepare : unit -> 'a) ?(after_prepare : 'a -> unit = fun _ -> ())
    ~(apply : 'a -> Minecraft_converter.region_args -> unit)
    (overlay : 'a overlay_state) =
  let require () = require_overlay overlay in
  let prepare ?force () =
    before_prepare () ;
    let state = wrap_prepare ?force overlay prepare in
    after_prepare state
  in
  let apply args =
    let state = require () in
    Tale.blockf "Applying %s overlay" overlay.name ~f:(fun () ->
        apply state args) ;
    ()
  in
  (require, prepare, apply)

let canon_helper (_, canond) =
  Canonical_overlay.push_delta canond ;
  ()

let make_no_canon (name : string)
    ?(apply_progress_view : 'a -> unit = fun _ -> ()) (prepare : unit -> 'a)
    (apply_region : 'a -> Minecraft_converter.region_args -> unit)
    (reader : 'a Bin_prot.Type_class.reader)
    (writer : 'a Bin_prot.Type_class.writer) =
  Tale.logf "%s overlay using deprecated [make_no_canon] function" name ;
  let overlay = make_overlay name reader writer in
  make_lifecycle ~prepare ~after_prepare:apply_progress_view ~apply:apply_region
    overlay

let make name ?(apply_progress_view = fun _ -> ()) prepare apply_region reader
    writer =
  Tale.logf "%s overlay using deprecated [make] function" name ;
  let overlay = make_overlay name reader writer in
  let after_prepare ((_, canond) as state) =
    Canonical_overlay.push_delta canond ;
    apply_progress_view state ;
    ()
  in
  make_lifecycle ~prepare ~after_prepare ~apply:apply_region overlay

let%expect_test "consistent random state" =
  let overlay_a =
    make_overlay "test_a" Bin_prot.Type_class.bin_reader_unit
      Bin_prot.Type_class.bin_writer_unit
  in
  let overlay_b =
    make_overlay "test_b" Bin_prot.Type_class.bin_reader_unit
      Bin_prot.Type_class.bin_writer_unit
  in
  init 123 ;
  before_prepare overlay_a ;
  let a_rand_1 = Caml.Random.bits () in
  before_prepare overlay_b ;
  let b_rand_1 = Caml.Random.bits () in
  before_prepare overlay_a ;
  let a_rand_2 = Caml.Random.bits () in
  before_prepare overlay_b ;
  let b_rand_2 = Caml.Random.bits () in
  Printf.printf "overlay a: %d == %d" a_rand_1 a_rand_2 ;
  [%expect "TODO"] ;
  Printf.printf "overlay b: %d == %d" b_rand_1 b_rand_2 ;
  [%expect "TODO"]
