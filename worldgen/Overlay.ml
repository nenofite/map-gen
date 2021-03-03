open Core_kernel

type global_state = {mutable seed: int}

let global_state = {seed= 0}

type 't overlay_state =
  { name: string
  ; reader: 't Bin_prot.Type_class.reader
  ; writer: 't Bin_prot.Type_class.writer
  ; apply_progress_view: 't -> unit
  ; mutable canon_before: Canonical_overlay.t option
  ; mutable prepared_state: 't option
        (* ; mutable seed: int option *)
        (* mutable progress_view_layer : Progress_view.layer *) }

let init seed =
  global_state.seed <- seed ;
  ()

let make_overlay name ?(apply_progress_view = fun _ -> ()) reader writer =
  { name
  ; reader
  ; writer
  ; apply_progress_view
  ; canon_before= None
  ; prepared_state= None }

let require_overlay overlay =
  match overlay.prepared_state with
  | Some s ->
      s
  | None ->
      failwithf "required %s overlay, but it isn't prepared" overlay.name

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
  ()

let finish_prepare ~state overlay =
  overlay.prepared_state <- Some state ;
  save_cache overlay.name overlay.writer state ;
  ()

let wrap_prepare overlay f () =
  let state =
    match read_cache overlay.reader (cache_path overlay.name) with
    | Some s ->
        Tale.logf "Read %s overlay from cache" overlay.name ;
        overlay.prepared_state <- Some s ;
        s
    | None ->
        Tale.blockf "Preparing %s overlay" overlay.name ~f:(fun () ->
            before_prepare overlay ;
            let state = f () in
            finish_prepare ~state overlay ;
            state)
  in
  overlay.apply_progress_view state ;
  state

let make (name : string) ?(apply_progress_view : ('a -> unit) option)
    (prepare : unit -> 'a)
    (apply_region : 'a -> Minecraft_converter.region_args -> unit)
    (reader : 'a Bin_prot.Type_class.reader)
    (writer : 'a Bin_prot.Type_class.writer) =
  Tale.logf "%s overlay using deprecated [make] function" name ;
  let overlay = make_overlay name ?apply_progress_view reader writer in
  let require () = require_overlay overlay in
  let prepare = wrap_prepare overlay prepare in
  let apply args =
    let state = require () in
    Mg_util.print_progress
      ("Applying " ^ name ^ " overlay")
      (fun () -> apply_region state args)
  in
  (require, prepare, apply)

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
