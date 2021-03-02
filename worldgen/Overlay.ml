open Core_kernel

type 'state monad =
  {prepare: unit -> 'state * (Minecraft_converter.region_args -> unit)}

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

let make (name : string) ?(apply_progress_view : 'a -> unit = fun _ -> ())
    (prepare : unit -> 'a)
    (apply_region : 'a -> Minecraft_converter.region_args -> unit)
    (reader : 'a Bin_prot.Type_class.reader)
    (writer : 'a Bin_prot.Type_class.writer) : 'a monad =
  let prepare () =
    let state =
      match read_cache reader (cache_path name) with
      | Some state ->
          Tale.logf "Read %s overlay from cache" name ;
          state
      | None ->
          Mg_util.print_progress
            ("Preparing " ^ name ^ " overlay")
            (fun () ->
              let state = prepare () in
              save_cache name writer state ;
              state)
    in
    apply_progress_view state ;
    let apply_region args =
      Mg_util.print_progress
        ("Applying " ^ name ^ " overlay")
        (fun () -> apply_region state args)
    in
    (state, apply_region)
  in
  {prepare}

let bind m ~f =
  let prepare () =
    let next_seed = Random.bits () in
    let m_state, m_apply_f = m.prepare () in
    Caml.Random.init next_seed ;
    Random.init next_seed ;
    let o_state, o_apply_f = (f m_state).prepare () in
    let apply_f args =
      m_apply_f args ;
      Caml.Random.init next_seed ;
      Random.init next_seed ;
      o_apply_f args
    in
    (o_state, apply_f)
  in
  {prepare}

let return v =
  let prepare () = (v, fun _ -> ()) in
  {prepare}

module Let_syntax = struct
  let bind = bind

  let return = return
end

module Infix = struct
  let ( let* ) x f = bind x ~f

  let return = return
end

let prepare seed monad =
  Caml.Random.init seed ; Random.init seed ; monad.prepare ()

let%expect_test "consistent random state" =
  let overlay_a : unit monad =
    { prepare=
        (fun () ->
          Caml.Random.bits () |> ignore ;
          Caml.Random.bits () |> ignore ;
          ((), fun _ -> ())) }
  in
  let overlay_b : unit monad =
    { prepare=
        (fun () ->
          Caml.Random.bits () |> ignore ;
          ((), fun _ -> ())) }
  in
  let spy : unit monad =
    { prepare=
        (fun () ->
          let test = Caml.Random.int 100 in
          Printf.printf "%d\n" test ;
          ((), fun _ -> ())) }
  in
  prepare 1 (bind overlay_a ~f:(fun _ -> spy)) |> ignore ;
  [%expect "13"] ;
  prepare 1 (bind overlay_b ~f:(fun _ -> spy)) |> ignore ;
  [%expect "13"]
