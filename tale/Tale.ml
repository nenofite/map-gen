(* Need to bypass Core *)
let gettimeofday = Unix.gettimeofday

open Core

type level = {title: string; indents: int; needs_closer: bool; start_ms: float}

let current : level list ref = ref []

let show_times = ref true

let time_ms () = gettimeofday () *. 1000.

let prev_log_ms = ref (time_ms ())

let current_indents () =
  match !current with {indents; _} :: _ -> indents | [] -> 0

let mark_level_needs_closer () =
  match !current with
  | l :: rest ->
      current := {l with needs_closer= true} :: rest
  | [] ->
      ()

let rec print_indents i =
  if i > 0 then (
    print_string "  " ;
    print_indents (i - 1) )

let print_line_no_time s =
  print_indents (current_indents ()) ;
  print_endline s ;
  Out_channel.flush stdout ;
  mark_level_needs_closer ()

let time_ms_when_needed () = if !show_times then time_ms () else 0.0

let print_line s =
  let s_with_time =
    if !show_times then (
      let now_ms = time_ms () in
      let prev_ms = !prev_log_ms in
      prev_log_ms := now_ms ;
      let elapsed_sec = (now_ms -. prev_ms) /. 1000. in
      Printf.sprintf "+%.2fs| %s" elapsed_sec s )
    else s
  in
  print_line_no_time s_with_time

let pop_level () =
  match !current with
  | l :: rest ->
      current := rest ;
      l
  | [] ->
      failwith "cannot pop level"

let open_level ?(always_close = false) (title : string) =
  print_line title ;
  current :=
    { title
    ; indents= current_indents () + 1
    ; needs_closer= always_close
    ; start_ms= time_ms_when_needed () }
    :: !current

let close_level () =
  let l = pop_level () in
  if l.needs_closer then
    if !show_times then (
      let now_ms = time_ms () in
      let duration_sec = (now_ms -. l.start_ms) /. 1000. in
      prev_log_ms := now_ms ;
      print_line_no_time (Printf.sprintf "/ =%.2fs| %s" duration_sec l.title) )
    else print_line ("/ " ^ l.title)

(* Public *)

let log title = print_line title

let logf fmt = Printf.ksprintf print_line fmt

let block ?always_close title ~f =
  open_level ?always_close title ;
  let result = f () in
  close_level () ; result
  [@@inline]

let blockf ?always_close fmt ~f = Printf.ksprintf (block ?always_close ~f) fmt
  [@@inline]

let log_progress ?every ~label start stop ~f =
  let total = stop - start + 1 in
  let every = match every with Some n -> n | None -> Int.max (total / 20) 1 in
  let prog i total =
    Out_channel.print_string "\r" ;
    print_indents (current_indents ()) ;
    Printf.printf "%s: %d of %d..." label i total ;
    Out_channel.flush stdout
  in
  prog 0 total ;
  for i = start to stop do
    if Int.(i % every = 0) then prog i total ;
    f i
  done ;
  Out_channel.print_string "\r" ;
  logf "%s: %d of %d" label total total ;
  Out_channel.newline stdout
  [@@inline]
