open Core_kernel

type level = {
  title: string;
  indents: int;
  needs_closer: bool;
}

let current: level list ref = ref []

let current_indents () =
  match !current with
  | {indents; _} :: _ -> indents
  | [] -> 0

let mark_level_needs_closer () =
  match !current with
  | l :: rest -> current := {l with needs_closer = true} :: rest
  | [] -> ()

let rec print_indents i =
  if i > 0 then (
    print_string "  ";
    print_indents (i - 1)
  )

let print_line s =
  print_indents (current_indents ());
  print_endline s;
  Out_channel.flush stdout;
  mark_level_needs_closer ()

let pop_level () =
  match !current with
  | l :: rest ->
    current := rest;
    l
  | [] -> failwith "cannot pop level"

let open_level ?(always_close = false) (title: string) =
  print_line title;
  current := {
    title;
    indents = current_indents () + 1;
    needs_closer = always_close;
  } :: !current

let close_level () =
  let l = pop_level () in
  if l.needs_closer then
    print_line ("/ " ^ l.title)

(* Public *)

let log title =
  print_line title

let logf fmt =    
  Printf.ksprintf print_line fmt

let block ?always_close title ~f =
  open_level ?always_close title;
  let result = f () in
  close_level ();
  result

let blockf ?always_close fmt ~f =    
  Printf.ksprintf (block ?always_close ~f) fmt