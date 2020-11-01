open Core_kernel

type segment = {
  start: Geometry.Vec3i.t;
  finish: Geometry.Vec3i.t;
}
[@@deriving bin_io]

type cave = {
  segments: segment list;
}
[@@deriving bin_io]

let down_moves = [ 0, -1, -1;
                   1, -1, 0;
                   0, -1, 1;
                   -1, -1, 0 ]

let flat_moves = [ 0, 0, -1;
                   1, 0, 0;
                   0, 0, 1;
                   -1, 0, 0 ]

let random_segments ?(max_moves = 10) (start: Geometry.vec3i) =
  let module V = Geometry.Vec3i in
  let rec add_segments position prev_move segments max_moves =
    if max_moves <= 0 then
      segments
    else
      let next_move = List.random_element_exn (if Random.int 100 < 10 then down_moves else flat_moves) in
      if V.(next_move = prev_move *. -1) then
        (* We backtracked, so try again *)
        add_segments position prev_move segments max_moves
      else
        let segment = { start = position; finish = V.(position + next_move) } in
        add_segments segment.finish next_move (segment :: segments) (max_moves - 1)
  in
  let first_move = List.random_element_exn down_moves in
  let first_segment = { start; finish = V.(start + first_move) } in
  add_segments first_segment.finish first_move [first_segment] max_moves |>
  List.rev
;;