open! Core_kernel

type 'a t = 'a Mut.t [@@deriving bin_io]

(* This makes it easier to include t in submodules *)
type 'a grid = 'a t

let side t = Mut.side t

(** very efficient check if an int is a power of 2. I'm not smart enough to
    think of this; taken from
    https://helloacm.com/check-integer-is-the-power-of-two/ *)
let is_power_of_2 n = (n - 1) land n = 0

let assert_side_invariant side =
  if side <= 1 then failwith (Printf.sprintf "cannot have grid side: %d" side) ;
  if not (is_power_of_2 side) then
    failwith (Printf.sprintf "side must be power of 2: %d" side)

let is_within_side ~x ~y side = 0 <= x && x < side && 0 <= y && y < side

let is_within x y t = is_within_side ~x ~y (Mut.side t)

let assert_within_side ~x ~y side =
  if not (is_within_side ~x ~y side) then
    invalid_argf "outside grid bounds: (%d, %d) but side=%d" x y side ()

let get x y t = Mut.get ~x ~z:y t

let make ~side init = Mut.create ~side init

include Container.Make (struct
  type 'a t = 'a grid

  let fold t ~init ~f =
    let side = Mut.side t in
    Mg_util.Range.fold 0 (side - 1) init (fun acc z ->
        Mg_util.Range.fold 0 (side - 1) acc (fun acc x ->
            let here = Mut.get ~x ~z t in
            f acc here ) )

  let iter = `Define_using_fold

  let length_impl t = Mut.side t * Mut.side t

  let length = `Custom length_impl
end)

module With_coords_impl = struct
  module T = struct
    type 'a t = T : 'a grid -> (int * int * 'a) t
  end

  include T

  let fold (type a) (T t : a t) ~init ~(f : _ -> a -> _) =
    let side = Mut.side t in
    Mg_util.Range.fold 0 (side - 1) init (fun acc z ->
        Mg_util.Range.fold 0 (side - 1) acc (fun acc x ->
            let here = Mut.get ~x ~z t in
            f acc (x, z, here) ) )

  let iter = `Define_using_fold

  let length = `Custom (fun (type a) (T t : a t) -> length t)
end

module With_coords = struct
  include With_coords_impl.T
  include Container.Make (With_coords_impl)
end

module Scan_impl = struct
  module T = struct
    type 'a t = T : 'a grid -> ('a * bool) t
  end

  include T

  let fold (type a) (T t : a t) ~init ~(f : _ -> a -> _) =
    let side = Mut.side t in
    Mg_util.Range.fold 0 (side - 1) init (fun acc z ->
        let acc =
          Mg_util.Range.fold 0 (side - 2) acc (fun acc x ->
              let here = Mut.get ~x ~z t in
              f acc (here, false) )
        in
        let last = Mut.get ~x:(side - 1) ~z t in
        f acc (last, true) )

  let iter = `Define_using_fold

  let length_impl (type a) (T t : a t) = Mut.side t * Mut.side t

  let length = `Custom length_impl
end

module Scan = struct
  include Scan_impl.T
  include Container.Make (Scan_impl)
end

let init ~side f = Mut.init ~side ~f:(fun ~x ~z -> f (x, z)) (f (0, 0))

let set x y new_v t = Mut.set ~x ~z:y new_v t ; t

let update x y ~f t =
  let n = f (Mut.get ~x ~z:y t) in
  Mut.set ~x ~z:y n t ; t

module Make (Args : sig
  type 'a t
end) =
struct
  type 'a data = 'a Args.t

  let set = set

  let init = init

  let map ~f t = Mut.map ~f:(fun ~x:_ ~z:_ v -> f v) t

  let zip_map ~f a b =
    if Mut.side a <> Mut.side b then
      failwithf "grid sides must match to zip: %d vs %d" a.side b.side () ;
    Mut.init_exact ~side:(Mut.side a) ~f:(fun ~x ~z ->
        let la = Mut.get ~x ~z a in
        let lb = Mut.get ~x ~z b in
        f la lb )

  let of_mut m = init ~side:(Mut.side m) (fun (x, z) -> Mut.get ~x ~z m)

  let map_of_mut = map
end

module Make0 (Args : sig
  type t
end) =
Make (struct
  include Args

  type _ t = Args.t
end)

module Poly = Make (struct
  type 'a t = 'a
end)

module Int = Make0 (Int)

let to_mut ?alloc_side ?fill t =
  let fill = match fill with Some f -> f | None -> get 0 0 t in
  let m = Mut.create ~side:(side t) ?alloc_side fill in
  With_coords.iter (With_coords.T t) ~f:(fun (x, z, v) -> Mut.set ~x ~z v m) ;
  m

let map_to_mut ?alloc_side ?fill ~f t =
  let fill = match fill with Some f -> f | None -> f ~x:0 ~z:0 (get 0 0 t) in
  let m = Mut.create ~side:(side t) ?alloc_side fill in
  With_coords.iter (With_coords.T t) ~f:(fun (x, z, v) ->
      Mut.set ~x ~z (f ~x ~z v) m ) ;
  m

let wrap_coord t x y = (x % Mut.side t, y % Mut.side t)

let print ?(sep = " ") ?(row_sep = "\n") ~f t =
  Scan.iter (Scan.T t) ~f:(fun (n, row_end) ->
      print_string (f n) ;
      print_string sep ;
      if row_end then print_string row_sep )

let%expect_test "inits and gets correct elements" =
  let print (x, y) = Printf.printf "%d, %d\n" x y in
  let g = init ~side:8 ident in
  print @@ get 0 0 g ;
  print @@ get 1 0 g ;
  print @@ get 0 1 g ;
  print @@ get 3 4 g ;
  print @@ get 7 7 g ;
  print @@ get 6 7 g ;
  [%expect "\n    0, 0\n    1, 0\n    0, 1\n    3, 4\n    7, 7\n    6, 7\n  "]

let%expect_test "set and compact" =
  let module Int_grid = Int in
  let open Int_grid in
  (* Ensure init compacts into a single leaf *)
  let g = Int_grid.init ~side:8 (fun _ -> 0) in
  print g ~f:string_of_int ;
  print_endline "" ;
  let g = g |> set 1 2 10 |> set 6 7 20 |> set 6 7 21 in
  print g ~f:string_of_int ;
  print_endline "" ;
  let g = g |> set 1 2 0 |> set 6 7 0 in
  print g ~f:string_of_int ;
  print_endline "" ;
  let g = g |> set 1 2 0 in
  print g ~f:string_of_int ;
  print_endline "" ;
  [%expect
    {|
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0

    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 10 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 21 0

    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0

    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0
     |}]