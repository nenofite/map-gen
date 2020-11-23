open Core_kernel

module Griddable = Griddable
module Mut = Mut

type 'a node =
  | Leaf of 'a
  | Quad of 'a node * 'a node * 'a node * 'a node
[@@deriving sexp, bin_io]

type sel = Nw | Ne | Sw | Se

type 'a t = {
  side: int;
  root: 'a node;
}
[@@deriving sexp, bin_io]

(* This makes it easier to include t in submodules *)
type 'a grid = 'a t

let side t = t.side

(** very efficient check if an int is a power of 2. I'm not smart enough to
    think of this; taken from
    https://helloacm.com/check-integer-is-the-power-of-two/ *)
let is_power_of_2 n = (n - 1) land n = 0

let assert_side_invariant side =
  if side <= 1 then failwith (Printf.sprintf "cannot have grid side: %d" side);
  if not (is_power_of_2 side) then failwith (Printf.sprintf "side must be power of 2: %d" side)

let is_within_side ~x ~y side = 0 <= x && x < side && 0 <= y && y < side

let is_within x y t = is_within_side ~x ~y t.side

let assert_within_side ~x ~y side =
  if not (is_within_side ~x ~y side) then
    invalid_argf "outside grid bounds: (%d, %d) but side=%d" x y side ()
;;

let get x y t =
  assert_within_side ~x ~y t.side;
  let rec get_from_node x y side node =
    match node with
    | Leaf v -> v
    | Quad (nw, ne, sw, se) ->
      let half = side / 2 in
      match x >= half, y >= half with
      | false, false -> get_from_node x y half nw
      | true, false -> get_from_node (x - half) y half ne
      | false, true -> get_from_node x (y - half) half sw
      | true, true -> get_from_node (x - half) (y - half) half se
  in
  get_from_node x y t.side t.root
;;

let rec get_path path node =
  match path, node with
  | [], node -> node
  | _ :: _, (Leaf _ as leaf) -> leaf
  | Nw :: path_rest, Quad (nw, _, _, _) -> get_path path_rest nw
  | Ne :: path_rest, Quad (_, ne, _, _) -> get_path path_rest ne
  | Sw :: path_rest, Quad (_, _, sw, _) -> get_path path_rest sw
  | Se :: path_rest, Quad (_, _, _, se) -> get_path path_rest se
;;

let rec force_set_node x y new_v side node =
  let half = side / 2 in
  match node with
  | _ when side = 1 -> Leaf new_v
  | Leaf _ as l -> 
    (match x >= half, y >= half with
     | false, false -> let new_nw = force_set_node x y new_v half l in Quad (new_nw, l, l, l)
     | true, false -> let new_ne = force_set_node (x - half) y new_v half l in Quad (l, new_ne, l, l)
     | false, true -> let new_sw = force_set_node x (y - half) new_v half l in Quad (l, l, new_sw, l)
     | true, true -> let new_se = force_set_node (x - half) (y - half) new_v half l in Quad (l, l, l, new_se))
  | Quad (nw, ne, sw, se) ->
    (match x >= half, y >= half with
     | false, false -> let new_nw = force_set_node x y new_v half nw in Quad (new_nw, ne, sw, se)
     | true, false -> let new_ne = force_set_node (x - half) y new_v half ne in Quad (nw, new_ne, sw, se)
     | false, true -> let new_sw = force_set_node x (y - half) new_v half sw in Quad (nw, ne, new_sw, se)
     | true, true -> let new_se = force_set_node (x - half) (y - half) new_v half se in Quad (nw, ne, sw, new_se))
;;

let force_set x y new_v t =
  assert_within_side ~x ~y t.side;
  { t with root = force_set_node x y new_v t.side t.root }

let rec force_set_path path new_v node =
  match path, node with
  | [], _old -> Leaf new_v
  | Nw :: path_rest, (Leaf _ as l) ->
    let nw = force_set_path path_rest new_v l in Quad (nw, l, l, l)
  | Ne :: path_rest, (Leaf _ as l) ->
    let ne = force_set_path path_rest new_v l in Quad (l, ne, l, l)
  | Sw :: path_rest, (Leaf _ as l) ->
    let sw = force_set_path path_rest new_v l in Quad (l, l, sw, l)
  | Se :: path_rest, (Leaf _ as l) ->
    let se = force_set_path path_rest new_v l in Quad (l, l, l, se)
  | Nw :: path_rest, Quad (nw, ne, sw, se) ->
    let new_nw = force_set_path path_rest new_v nw in Quad (new_nw, ne, sw, se)
  | Ne :: path_rest, Quad (nw, ne, sw, se) ->
    let new_ne = force_set_path path_rest new_v ne in Quad (nw, new_ne, sw, se)
  | Sw :: path_rest, Quad (nw, ne, sw, se) ->
    let new_sw = force_set_path path_rest new_v sw in Quad (nw, ne, new_sw, se)
  | Se :: path_rest, Quad (nw, ne, sw, se) ->
    let new_se = force_set_path path_rest new_v se in Quad (nw, ne, sw, new_se)
;;

let make ~side init =
  assert_side_invariant side;
  { side; root = Leaf init }

include Container.Make(struct
    type 'a t = 'a grid

    let fold t ~init ~f =
      let rec go node accum ~side ~f =
        match node with
        | Leaf x ->
          Mg_util.Range.fold 1 (side * side) accum (fun a _ -> f a x)
        | Quad (nw, ne, sw, se) ->
          let half = side / 2 in
          go nw accum ~side: half ~f |>
          go ne ~side: half ~f |>
          go sw ~side: half ~f |>
          go se ~side: half ~f
      in
      go t.root init ~side: t.side ~f
    ;;

    let iter = `Define_using_fold

    let length_impl t = t.side * t.side
    let length = `Custom length_impl
  end)

module With_coords_impl = struct
  module T = struct
    type 'a t =
      | T: 'a grid -> (int * int * 'a) t
  end
  include T

  let fold (type a) ((T t): a t) ~init ~(f: _ -> a -> _) = (
    let rec go node accum ~x ~y ~side ~f =
      match node with
      | Leaf n ->
        let open Mg_util.Range in
        fold y (y + side - 1) accum (fun accum yi ->
            fold x (x + side - 1) accum (fun accum xi ->
                f accum (xi, yi, n)
              )
          )
      | Quad (nw, ne, sw, se) ->
        let half = side / 2 in
        go nw accum ~x ~y ~side: half ~f |>
        go ne ~x:(x + half) ~y ~side: half ~f |>
        go sw ~x ~y:(y + half) ~side: half ~f |>
        go se ~x:(x + half) ~y:(y + half) ~side: half ~f
    in
    go t.root init ~x: 0 ~y: 0 ~side: t.side ~f
  )

  let iter = `Define_using_fold

  let length = `Custom (fun (type a) ((T t): a t) -> length t)
end
module With_coords = struct
  include With_coords_impl.T
  include Container.Make(With_coords_impl)
end

module Scan_impl = struct
  module T = struct
    type 'a t =
      | T: 'a grid -> ('a * bool) t
  end
  include T

  let fold (type a) ((T t): a t) ~init ~(f: _ -> a -> _) = (
    let open Mg_util.Range in
    fold 0 (t.side - 1) init (fun accum z ->
        let accum = fold 0 (t.side - 2) accum (fun accum x ->
            let here = get x z t in
            f accum (here, false)
          ) in
        let last = get (t.side - 1) z t in
        f accum (last, true)
      )
  )

  let iter = `Define_using_fold

  let length_impl (type a) ((T t): a t) = t.side * t.side
  let length = `Custom length_impl
end
module Scan = struct
  include Scan_impl.T
  include Container.Make(Scan_impl)
end

module Leafwise_impl = struct
  module T = struct
    type 'a t =
      | T: 'a grid -> ('a * int) t
  end
  include T

  let fold (type a) ((T t): a t) ~init ~(f: _ -> a -> _) =
    let rec go node accum ~side ~f =
      match node with
      | Leaf x -> f accum (x, side)
      | Quad (nw, ne, sw, se) ->
        let half = side / 2 in
        go nw accum ~side: half ~f |>
        go ne ~side: half ~f |>
        go sw ~side: half ~f |>
        go se ~side: half ~f
    in
    go t.root init ~side: t.side ~f
  ;;

  let iter = `Define_using_fold

  (* In this case, length is the number of leaves *)
  let length = `Define_using_fold
end
module Leafwise = struct
  include Leafwise_impl.T
  include Container.Make(Leafwise_impl)
end

module Make(Args: sig
    type 'a t
    val (=) : 'a t -> 'a t -> bool
  end) = struct
  type 'a data = 'a Args.t

  let compact_quad = function
    | Leaf nw as l, Leaf ne, Leaf sw, Leaf se when Args.(nw = ne && ne = sw && sw = se) -> l
    | (nw, ne, sw, se) -> Quad (nw, ne, sw, se)

  let update x y ~f t = (
    let rec upd_if_different x y ~f side node = (
      match node with
      | Leaf current_v as l ->
        let new_v = f current_v in
        if Args.(current_v = new_v) then
          None
        else
          Some (force_set_node x y new_v side l)
      | Quad (nw, ne, sw, se) ->
        let open Option.Monad_infix in
        let half = side / 2 in
        (match x >= half, y >= half with
         | false, false -> upd_if_different x y ~f half nw
           >>| fun new_nw -> compact_quad (new_nw, ne, sw, se)
         | true, false -> upd_if_different (x - half) y ~f half ne
           >>| fun new_ne -> compact_quad (nw, new_ne, sw, se)
         | false, true -> upd_if_different x (y - half) ~f half sw
           >>| fun new_sw -> compact_quad (nw, ne, new_sw, se)
         | true, true -> upd_if_different (x - half) (y - half) ~f half se
           >>| fun new_se -> compact_quad (nw, ne, sw, new_se))
    ) in
    match upd_if_different x y ~f t.side t.root with
    | Some new_root -> { t with root = new_root }
    | None -> t
  )

  let set x y new_v t = update x y ~f: (fun _ -> new_v) t

  let set_path path new_v t =
    let rec set_path_node path new_v node =
      let open Option.Monad_infix in
      match path, node with
      (* Short-circuit: No change if we encounter a leaf with the same value *)
      | _, Leaf v when Args.(v = new_v) -> None
      (* Value doesn't match and we'd like to replace this node *)
      | [], _ -> Some (Leaf new_v)
      (* If we encounter a leaf with different value, split it *)
      | path, (Leaf _ as l) -> Some (force_set_path path new_v l)
      (* Otherwise walk down the tree *)
      | Nw :: path_rest, Quad (nw, ne, sw, se) ->
        set_path_node path_rest new_v nw >>| fun new_nw -> Quad (new_nw, ne, sw, se)
      | Ne :: path_rest, Quad (nw, ne, sw, se) ->
        set_path_node path_rest new_v ne >>| fun new_ne -> Quad (nw, new_ne, sw, se)
      | Sw :: path_rest, Quad (nw, ne, sw, se) ->
        set_path_node path_rest new_v sw >>| fun new_sw -> Quad (nw, ne, new_sw, se)
      | Se :: path_rest, Quad (nw, ne, sw, se) ->
        set_path_node path_rest new_v se >>| fun new_se -> Quad (nw, ne, sw, new_se)
    in
    match set_path_node path new_v t.root with
    | Some new_root -> { t with root = new_root }
    | None -> t
  ;;

  let init ~side f = (
    assert_side_invariant side;
    let f x y = f (x, y) in
    let rec go side min_x min_y =
      if side > 1 then
        let half = side / 2 in
        compact_quad (
          go half min_x min_y,
          go half (min_x + half) min_y,
          go half min_x (min_y + half),
          go half (min_x + half) (min_y + half)
        )
      else
        Leaf (f min_x min_y)
    in
    { side; root = go side 0 0 }
  )

  let map ~f t = (
    let rec go old_node =
      match old_node with
      | Leaf old_v -> Leaf (f old_v)
      | Quad (nw, ne, sw, se) -> compact_quad (go nw, go ne, go sw, go se)
    in
    { t with root = go t.root }
  )

  let zip_map ~f a b = (
    if a.side <> b.side then failwithf "grid sides must match to zip: %d vs %d" a.side b.side ();
    let rec go a b = match a, b with
      | Leaf la, Leaf lb -> Leaf (f la lb)
      | Leaf _, Quad (nwb, neb, swb, seb) -> compact_quad (go a nwb, go a neb, go a swb, go a seb)
      | Quad (nwa, nea, swa, sea), Leaf _ -> compact_quad (go nwa b, go nea b, go swa b, go sea b)
      | Quad (nwa, nea, swa, sea), Quad (nwb, neb, swb, seb) -> compact_quad (go nwa nwb, go nea neb, go swa swb, go sea seb)
    in
    { side = a.side; root = go a.root b.root }
  )

  let compact t =
    let rec compact_node node =
      match node with
      | Leaf _ -> None
      | Quad (nw, ne, sw, se) ->
        let opt_nw = compact_node nw in
        let opt_ne = compact_node ne in
        let opt_sw = compact_node sw in
        let opt_se = compact_node se in
        let new_nw = Option.value opt_nw ~default: nw in
        let new_ne = Option.value opt_ne ~default: ne in
        let new_sw = Option.value opt_sw ~default: sw in
        let new_se = Option.value opt_se ~default: se in
        match new_nw, new_ne, new_sw, new_se with
        | Leaf nw as l, Leaf ne, Leaf sw, Leaf se when Args.(nw = ne && ne = sw && sw = se) -> Some l
        | _ -> match opt_nw, opt_ne, opt_sw, opt_se with
          | None, None, None, None -> None
          | _ -> Some (Quad (new_nw, new_ne, new_sw, new_se))
    in
    match compact_node t.root with
    | Some new_root -> { t with root = new_root }
    | None -> t
  ;;

  let of_mut m = init ~side:(Mut.side m) (fun (x, z) -> Mut.get ~x ~z m)

  let map_of_mut ~f m = init ~side:(Mut.side m) (fun (x, z) -> f ~x ~z (Mut.get ~x ~z m))
end

module Make0(Args: sig
    type t
    val (=) : t -> t -> bool
  end) = Make(struct
    include Args
    type _ t = Args.t
  end)

module Poly = Make(struct
    type 'a t = 'a
    let (=) = Poly.(=)
  end)

let to_mut ?alloc_side ?fill t = (
  let fill = match fill with
    | Some f -> f
    | None -> get 0 0 t
  in
  let m = Mut.create ~side:(side t) ?alloc_side fill in
  With_coords.iter (With_coords.T t) ~f:(fun (x, z, v) ->
      Mut.set ~x ~z v m
    );
  m
)

let map_to_mut ?alloc_side ?fill ~f t = (
  let fill = match fill with
    | Some f -> f
    | None -> f ~x:0 ~z:0 (get 0 0 t)
  in
  let m = Mut.create ~side:(side t) ?alloc_side fill in
  With_coords.iter (With_coords.T t) ~f:(fun (x, z, v) ->
      Mut.set ~x ~z (f ~x ~z v) m
    );
  m
)

let wrap_coord t x y = x % t.side, y % t.side

(** creates a non-compact tree. It's more space-efficient to use
    Make_setters(...).init when possible *)
let init ~side f =
  assert_side_invariant side;
  let f x y = f (x, y) in
  let rec go side min_x min_y =
    if side > 1 then
      let half = side / 2 in
      Quad (
        go half min_x min_y,
        go half (min_x + half) min_y,
        go half min_x (min_y + half),
        go half (min_x + half) (min_y + half)
      )
    else
      Leaf (f min_x min_y)
  in
  { side; root = go side 0 0 }
;;

let print ?(sep = " ") ?(row_sep = "\n") ~f t = (
  Scan.iter (Scan.T t) ~f: (fun (n, row_end) ->
      print_string (f n);
      print_string sep;
      if row_end then print_string row_sep
    )
)

let%expect_test "inits and gets correct elements" = (
  let print (x, y) = Printf.printf "%d, %d\n" x y in
  let g = init ~side: 8 ident in
  print @@ get 0 0 g;
  print @@ get 1 0 g;
  print @@ get 0 1 g;
  print @@ get 3 4 g;
  print @@ get 7 7 g;
  print @@ get 6 7 g;
  [%expect "
    0, 0
    1, 0
    0, 1
    3, 4
    7, 7
    6, 7
  "]
)

let%expect_test "force_sets, scans, and counts leaves" = (
  let g = make 0 ~side: 8 in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  let g = g |> force_set 1 2 10 |>
          force_set 6 7 20 |>
          force_set 6 7 21 in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  [%expect "
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    leaves: 1

    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 10 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 21 0 
    leaves: 16
  "]
)

let%expect_test "set and compact" = (
  let module Int_grid = Make0(Int) in
  let open Int_grid in
  (* Ensure init compacts into a single leaf *)
  let g = Int_grid.init ~side: 8 (fun _ -> 0) in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  let g = g |> set 1 2 10 |>
          set 6 7 20 |>
          set 6 7 21 in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  let g = g |> set 1 2 0 |>
          set 6 7 0 in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  let g = g |> force_set 1 2 0 |> compact in
  print g ~f: string_of_int;
  Printf.printf "leaves: %d\n\n" (Leafwise.length (Leafwise.T g));

  [%expect "
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    leaves: 1

    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 10 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 21 0 
    leaves: 16

    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    leaves: 1

    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 
    leaves: 1
  "]
)