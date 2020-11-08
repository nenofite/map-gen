open Core_kernel

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

let assert_side_invariant side =
  if side <= 1 then failwith (Printf.sprintf "cannot have grid side: %d" side)

let get x y t =
  let rec get_from_node x y side node =
    match node with
    | Leaf v -> v
    | Quad (nw, ne, sw, se) ->
      assert_side_invariant side;
      let half = side / 2 in
      match x < half, y < half with
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
    (match x < half, y < half with
     | false, false -> let new_nw = force_set_node x y new_v half l in Quad (new_nw, l, l, l)
     | true, false -> let new_ne = force_set_node (x - half) y new_v half l in Quad (l, new_ne, l, l)
     | false, true -> let new_sw = force_set_node x (y - half) new_v half l in Quad (l, l, new_sw, l)
     | true, true -> let new_se = force_set_node (x - half) (y - half) new_v half l in Quad (l, l, l, new_se))
  | Quad (nw, ne, sw, se) ->
    (match x < half, y < half with
     | false, false -> let new_nw = force_set_node x y new_v half nw in Quad (new_nw, ne, sw, se)
     | true, false -> let new_ne = force_set_node (x - half) y new_v half ne in Quad (nw, new_ne, sw, se)
     | false, true -> let new_sw = force_set_node x (y - half) new_v half sw in Quad (nw, ne, new_sw, se)
     | true, true -> let new_se = force_set_node (x - half) (y - half) new_v half se in Quad (nw, ne, sw, new_se))
;;

let force_set x y new_v t = { t with root = force_set_node x y new_v t.side t.root }

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

let make ~side init = { side; root = Leaf init }

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

module Scan = Container.Make(struct
    type 'a t = 'a grid

    let fold t ~init ~f =
      let open Mg_util.Range in
      fold 1 t.side init (fun accum z ->
          fold 1 t.side accum (fun accum x ->
              let here = get x z t in
              f accum here
            )
        )
    ;;

    let iter = `Define_using_fold

    let length_impl t = t.side * t.side
    let length = `Custom length_impl
  end)

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

module Make_setters(Args: sig
    type data
    val (=) : data -> data -> bool
  end) = struct
  type data = Args.data

  let set x y new_v t =
    let rec set_if_different x y new_v side node =
      let half = side / 2 in
      match node with
      | Leaf current_v as l ->
        if Args.(current_v = new_v) then
          None
        else
          Some (force_set_node x y new_v half l)
      | Quad (nw, ne, sw, se) ->
        let open Option.Monad_infix in
        (match x < half, y < half with
         | false, false -> set_if_different x y new_v half nw
           >>| fun new_nw -> Quad (new_nw, ne, sw, se)
         | true, false -> set_if_different (x - half) y new_v half ne
           >>| fun new_ne -> Quad (nw, new_ne, sw, se)
         | false, true -> set_if_different x (y - half) new_v half sw
           >>| fun new_sw -> Quad (nw, ne, new_sw, se)
         | true, true -> set_if_different (x - half) (y - half) new_v half se
           >>| fun new_se -> Quad (nw, ne, sw, new_se))
    in
    match set_if_different x y new_v t.side t.root with
    | Some new_root -> { t with root = new_root }
    | None -> t
  ;;

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
end