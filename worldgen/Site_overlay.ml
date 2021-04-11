open Core_kernel

module type Site = sig
  type t

  val prepare : x:int -> z:int -> (t * int * int) option

  (* val canon_delta : t -> x:int -> z:int -> Overlay.Canon.delta *)
  val put_obstacles :
       t
    -> x:int
    -> z:int
    -> put:(Overlay.Canon.obstacle -> x:int -> z:int -> unit)
    -> unit

  val apply :
    t -> x:int -> z:int -> args:Minecraft_converter.region_args -> unit
end

type 'a site_list = ('a * int * int) list [@@deriving bin_io]

type t = {entrances: Entrance_site.t site_list; gates: Gate_site.t site_list}
[@@deriving bin_io]

type t_with_canon = t * Overlay.Canon.delta [@@deriving bin_io]

let overlay =
  Overlay.make_overlay "site" bin_reader_t_with_canon bin_writer_t_with_canon

let filter_and_grab_until_full list ~bag ~target ~f =
  let rec go list ~unwanted =
    let count = Bag.length bag in
    if count >= target then List.rev_append unwanted list
    else
      match list with
      | [] ->
          List.rev_append unwanted list
      | elem :: rest -> (
        match f elem with
        | Some a ->
            Bag.add_unit bag a ; go rest ~unwanted
        | None ->
            go rest ~unwanted:(elem :: unwanted) )
  in
  go list ~unwanted:[]

let prepare () =
  let canon = Overlay.Canon.require () in
  let available_sites =
    Point_cloud.make_int_list ~side:canon.side ~spacing:128 ()
    |> Mg_util.shuffle
  in
  let target_entrance_count = 3 in
  let entrances = Bag.create () in
  let target_gate_count = 10_000 in
  let gates = Bag.create () in
  available_sites
  |> filter_and_grab_until_full ~bag:entrances ~target:target_entrance_count
       ~f:(fun (x, z) -> Entrance_site.prepare ~x ~z)
  |> filter_and_grab_until_full ~bag:gates ~target:target_gate_count
       ~f:(fun (x, z) -> Gate_site.prepare ~x ~z)
  |> ignore ;
  let obs = Grid.Mut.create ~side:canon.side Overlay.Canon.Obstacle.Clear in
  let put_obs level ~x ~z =
    Grid.Mut.update_opt obs ~x ~z ~f:(fun old_level ->
        Overlay.Canon.Obstacle.max level old_level )
    |> ignore
  in
  Bag.iter entrances ~f:(fun (t, x, z) ->
      Entrance_site.put_obstacles t ~x ~z ~put:put_obs ) ;
  Bag.iter gates ~f:(fun (t, x, z) ->
      Gate_site.put_obstacles t ~x ~z ~put:put_obs ) ;
  let canond =
    Overlay.Canon.make_delta
      ~obstacles:(`Add (Overlay.Canon.Obstacles.of_mut obs))
      ()
  in
  ({entrances= Bag.to_list entrances; gates= Bag.to_list gates}, canond)

let apply_progress_view sites =
  let l = Progress_view.push_layer () in
  Progress_view.update ~title:"Sites!"
    ~draw_sparse:(fun () d ->
      let {entrances; gates} = sites in
      let red = (255, 0, 0) in
      List.iter entrances ~f:(fun (_, x, z) -> d ~size:1 x z red) ;
      let black = (0, 0, 0) in
      List.iter gates ~f:(fun (_, x, z) -> d ~size:1 x z black) )
    ~state:() l ;
  ()

let after_prepare (sites, canond) =
  Overlay.Canon.push_delta canond ;
  apply_progress_view sites ;
  ()

let apply_standard args ~x ~z template =
  Building.apply_template args ~x ~z template

let apply_region (sites, _) (args : Minecraft_converter.region_args) : unit =
  let {entrances; gates} = sites in
  let apply_if_within apply (t, x, z) =
    if Minecraft.Region.is_within ~x ~y:0 ~z args.region then
      apply t ~x ~z ~args
  in
  List.iter entrances ~f:(apply_if_within Entrance_site.apply) ;
  List.iter gates ~f:(apply_if_within Gate_site.apply)

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare ~apply:apply_region overlay
