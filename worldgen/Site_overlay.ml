open Core_kernel

module type Site = sig
  type t

  val prepare : x:int -> z:int -> (t * int * int) option

  val update_canon : t -> Canonical_overlay.delta

  val apply :
    t -> x:int -> z:int -> args:Minecraft_converter.region_args -> unit
end

(* type site = Cavern_entrance of Entrance_site.t | Gate of Gate_site.t
[@@deriving bin_io] *)

type 'a site_list = ('a * int * int) list [@@deriving bin_io]

type t = {entrances: Entrance_site.t site_list; gates: Gate_site.t site_list}
[@@deriving bin_io]

let overlay = Overlay.make_overlay "site" bin_reader_t bin_writer_t

let prepare () =
  let canon = Canonical_overlay.require () in
  let available_sites =
    Point_cloud.make_int_list ~side:canon.side ~spacing:128 ()
    |> Mg_util.shuffle
  in
  let target_entrance_count = 3 in
  let entrances = Bag.create () in
  let target_gate_count = 100 in
  let gates = Bag.create () in
  let rec filter_and_grab_until_full list ~unwanted ~bag ~target ~f =
    let count = Bag.length bag in
    if count >= target then List.rev_append unwanted list
    else
      match list with
      | [] ->
          List.rev_append unwanted list
      | elem :: rest -> (
        match f elem with
        | Some a ->
            Bag.add_unit bag a ;
            filter_and_grab_until_full rest ~unwanted ~bag ~target ~f
        | None ->
            filter_and_grab_until_full rest ~unwanted:(elem :: unwanted) ~bag
              ~target ~f )
  in
  available_sites
  |> filter_and_grab_until_full ~unwanted:[] ~bag:entrances
       ~target:target_entrance_count ~f:(fun (x, z) ->
         Entrance_site.prepare ~x ~z )
  |> filter_and_grab_until_full ~unwanted:[] ~bag:gates
       ~target:target_gate_count ~f:(fun (x, z) -> Gate_site.prepare ~x ~z)
  |> ignore ;
  (* TODO canon deltas *)
  {entrances= Bag.to_list entrances; gates= Bag.to_list gates}

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

let after_prepare sites = apply_progress_view sites

let apply_standard args ~x ~z template =
  Building.apply_template args ~x ~z template

let apply_region sites (args : Minecraft_converter.region_args) : unit =
  let {entrances; gates} = sites in
  let apply_if_within apply (t, x, z) =
    if Minecraft.Region.is_within ~x ~y:0 ~z args.region then
      apply t ~x ~z ~args
  in
  List.iter entrances ~f:(apply_if_within Entrance_site.apply) ;
  List.iter gates ~f:(apply_if_within Gate_site.apply)
(* Sparse_grid.iter sites.Point_cloud.points
   (fun _ Point_cloud.{px= x; py= z; value= site} ->
     let x = int_of_float x in
     let z = int_of_float z in
     if Minecraft.Region.is_within ~x ~y:0 ~z args.region then
       match site with
       | Some (Cavern_entrance tube_depth) ->
           apply_cavern_entrance args ~tube_depth ~x ~z
       | None ->
           () ) *)

let require, prepare, apply =
  Overlay.make_lifecycle ~prepare ~after_prepare ~apply:apply_region overlay
