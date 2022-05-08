open! Core;

let force_overlays: ref(list(string)) = ref([]);

let set_force_overlays = (overlays: list(string)) =>
  force_overlays := overlays;

let should_force_overlay = (overlay: string) =>
  List.mem(force_overlays^, overlay, ~equal=String.equal)
  || List.mem(force_overlays^, "all", ~equal=String.equal);

let max_regions = ref(None: option(int));

let set_max_regions = n => max_regions := n;

let max_regions = () => max_regions^;