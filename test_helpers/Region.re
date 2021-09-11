open! Core_kernel;
open Text_grid;

let show_region_top_down =
    (~show_block, ~resolve_multi=List.hd_exn, r: Minecraft.Region.t)
    : text_grid => {
  let (ox, oz) = Minecraft.Region.region_offset(r);
  let top_down_at = (~x, ~z) => {
    let x = x + ox;
    let z = z + oz;
    let matches =
      Mg_util.Range.map(0, Minecraft.Region.block_per_region_vertical - 1, y =>
        show_block(Minecraft.Region.get_block(~x, ~y, ~z, r))
      )
      |> List.filter_map(~f=opt => opt);
    switch (matches) {
    | [] => " "
    | [single] => single
    | multi => resolve_multi(multi)
    };
  };
  Grid.Mut.init(
    ~side=Minecraft.Region.block_per_region_side,
    ~f=top_down_at,
    "",
  );
};

let show_region_south_north =
    (~show_block, ~resolve_multi=List.hd_exn, r: Minecraft.Region.t)
    : text_grid => {
  let (ox, oz) = Minecraft.Region.region_offset(r);
  let south_north_at = (~x, ~z as y) => {
    let x = x + ox;
    let y = Minecraft.Region.block_per_region_vertical - y - 1;
    if (y < 0) {
      " ";
    } else {
      let matches =
        Mg_util.Range.map(
          oz, oz + Minecraft.Region.block_per_region_side - 1, z =>
          show_block(Minecraft.Region.get_block(~x, ~y, ~z, r))
        )
        |> List.filter_map(~f=opt => opt);
      switch (matches) {
      | [] => " "
      | [single] => single
      | multi => resolve_multi(multi)
      };
    };
  };
  Grid.Mut.init(
    ~side=Minecraft.Region.block_per_region_side,
    ~f=south_north_at,
    "",
  );
};

let show_entities = (r: Minecraft.Region.t): unit => {
  print_s(
    [%sexp_of: list(Minecraft.Entity.t)](Minecraft.Region.all_entities(r)),
  );
};

let cached_region = ref(None: option(Minecraft.Region.t));

let build_test_region = (~rx=0, ~rz=0, ()) => {
  open Minecraft.Region;
  let r =
    switch (cached_region^) {
    | Some(r) =>
      reset(~rx, ~rz, r);
      r;
    | None =>
      let r = create(~rx, ~rz);
      cached_region := Some(r);
      r;
    };

  let elevation = 40;
  iter_region_xz(
    r,
    ~f=(~x, ~z) => {
      set_block(~x, ~y=0, ~z, Bedrock, r);
      for (y in 1 to elevation - 1) {
        set_block(~x, ~y, ~z, Dirt, r);
      };
      set_block(~x, ~y=elevation, ~z, Grass_block, r);
    },
  );
  r;
};
