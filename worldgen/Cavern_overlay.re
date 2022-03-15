open! Core_kernel;

[@deriving bin_io]
type t = {
  floor: Grid.t(int),
  ceiling: Grid.t(int),
};

/*
   floor: 1 - 15
   ceiling: 30 - 40
   pillars: 20
 */

let min_dist_to_surface = 5;
let pillar_meet_elev = 20;
let magma_sea_elev = 3;

let floor_elev_at = (t, ~x, ~z) => Grid.get(~x, ~z, t.floor);
let ceiling_elev_at = (t, ~x, ~z) => Grid.get(~x, ~z, t.ceiling);

let draw_dense = (t, x, z) =>
  if (Grid.is_within(~x, ~z, t.floor)) {
    let floor = floor_elev_at(~x, ~z, t);
    let ceiling = ceiling_elev_at(~x, ~z, t);
    let color =
      if (floor >= ceiling) {
        0;
      } else if (floor <= magma_sea_elev) {
        0xFF0000;
      } else {
        let frac = float_of_int(floor) /. 50.;
        Mg_util.Color.blend(0x101010, 0xFFFFFF, frac);
      };
    Some(color);
  } else {
    None;
  };

let add_floor_pillars = (pillar_cloud: Point_cloud.t(bool), floor) => {
  Sparse_grid.iter(
    pillar_cloud.points, (_, Point_cloud.{px: x, py: y, value}) =>
    if (value) {
      Grid.Mut.update(
        floor, ~x=int_of_float(x), ~z=int_of_float(y), ~f=old_elev =>
        if (old_elev < pillar_meet_elev) {
          pillar_meet_elev + 5;
        } else {
          old_elev;
        }
      );
    }
  );
};

let add_ceiling_pillars = (pillar_cloud: Point_cloud.t(bool), ceiling) => {
  Sparse_grid.iter(
    pillar_cloud.points, (_, Point_cloud.{px: x, py: y, value}) =>
    if (value) {
      Grid.set(~x=int_of_float(x), ~z=int_of_float(y), -5, ceiling);
    }
  );
};

let prepare = () => {
  let (world, _) = Base_overlay.require();
  let r = 32;
  let start_side = Base_overlay.side(world) / r;
  let floor_cloud =
    Point_cloud.init(~side=start_side, ~spacing=4, (_x, _y) =>
      switch (Random.int(3)) {
      | 0 => 1.
      | 1 => 3. +. Random.float(12.)
      | _ => float_of_int(pillar_meet_elev + 5)
      }
    )
    |> Point_cloud.subdivide(~spacing=2)
    |> Point_cloud.subdivide_interpolate_nf(~n=5, ~spacing=1);
  let ceiling_cloud =
    Point_cloud.init(~side=start_side, ~spacing=4, (_x, _y) =>
      Random.float(10.) +. 3.
    );
  let pillar_cloud =
    Point_cloud.init(~side=start_side * 4, ~spacing=8, (_, _) => true);

  let floor =
    Tale.block("Creating floor", ~f=() => {
      let floor =
        Grid.init(
          ~side=start_side,
          ~alloc_side=start_side * Int.(2 ** 5),
          0,
          ~f=(~x, ~z) =>
          if (Base_overlay.ocean_at(~x=x * r, ~z=z * r, world)) {
            pillar_meet_elev + 5;
          } else {
            (
              Point_cloud.interpolate(
                floor_cloud,
                float_of_int(x),
                float_of_int(z),
              )
              |> int_of_float
            )
            + Random.int(2);
          }
        );
      for (_ in 1 to 2) {
        Grid.Subdivide_mut.subdivide_with_fill(
          floor,
          ~fill=Grid.Fill.random_avg,
        );
      };
      add_floor_pillars(pillar_cloud, floor);
      for (_ in 1 to 2) {
        Grid.Subdivide_mut.subdivide_with_fill(
          floor,
          ~fill=Grid.Fill.random_avg,
        );
      };
      Grid.Subdivide_mut.subdivide_with_fill(
        floor,
        ~fill=Grid.Fill.(line() **> avg),
      );
      floor;
    });

  let ceiling =
    Tale.block("Creating ceiling", ~f=() => {
      let ceiling =
        Grid.Mut.init(
          ~side=start_side,
          ~alloc_side=start_side * Int.(2 ** 5),
          0,
          ~f=(~x, ~z) =>
          if (Base_overlay.ocean_at(~x=x * r, ~z=z * r, world)) {
            (-5);
          } else {
            (
              Point_cloud.interpolate(
                ceiling_cloud,
                float_of_int(x),
                float_of_int(z),
              )
              |> int_of_float
            )
            + Random.int(3)
            - 2;
          }
        );
      for (_ in 1 to 2) {
        Grid.Subdivide_mut.subdivide_with_fill(
          ceiling,
          ~fill=Grid.Fill.random_avg,
        );
      };
      add_ceiling_pillars(pillar_cloud, ceiling);
      for (_ in 1 to 2) {
        Grid.Subdivide_mut.subdivide_with_fill(
          ceiling,
          ~fill=Grid.Fill.random_avg,
        );
      };
      Grid.Subdivide_mut.subdivide_with_fill(ceiling, ~fill=Grid.Fill.random);
      Grid.map_in_place(ceiling, ~f=(~x, ~z, d) => {
        Grid.get(~x, ~z, floor) + d
      });
      ceiling;
    });

  let t = {floor, ceiling};
  let l =
    Progress_view.push_update(
      ~title="cavern",
      ~draw_dense=draw_dense(t),
      (),
    );
  Progress_view.save(~side=Grid.side(floor), "cavern");
  Progress_view.remove_layer(l);
  t;
};

let apply_region = (cavern, region: Minecraft.Region.t) => {
  let canon = Overlay.Canon.require();
  let region = region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;

      let surface_elev = Overlay.Canon.elevation_at(~x, ~z, canon);
      let floor_elev = Grid.get(~x, ~z, cavern.floor);
      let ceiling_elev = Grid.get(~x, ~z, cavern.ceiling);

      /* Don't go above the maximum */
      let ceiling_elev =
        min(ceiling_elev, surface_elev - min_dist_to_surface);
      for (y in floor_elev + 1 to pred(ceiling_elev)) {
        set_block(~x, ~y, ~z, Minecraft.Block.Air, region);
      };
      /* Add magma sea */
      for (y in 1 to magma_sea_elev) {
        switch (get_block(region, ~x, ~y, ~z)) {
        | Air => set_block(~x, ~y, ~z, Lava, region)
        | _ => ()
        };
      };
    },
  );
};

let (require, prepare, apply) =
  Overlay.make_no_canon(
    "cavern",
    prepare,
    apply_region,
    bin_reader_t,
    bin_writer_t,
  );