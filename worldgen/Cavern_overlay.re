open Bin_prot.Std;

[@deriving bin_io]
type tile = {
  floor_elev: int,
  ceiling_elev: int,
};

[@deriving bin_io]
type t = Grid.t(tile);

/*
   floor: 1 - 15
   ceiling: 30 - 40
   pillars: 20
 */

let min_dist_to_surface = 5;
let pillar_meet_elev = 20;
let magma_sea_elev = 3;

let colorize = tile =>
  if (tile.floor_elev >= tile.ceiling_elev) {
    0;
  } else if (tile.floor_elev <= magma_sea_elev) {
    0xFF0000;
  } else {
    let frac = float_of_int(tile.floor_elev) /. 50.;
    Color.blend(0x101010, 0xFFFFFF, frac);
  };

let add_floor_pillars = (pillar_cloud: Point_cloud.t(bool), floor) => {
  Sparse_grid.fold(
    pillar_cloud.points,
    (_, Point_cloud.{px: x, py: y, value}, floor) =>
      if (value) {
        Grid.Compat.update(floor, int_of_float(x), int_of_float(y), old_elev =>
          if (old_elev < pillar_meet_elev) {
            pillar_meet_elev + 5;
          } else {
            old_elev;
          }
        );
      } else {
        floor;
      },
    floor,
  );
};

let add_ceiling_pillars = (pillar_cloud: Point_cloud.t(bool), ceiling) => {
  Sparse_grid.fold(
    pillar_cloud.points,
    (_, Point_cloud.{px: x, py: y, value}, ceiling) =>
      if (value) {
        Grid.Compat.update(
          ceiling, int_of_float(x), int_of_float(y), old_elev =>
          if (old_elev > pillar_meet_elev) {
            pillar_meet_elev - 5;
          } else {
            old_elev;
          }
        );
      } else {
        ceiling;
      },
    ceiling,
  );
};

let prepare = () => {
  let (world, _) = Base_overlay.require();
  let r = 32;
  let start_side = Base_overlay.side(world) / r;
  let floor_cloud =
    Point_cloud.init(~side=start_side, ~spacing=16, (_x, _y) =>
      switch (Random.int(3)) {
      | 0 => 1.
      | 1 => 3. +. Random.float(5.)
      | _ => float_of_int(pillar_meet_elev + 5)
      }
    );
  let ceiling_cloud =
    Point_cloud.init(~side=start_side, ~spacing=4, (_x, _y) =>
      Random.float(10.) +. 30.
    );
  let pillar_cloud =
    Point_cloud.init(~side=start_side * 4, ~spacing=8, (_, _) => true);
  let floor =
    Phase_chain.(
      run_all(
        phase("Init floor", () =>
          Grid.Compat.init(start_side, (x, y) =>
            if (Base_overlay.ocean_at(~x=x * r, ~z=y * r, world)) {
              pillar_meet_elev + 5;
            } else {
              (
                Point_cloud.interpolate(
                  floor_cloud,
                  float_of_int(x),
                  float_of_int(y),
                )
                |> int_of_float
              )
              + Random.int(2);
            }
          )
        )
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.random_avg),
           )
        @> phase("Add pillars", add_floor_pillars(pillar_cloud))
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "Line subdivide",
             Grid.Subdivide.subdivide_with_fill(
               _,
               Grid.Fill.(line() **> avg),
             ),
           ),
      )
    );
  let ceiling =
    Phase_chain.(
      run_all(
        phase("Init ceiling", () =>
          Grid.Compat.init(start_side, (x, y) =>
            if (Base_overlay.ocean_at(~x=x * r, ~z=y * r, world)) {
              pillar_meet_elev - 5;
            } else {
              (
                Point_cloud.interpolate(
                  ceiling_cloud,
                  float_of_int(x),
                  float_of_int(y),
                )
                |> int_of_float
              )
              + Random.int(3)
              - 2;
            }
          )
        )
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.random_avg),
           )
        @> phase("Add pillars", add_ceiling_pillars(pillar_cloud))
        @> phase_repeat(
             2,
             "Random avg subdivide",
             Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.random_avg),
           )
        @> phase_repeat(
             1,
             "Rough subdivide",
             Grid.Subdivide.subdivide_with_fill(_, Grid.Fill.random),
           ),
      )
    );
  Phase_chain.(
    run_all(
      phase("Combine", () =>
        Grid.Compat.zip_map(floor, ceiling, (floor_elev, ceiling_elev) =>
          {floor_elev, ceiling_elev}
        )
      )
      @> Draw.phase("cavern.png", colorize),
    )
  );
};

let apply_region = (cavern, region: Minecraft.Region.t) => {
  let (world, _) = Base_overlay.require();
  let region = region;
  Minecraft_converter.iter_blocks(
    region,
    (~x, ~z) => {
      open Minecraft.Region;

      let surface_elev = Base_overlay.elevation_at(~x, ~z, world);
      let {floor_elev, ceiling_elev} = Grid.Compat.at(cavern, x, z);

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
