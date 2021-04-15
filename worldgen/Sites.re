open Core_kernel;

module Tile = River.Tile;
type tile = Tile.t;

let touchup = (grid: Grid.Mut.t(tile)) => {
  Grid.Mut.map(
    grid,
    ~f=(~x as _, ~z as _, here) => {
      let here =
        if (here.Tile.elevation <= Heightmap.sea_level) {
          {...here, ocean: true, river: false};
        } else {
          {...here, ocean: false};
        };
      here;
    },
  )
  |> ignore;
};

let fill = (~fill_diags, a: tile, b: tile, c: tile, d: tile): tile => {
  /* Elevation forms lines, otherwise is average */
  let elevation =
    switch (a.elevation == c.elevation, b.elevation == d.elevation) {
    | (true, true) => Random.bool() ? a.elevation : b.elevation
    | (true, false) => a.elevation
    | (false, true) => b.elevation
    | (false, false) =>
      (a.elevation + b.elevation + c.elevation + d.elevation) / 4
    };
  let ocean = elevation < Heightmap.sea_level;

  /* River if opposing sides are river or ocean */
  let ar = River.has_water(a);
  let br = River.has_water(b);
  let cr = River.has_water(c);
  let dr = River.has_water(d);

  /* At least one needs to be an actual river, not ocean */
  let one_river =
    River.has_river(a)
    || River.has_river(b)
    || River.has_river(c)
    || River.has_river(d);

  let river =
    !ocean
    && one_river
    && (
      switch (ar, br, cr, dr) {
      /* opposing sides */
      | (true, _, true, _)
      | (_, true, _, true) => true
      /* two on same side */
      | (true, true, _, _)
      | (_, true, true, _)
      | (_, _, true, true)
      | (true, _, _, true) => fill_diags || Random.int(100) < 33
      /* one or none */
      | _ => false
      }
    );

  /* If here is a river, then take min elevation instead of average */
  let elevation =
    if (river) {
      min(min(a.elevation, b.elevation), min(c.elevation, d.elevation));
    } else {
      elevation;
    };
  /* Now that we've updated elevation, reconsider being an ocean */
  let ocean = elevation < Heightmap.sea_level;
  let river = !ocean && river;

  {elevation, ocean, river};
};

/**
  site_subdivide performs context-aware diamond-square subdivision. Instead
  of picking a random neighbor to copy, precedence is given.

  For example, if two opposing neighbors are rivers, this tile must become a
  river.
 */

let phase = m =>
  Tale.block("Sites", ~f=() => {
    Subdivide_mut.overwrite_subdivide_with_fill(
      m,
      ~fill=fill(~fill_diags=true),
    );
    Subdivide_mut.overwrite_subdivide_with_fill(
      m,
      ~fill=fill(~fill_diags=false),
    );
    touchup(m);
    m;
  });
