type tile = River.tile;

let touchup = (grid: Grid.t(tile)) => {
  Grid.map(
    grid,
    (_x, _y, here) => {
      /* let here =
         switch (here) {
         | {river: true, elevation, _} as here => {
             ...here,
             elevation: elevation - 1,
           }
         | {river: false, _} as here => here
         }; */
      let here =
        if (here.elevation <= 0) {
          {...here, ocean: true, river: false};
        } else {
          {...here, ocean: false};
        };
      here;
    },
  );
};

let fill = (~fill_diags=false, a: tile, b: tile, c: tile, d: tile): tile => {
  /* Elevation is average */
  let elevation = (a.elevation + b.elevation + c.elevation + d.elevation) / 4;
  let ocean = elevation <= 0;

  /* River if opposing sides are river or ocean */
  let ar = a.river || a.ocean;
  let br = b.river || b.ocean;
  let cr = c.river || c.ocean;
  let dr = d.river || d.ocean;

  /* At least one needs to be an actual river, not ocean */
  let one_river = a.river || b.river || c.river || d.river;

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
  let ocean = elevation <= 0;
  let river = !ocean && river;

  {elevation, ocean, river};
};

/**
  site_subdivide performs context-aware diamond-square subdivision. Instead
  of picking a random neighbor to copy, precedence is given.

  For example, if two opposing neighbors are rivers, this tile must become a
  river.
 */

let phase =
  Phase_chain.(
    phase(
      "Subdivide into sites",
      Subdivide.overwrite_subdivide_with_fill(_, fill(~fill_diags=true)),
    )
    @> phase_repeat(
         1,
         "Subdivide into sites",
         Subdivide.overwrite_subdivide_with_fill(_, fill),
       )
    @> phase("Carve rivers down and touch-up ocean", touchup(_))
  );