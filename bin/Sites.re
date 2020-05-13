type tile = River.tile;

let fill = (~fill_diags=false, a: tile, b: tile, c: tile, d: tile): tile => {
  /* Elevation is average */
  let elevation = (a.elevation + b.elevation + c.elevation + d.elevation) / 4;
  let ocean = elevation <= 0;

  /* River if opposing sides are river or ocean */
  let ar = a.river || a.ocean;
  let br = b.river || b.ocean;
  let cr = c.river || c.ocean;
  let dr = d.river || d.ocean;

  let river =
    !ocean
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
  );