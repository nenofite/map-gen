/** turns a grid colorizer into a callback for Progress_view */
let dense = (colorizer, grid, x, y) =>
  if (Grid.is_within(x, y, grid)) {
    let rgb = colorizer(Grid.get(x, y, grid));
    let r = (rgb land 0xFF0000) lsr 16;
    let g = (rgb land 0x00FF00) lsr 8;
    let b = rgb land 0x0000FF;
    Some((r, g, b));
  } else {
    None;
  };

let draw_with_colorizer = (~title=?, ~colorize, grid, side, layer) => {
  Progress_view.update(
    ~fit=(0, side, 0, side),
    ~title?,
    ~draw_dense=dense(colorize),
    ~state=grid,
    layer,
  );
};

/** creates a Phase_chain phase that updates the Progress_view */
let phase = (~title=?, layer, colorize) => {
  Phase_chain.phase("preview", grid => {
    draw_with_colorizer(~title?, ~colorize, grid, grid.Grid.side, layer);
    grid;
  });
};

module Make = (G: Grid.Griddable.S) => {
  /** turns a grid colorizer into a callback for Progress_view */
  let dense = (~colorize, grid, x, z) =>
    if (G.is_within(~x, ~z, grid)) {
      let rgb = colorize(G.get(~x, ~z, grid));
      let r = (rgb land 0xFF0000) lsr 16;
      let g = (rgb land 0x00FF00) lsr 8;
      let b = rgb land 0x0000FF;
      Some((r, g, b));
    } else {
      None;
    };

  let update_with_colorize = (~title=?, ~colorize, grid, layer) => {
    let s = G.side(grid);
    Progress_view.update(
      ~fit=(0, s, 0, s),
      ~title?,
      ~draw_dense=dense(~colorize),
      ~state=grid,
      layer,
    );
  };
};