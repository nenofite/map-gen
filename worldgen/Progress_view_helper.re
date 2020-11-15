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

/** creates a Phase_chain phase that updates the Progress_view */
let phase = (~title=?, layer, colorizer) => {
  Phase_chain.phase("preview", grid => {
    Progress_view.update(
      ~fit=(0, grid.Grid.side, 0, grid.Grid.side),
      ~title?,
      ~draw_dense=dense(colorizer),
      ~state=grid,
      layer,
    );
    grid;
  });
};