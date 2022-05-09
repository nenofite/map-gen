open Core;

module Pq = Priority_queue.Lite;

type needs_update_list('a) = Pq.t('a);

let rec flood_gen = (~init, ~live: needs_update_list(_), ~spread) =>
  switch (Pq.pop(live)) {
  | Some((level, coord, needs_update)) =>
    let (acc, updated_coords) = spread(init, ~level, coord);
    let next_live =
      List.fold(updated_coords, ~init=needs_update, ~f=(ls, (level, coord)) =>
        Pq.insert(~p=level, coord, ls)
      );
    flood_gen(~init=acc, ~live=next_live, ~spread);
  | None => init
  };

[@deprecated]
let flood =
    (grid: Grid.t('a), ~initial: needs_update_list((int, int)), ~spread) => {
  let spread = (grid, ~level as _, (x, y)) => spread(grid, x, y);
  flood_gen(~init=grid, ~live=initial, ~spread);
};