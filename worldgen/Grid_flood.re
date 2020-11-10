open Core_kernel;

type needs_update_list('a) = list((int, list('a)));

let rec pop_update =
        (needs_update: needs_update_list('a))
        : option((int, 'a, needs_update_list('a))) =>
  switch (needs_update) {
  | [(level, [coord, ...level_rest]), ...update_rest] =>
    /* This level has some available, so pop one off */
    let needs_update = [(level, level_rest), ...update_rest];
    Some((level, coord, needs_update));
  | [(_level, []), ...update_rest] =>
    /* This level is empty, so remove it and move on */
    pop_update(update_rest)
  | [] =>
    /* Nothing needs an update */
    None
  };

let rec insert_update = (~level, n, needs_update: needs_update_list(_)) =>
  switch (needs_update) {
  | [(hlevel, _), ..._] as update_rest when hlevel > level =>
    /* This index is a higher level than what we're inserting, so make a new level here */
    [(level, [n]), ...update_rest]
  | [(hlevel, hrest), ...update_rest] when hlevel == level =>
    /* We've found our level, so insert here */
    [(hlevel, [n, ...hrest]), ...update_rest]
  | [(_hlevel, _) as h, ...update_rest] /*when hlevel < level*/ =>
    /* This index is a lower level than what we're inserting, so keep searching */
    /* not tail-recursive, but these lists should be O(100) elements so should be fine*/
    [h, ...insert_update(~level, n, update_rest)]
  | [] =>
    /* We've reached the end of the list, so just create our level */
    [(level, [n])]
  };

let rec flood_gen = (~init, ~live: needs_update_list(_), ~spread) =>
  switch (pop_update(live)) {
  | Some((level, coord, needs_update)) =>
    let (acc, updated_coords) = spread(init, ~level, coord);
    let next_live =
      List.fold(updated_coords, ~init=needs_update, ~f=(ls, (level, coord)) =>
        insert_update(~level, coord, ls)
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