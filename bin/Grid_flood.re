type needs_update_list('a) = list((int, list('a)));

let rec pop_update =
        (needs_update: needs_update_list('a))
        : option(('a, needs_update_list('a))) =>
  switch (needs_update) {
  | [(level, [coord, ...level_rest]), ...update_rest] =>
    /* This level has some available, so pop one off */
    let needs_update = [(level, level_rest), ...update_rest];
    Some((coord, needs_update));
  | [(_level, []), ...update_rest] =>
    /* This level is empty, so remove it and move on */
    pop_update(update_rest)
  | [] =>
    /* Nothing needs an update */
    None
  };

let rec insert_update = (level, n, needs_update: needs_update_list(_)) =>
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
    [h, ...insert_update(level, n, update_rest)]
  | [] =>
    /* We've reached the end of the list, so just create our level */
    [(level, [n])]
  };

let rec flood =
        (
          grid: Grid.t('a),
          ~initial as needs_update: needs_update_list((int, int)),
          ~spread,
        ) =>
  switch (pop_update(needs_update)) {
  | Some(((x, y), needs_update)) =>
    let updated_coords = spread(grid, x, y);
    let needs_update =
      List.fold_left(
        (ls, (level, x, y)) => insert_update(level, (x, y), ls),
        needs_update,
        updated_coords,
      );
    flood(grid, ~initial=needs_update, ~spread);
  | None => ()
  };