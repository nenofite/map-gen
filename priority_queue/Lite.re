open! Core;

type t('a) = list((int, list('a)));

let rec pop = (queue: t('a)): option((int, 'a, t('a))) =>
  switch (queue) {
  | [(level, [coord, ...level_rest]), ...update_rest] =>
    /* This level has some available, so pop one off */
    let needs_update = [(level, level_rest), ...update_rest];
    Some((level, coord, needs_update));
  | [(_level, []), ...update_rest] =>
    /* This level is empty, so remove it and move on */
    pop(update_rest)
  | [] =>
    /* Nothing needs an update */
    None
  };

let rec insert = (~level, n, queue: t(_)) =>
  switch (queue) {
  | [(hlevel, _), ..._] as update_rest when hlevel > level =>
    /* This index is a higher level than what we're inserting, so make a new level here */
    [(level, [n]), ...update_rest]
  | [(hlevel, hrest), ...update_rest] when hlevel == level =>
    /* We've found our level, so insert here */
    [(hlevel, [n, ...hrest]), ...update_rest]
  | [(_hlevel, _) as h, ...update_rest] /*when hlevel < level*/ =>
    /* This index is a lower level than what we're inserting, so keep searching */
    /* not tail-recursive, but these lists should be O(100) elements so should be fine*/
    [h, ...insert(~level, n, update_rest)]
  | [] =>
    /* We've reached the end of the list, so just create our level */
    [(level, [n])]
  };
