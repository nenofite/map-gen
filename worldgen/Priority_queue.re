type priority = float;

/**
  Shamelessly ripped from the OCaml manual, converted to Reason, and did some
  minor clean-up and removed the exception
 */
type t('a) =
  | Empty
  | Node(priority, 'a, t('a), t('a));

let empty = Empty;

let rec insert = (queue, prio, elt) =>
  switch (queue) {
  | Empty => Node(prio, elt, Empty, Empty)
  | Node(p, e, left, right) =>
    if (prio <= p) {
      Node(prio, elt, insert(right, p, e), left);
    } else {
      Node(p, e, insert(right, prio, elt), left);
    }
  };

let rec remove_top =
  fun
  | Empty => Empty
  | Node(_prio, _elt, left, Empty) => left
  | Node(_prio, _elt, Empty, right) => right
  | Node(
      _prio,
      _elt,
      Node(lprio, lelt, _, _) as left,
      Node(rprio, relt, _, _) as right,
    ) =>
    if (lprio <= rprio) {
      Node(lprio, lelt, remove_top(left), right);
    } else {
      Node(rprio, relt, left, remove_top(right));
    };

let extract =
  fun
  | Empty => (None, Empty)
  | Node(_prio, elt, _, _) as queue => (Some(elt), remove_top(queue));