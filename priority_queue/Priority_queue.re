open Core;

module Lite = Lite;

module type Priority = {
  type t;
  let (<=): (t, t) => bool;
};

/**
  Shamelessly ripped from the OCaml manual, converted to Reason, and did some
  minor clean-up and removed the exception
 */
module Make = (P: Priority) => {
  type t('a) =
    | Empty
    | Node(P.t, 'a, t('a), t('a));

  let empty = Empty;

  let rec insert = (queue, prio, elt) =>
    switch (queue) {
    | Empty => Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) =>
      if (P.(prio <= p)) {
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
      if (P.(lprio <= rprio)) {
        Node(lprio, lelt, remove_top(left), right);
      } else {
        Node(rprio, relt, left, remove_top(right));
      };

  let extract =
    fun
    | Empty => (None, Empty)
    | Node(_prio, elt, _, _) as queue => (Some(elt), remove_top(queue));

  let extract_with_priority =
    fun
    | Empty => (None, Empty)
    | Node(prio, elt, _, _) as queue => (
        Some((prio, elt)),
        remove_top(queue),
      );
};

module Float = Make(Float);
module Int = Make(Int);
module Int_desc =
  Make({
    type t = int;
    let (<=) = (>=);
  });
include Float;
