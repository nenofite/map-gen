open! Core;

module Int3 = {
  [@deriving (eq, ord, hash, sexp)]
  type t = (int, int, int);
};
module Hs_I3 = Hash_set.Make(Int3);

// Similar to a Hash_queue, but only lets each item enter once during the entire
// lifetime of the queue. Also is strictly FILO instead of double ended
type t = {
  mutable items: list(Int3.t),
  entered: Hs_I3.t,
};

let create = () => {items: [], entered: Hs_I3.create()};

let add = (t, coord) =>
  if (!Hash_set.mem(t.entered, coord)) {
    Hash_set.add(t.entered, coord);
    t.items = [coord, ...t.items];
  };

let pop = t => {
  switch (t.items) {
  | [n, ...rest] =>
    t.items = rest;
    Some(n);
  | [] => None
  };
};

let copy = src => {items: src.items, entered: Hash_set.copy(src.entered)};

let blit = (src, dest) => {
  Hash_set.clear(dest.entered);
  Hash_set.iter(src.entered, ~f=coord => Hash_set.add(dest.entered, coord));
  dest.items = src.items;
};