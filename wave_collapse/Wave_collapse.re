open! Core_kernel;

type wave('a) = {
  tileset: Tileset.tileset('a),
  /* x y z kind */
  possibilities: array(array(array(array(bool)))),
};

type wave_evaluator('a) = {
  mutable wave: wave('a),
  mutable entropy_queue: Priority_queue.t((int, int, int)),
};