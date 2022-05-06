open! Core_kernel;

type wave('a) = {
  /* TODO put this to use */
  tileset: Tileset.tileset('a),
  xs: int,
  ys: int,
  zs: int,
  /* x y z kind */
  possibilities: array(array(array(array(bool)))),
};
