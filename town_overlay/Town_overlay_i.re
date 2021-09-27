open! Core_kernel;

[@deriving (bin_io, sexp)]
type block_no_elevation = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
};

[@deriving (bin_io, sexp)]
type block = {
  xz: block_no_elevation,
  elevation: int,
};

[@deriving (bin_io, sexp)]
type worksite =
  | Butcher
  | Fisherman
  | Shepherd;

[@deriving (bin_io, sexp)]
type marks = [ | `Worksite | `Road | `Villager];

[@deriving (bin_io, sexp)]
type building = {template: [@sexp.opaque] Minecraft_template.t(marks)};

[@deriving (bin_io, sexp)]
type fitted_building = {
  building,
  block: block_no_elevation,
};

[@deriving (bin_io, sexp)]
type house = {
  building: fitted_building,
  worksite: option(worksite),
};

[@deriving bin_io]
type output = {
  bell: block,
  farms: list(block),
  houses: list(house),
  roads: list(Roads.Rules.t),
  fences: list((int, int)),
  obstacles: Sparse_grid.t(unit),
};

[@deriving bin_io]
type town = {
  x: int,
  z: int,
  town: output,
};

[@deriving bin_io]
type t' = list(town);

[@deriving bin_io]
type t = (t', Overlay.Canon.delta);
