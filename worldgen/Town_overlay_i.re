open! Core_kernel;

type input = {
  elevation: Grid.t(int),
  roads: Sparse_grid.t(unit),
};

[@deriving bin_io]
type block = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
  elevation: int,
};

[@deriving bin_io]
type block_no_elevation = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
};

[@deriving bin_io]
type worksite =
  | Butcher
  | Fisherman
  | Shepherd;

[@deriving bin_io]
type house = {
  block,
  worksite: option(worksite),
};

[@deriving bin_io]
type output = {
  bell: block,
  farms: list(block),
  houses: list(house),
};

[@deriving bin_io]
type town = {
  x: int,
  z: int,
  town: output,
};

[@deriving bin_io]
type t = (list(town), Overlay.Canon.delta);

type x = list(town);
