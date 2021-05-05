open! Core_kernel;

[@deriving bin_io]
type block_no_elevation = {
  min_x: int,
  max_x: int,
  min_z: int,
  max_z: int,
};

[@deriving bin_io]
type block = {
  xz: block_no_elevation,
  elevation: int,
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
  roads: list(Road_pathing_rules.t),
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
