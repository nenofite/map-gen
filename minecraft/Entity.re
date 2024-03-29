open! Core;

/**
  entity is a Minecraft entity[0] such as a villager

  [0]: https://minecraft.gamepedia.com/Chunk_format#Entity_format
 */
[@deriving (bin_io, sexp)]
type t = {
  id: string,
  x: float,
  y: float,
  z: float,
};