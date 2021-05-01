open! Core_kernel

type flower = {block: Minecraft.Block.material; percentage: int}
[@@deriving eq, bin_io]

type cactus = {percentage: int} [@@deriving eq, bin_io]

type mid_biome =
  | Plain of flower
  | Forest of flower
  | Desert of cactus
  | Savanna
[@@deriving eq, bin_io]

type shore_biome = Sand | Gravel | Clay [@@deriving eq, bin_io]

type high_biome = Pine_forest | Barren | Snow [@@deriving eq, bin_io]

type biome = Mid of mid_biome | Shore of shore_biome | High of high_biome
[@@deriving eq, bin_io]

type t' = biome Point_cloud.t [@@deriving bin_io]

type t = t' * Overlay.Canon.delta [@@deriving bin_io]

module Pq = Priority_queue.Int

let overlay = Overlay.make_overlay "biome" bin_reader_t bin_writer_t
