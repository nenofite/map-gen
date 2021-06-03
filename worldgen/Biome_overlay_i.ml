open! Core_kernel

type flower = {block: Minecraft.Block.material; percentage: int}
[@@deriving eq, bin_io]

type cactus = {percentage: int} [@@deriving eq, bin_io]

type biome =
  | Ocean (* TODO specific ocean biomes *)
  | Plain of flower
  | Forest of flower
  | Desert of cactus
  | Savanna
  | Shore
  | Stone_shore
  | River
  | Pine_forest
  | Barren_mountain
  | Snow_mountain
[@@deriving eq, bin_io]

type t' =
  { precipitation: int Point_cloud.t
  ; categories: (biome Point_cloud.t, biome) Either.t array }
[@@deriving bin_io]

type t = t' * Overlay.Canon.delta [@@deriving bin_io]

module Pq = Priority_queue.Int_desc

let overlay = Overlay.make_overlay "biome" bin_reader_t bin_writer_t
