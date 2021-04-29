open! Core_kernel

type t =
  | Plains
  | Desert
  | Mountains
  | Forest
  | River
  | Snowy_tundra
  | Beach
  | Stone_shore
  | Wooded_mountains
  | Savanna
[@@deriving eq, ord, bin_io]

let id = function
  | Plains ->
      1l
  | Desert ->
      2l
  | Mountains ->
      3l
  | Forest ->
      4l
  | River ->
      7l
  | Snowy_tundra ->
      12l
  | Beach ->
      16l
  | Stone_shore ->
      25l
  | Wooded_mountains ->
      34l
  | Savanna ->
      35l
