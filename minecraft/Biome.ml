open! Core

type t =
  | Ocean
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
  | Snowy_plains
  | Snowy_taiga
[@@deriving eq, ord, bin_io]

let id = function
  | Ocean ->
      0l
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
  | Snowy_plains ->
      3l
  | Snowy_taiga ->
      15l
