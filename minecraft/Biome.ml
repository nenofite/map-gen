open! Core_kernel

type t = Plains | Forest | Snowy_tundra [@@deriving eq, ord, bin_io]

let id = function Plains -> 1l | Forest -> 4l | Snowy_tundra -> 12l
