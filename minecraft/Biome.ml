open! Core_kernel

type t = Plains | Forest [@@deriving eq, ord, bin_io]

let id = function Plains -> 1l | Forest -> 4l
