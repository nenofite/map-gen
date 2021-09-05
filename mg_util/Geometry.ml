open Core_kernel

module Vec2i = struct
  module T = struct
    type t = int * int [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let ( + ) (x0, y0) (x1, y1) = Int.(x0 + x1, y0 + y1)

  let ( - ) (x0, y0) (x1, y1) = Int.(x0 - x1, y0 - y1)

  let ( * ) (x0, y0) (x1, y1) = Int.(x0 * x1, y0 * y1)

  let ( / ) (x0, y0) (x1, y1) = Int.(x0 / x1, y0 / y1)

  let ( +. ) (x, y) s = Int.(x + s, y + s)

  let ( -. ) (x, y) s = Int.(x - s, y - s)

  let ( *. ) (x, y) s = Int.(x * s, y * s)

  let ( /. ) (x, y) s = Int.(x / s, y / s)
end

module Vec3f = struct
  module T = struct
    type t = float * float * float [@@deriving eq, ord, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)

  let ( + ) (x0, y0, z0) (x1, y1, z1) = Float.(x0 + x1, y0 + y1, z0 + z1)

  let ( - ) (x0, y0, z0) (x1, y1, z1) = Float.(x0 - x1, y0 - y1, z0 - z1)

  let ( * ) (x0, y0, z0) (x1, y1, z1) = Float.(x0 * x1, y0 * y1, z0 * z1)

  let ( / ) (x0, y0, z0) (x1, y1, z1) = Float.(x0 / x1, y0 / y1, z0 / z1)

  let ( +. ) (x, y, z) s = Float.(x + s, y + s, z + s)

  let ( -. ) (x, y, z) s = Float.(x - s, y - s, z - s)

  let ( *. ) (x, y, z) s = Float.(x * s, y * s, z * s)

  let ( /. ) (x, y, z) s = Float.(x / s, y / s, z / s)

  let of_int = Tuple3.map ~f:Float.of_int
end

module Vec3i = struct
  module T = struct
    type t = int * int * int [@@deriving eq, ord, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let ( + ) (x0, y0, z0) (x1, y1, z1) = Int.(x0 + x1, y0 + y1, z0 + z1)

  let ( - ) (x0, y0, z0) (x1, y1, z1) = Int.(x0 - x1, y0 - y1, z0 - z1)

  let ( * ) (x0, y0, z0) (x1, y1, z1) = Int.(x0 * x1, y0 * y1, z0 * z1)

  let ( / ) (x0, y0, z0) (x1, y1, z1) = Int.(x0 / x1, y0 / y1, z0 / z1)

  let ( +. ) (x, y, z) s = Int.(x + s, y + s, z + s)

  let ( -. ) (x, y, z) s = Int.(x - s, y - s, z - s)

  let ( *. ) (x, y, z) s = Int.(x * s, y * s, z * s)

  let ( /. ) (x, y, z) s = Int.(x / s, y / s, z / s)

  let of_float = Tuple3.map ~f:Int.of_float
end

type vec3f = Vec3f.t

type vec3i = Vec3i.t

let points_on_line start finish =
  let module V = Vec3i in
  (* instead of reversing the list, just flip start and finish *)
  let start, finish = (finish, start) in
  let x0, y0, z0 = start in
  let x1, y1, z1 = finish in
  let dx, dy, dz = V.(finish - start |> Tuple3.map ~f:abs) in
  let dx2, dy2, dz2 = (2 * dx, 2 * dy, 2 * dz) in
  let sx = if x1 > x0 then 1 else -1 in
  let sy = if y1 > y0 then 1 else -1 in
  let sz = if z1 > z0 then 1 else -1 in
  let rec x_driving p1 p2 x y z points =
    if x = x1 then points
    else
      let x = x + sx in
      let p1, y = if p1 >= 0 then (p1 - dx2, y + sy) else (p1, y) in
      let p2, z = if p2 >= 0 then (p2 - dx2, z + sz) else (p2, z) in
      let p1 = p1 + dy2 in
      let p2 = p2 + dz2 in
      x_driving p1 p2 x y z ((x, y, z) :: points)
  in
  let rec y_driving p1 p2 x y z points =
    if y = y1 then points
    else
      let y = y + sy in
      let p1, x = if p1 >= 0 then (p1 - dy2, x + sx) else (p1, x) in
      let p2, z = if p2 >= 0 then (p2 - dy2, z + sz) else (p2, z) in
      let p1 = p1 + dx2 in
      let p2 = p2 + dz2 in
      y_driving p1 p2 x y z ((x, y, z) :: points)
  in
  let rec z_driving p1 p2 x y z points =
    if z = z1 then points
    else
      let z = z + sz in
      let p1, x = if p1 >= 0 then (p1 - dz2, x + sx) else (p1, x) in
      let p2, y = if p2 >= 0 then (p2 - dz2, y + sy) else (p2, y) in
      let p1 = p1 + dx2 in
      let p2 = p2 + dy2 in
      z_driving p1 p2 x y z ((x, y, z) :: points)
  in
  if dx >= dy && dx >= dz then x_driving (dy2 - dx) (dz2 - dx) x0 y0 z0 [start]
  else if dy >= dx && dy >= dz then
    y_driving (dx2 - dy) (dz2 - dy) x0 y0 z0 [start]
  else z_driving (dx2 - dz) (dy2 - dz) x0 y0 z0 [start]
