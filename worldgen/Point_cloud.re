open Core_kernel;

/*
 Conventions:

 - px, py aka. xf, yf -- float coordinates, relative to the caller's coordinate system, that have wiggling applied
 - cx, cy aka. xi, yi -- int coordinates, relative to the underlying Sparse_grid

 px_of_cx = cx * spacing + wiggle
 cx_of_px = Int.of_float (px / spacing + 0.5)
 */

[@deriving bin_io]
type point('v) = {
  px: float,
  py: float,
  value: 'v,
};

[@deriving bin_io]
type t('v) = {
  points: Sparse_grid.t(point('v)),
  spacing: int,
};

let is_within = (width, height, x, y) =>
  Float.(0. <= x && x < of_int(width) && 0. <= y && y < of_int(height));

let assert_within = (width, height, x, y) =>
  if (!is_within(width, height, x, y)) {
    let msg =
      Printf.sprintf("point is outside boundaries of cloud: %f, %f", x, y);
    raise(Invalid_argument(msg));
  };

let init_f = (~width, ~height, ~spacing=1, f) => {
  if (width != height) {
    invalid_argf("Point_cloud isn't square: %d %d", width, height, ());
  };
  let side = width;
  let points = ref(Sparse_grid.make(side / spacing + 1));
  for (yi in 0 to height / spacing) {
    for (xi in 0 to width / spacing) {
      let x = xi * spacing;
      let y = yi * spacing;
      let xf =
        float_of_int(x)
        +. (Random.float(1.) -. 0.5)
        *. float_of_int(spacing);
      let yf =
        float_of_int(y)
        +. (Random.float(1.) -. 0.5)
        *. float_of_int(spacing);
      /* Discard if the point is outside */
      if (is_within(width, height, xf, yf)) {
        let point = {px: xf, py: yf, value: f(~xf, ~yf, ~xi=x, ~yi=y)};
        points := Sparse_grid.put(points^, xi, yi, point);
      };
    };
  };
  {points: points^, spacing};
};
/**
  init creates a point cloud, randomizes the points, and initializes their
  value by calling `f(x, y)`.

  spacing determines how many points will fill the space. If width=10 and
  spacing=2, then there will be 5 points horizontally.
 */
let init = (~width, ~height, ~spacing=?, f) =>
  init_f(~width, ~height, ~spacing?, (~xf as _, ~yf as _, ~xi, ~yi) =>
    f(xi, yi)
  );

let make_list = (~width, ~height, ~spacing=?, ()) => {
  let cloud = init(~width, ~height, ~spacing?, (_, _) => ());
  Sparse_grid.fold(
    cloud.points,
    (_, {px, py, value: _}, ls) => [(px, py), ...ls],
    [],
  );
  /* |> List.rev; */
};

let make_int_list = (~width, ~height, ~spacing=?, ()) => {
  let cloud = init(~width, ~height, ~spacing?, (_, _) => ());
  Sparse_grid.fold(
    cloud.points,
    (_, {px, py, value: _}, ls) => [Mg_util.Floats.(~~px, ~~py), ...ls],
    [],
  );
  /* |> List.rev; */
};

let distance2 = (ax, ay, bx, by) => Float.((ax - bx) ** 2. + (ay - by) ** 2.);
let distance_actual = (ax, ay, bx, by) => sqrt(distance2(ax, ay, bx, by));

let rec closest_point = (~radius=1, cloud, x, y) => {
  open Core_kernel;
  /* Convert to cloud x, y */
  let cx = Float.(x / of_int(cloud.spacing) + 0.5 |> to_int);
  let cy = Float.(y / of_int(cloud.spacing) + 0.5 |> to_int);
  /* Compare among the point at cx, cy and its eight neighbors */
  let neighbors =
    Mg_util.Range.(
      fold(cy - radius, cy + radius, [], (ls, iter_cy) =>
        fold(cx - radius, cx + radius, ls, (ls, iter_cx) =>
          switch (Sparse_grid.at(cloud.points, iter_cx, iter_cy)) {
          | Some(n) => [n, ...ls]
          | None => ls
          }
        )
      )
    );
  switch (neighbors) {
  | [] =>
    let next_radius = radius * 2;
    if (next_radius <= cloud.points.side * cloud.spacing) {
      closest_point(~radius=next_radius, cloud, x, y);
    } else {
      failwithf("could not find closest point for (%f, %f)", x, y, ());
    };
  | [first, ...rest] =>
    let first_distance = distance2(first.px, first.py, x, y);
    List.fold(
      rest,
      ~init=(first, first_distance),
      ~f=((_, curr_dist) as curr, here) => {
        let here_dist = distance2(here.px, here.py, x, y);
        if (Float.(here_dist < curr_dist)) {
          (here, here_dist);
        } else {
          curr;
        };
      },
    );
  };
};

let rec two_closest_points = (~radius=1, cloud, x, y) => {
  /* Convert to cloud x, y */
  let cx = Float.(x / of_int(cloud.spacing) + 0.5 |> to_int);
  let cy = Float.(y / of_int(cloud.spacing) + 0.5 |> to_int);
  /* Compare among the point at cx, cy and its eight neighbors */
  let neighbors =
    Mg_util.Range.(
      fold(cy - radius, cy + radius, [], (ls, iter_cy) =>
        fold(cx - radius, cx + radius, ls, (ls, iter_cx) =>
          switch (Sparse_grid.at(cloud.points, iter_cx, iter_cy)) {
          | Some(n) => [n, ...ls]
          | None => ls
          }
        )
      )
    );
  switch (neighbors) {
  | [first, second, ...rest] =>
    let first_distance = distance2(first.px, first.py, x, y);
    let second_distance = distance2(second.px, second.py, x, y);
    List.fold(
      rest,
      ~init=(first, first_distance, second, second_distance),
      ~f=((a, a_dist, _b, b_dist) as curr, here) => {
        let here_dist = distance2(here.px, here.py, x, y);
        switch (Float.(here_dist < a_dist, here_dist < b_dist)) {
        | (true, _) => (here, here_dist, a, a_dist)
        | (false, true) => (a, a_dist, here, here_dist)
        | (false, false) => curr
        };
      },
    );
  | _ =>
    let next_radius = radius * 2;
    if (next_radius <= cloud.points.side * cloud.spacing) {
      two_closest_points(~radius=next_radius, cloud, x, y);
    } else {
      failwithf("could not find closest point for (%f, %f)", x, y, ());
    };
  };
};

/**
  nearest_with_edge returns the value of the nearest point, or edge_value if
  the edge is closer than any point.
 */
let nearest_with_edge = (cloud, edge_value, x, y) => {
  open Float;
  let side_f = float_of_int(cloud.points.side);
  let edge_distance = min(min(x, side_f - x), min(y, side_f - y));
  let (closest_point, closest_point_distance) = closest_point(cloud, x, y);
  if (closest_point_distance <= edge_distance) {
    closest_point.value;
  } else {
    edge_value;
  };
};

/**
  nearest returns the value of the nearest point.
 */
let nearest = (cloud, x, y) => {
  let (closest_point, _closest_point_distance) = closest_point(cloud, x, y);
  closest_point.value;
};

/**
  interpolate finds the two closest points and returns an interpolation based
  on the relative distance to each.
 */
let interpolate = (cloud, x, y) => {
  let (a, a_distance, b, b_distance) = two_closest_points(cloud, x, y);
  let frac = a_distance /. (a_distance +. b_distance);
  a.value *. (1. -. frac) +. b.value *. frac;
};