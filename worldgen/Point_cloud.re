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
  side: int,
};

let is_within = (side, x, y) =>
  Float.(0. <= x && x < of_int(side) && 0. <= y && y < of_int(side));

let assert_within = (side, x, y) =>
  if (!is_within(side, x, y)) {
    let msg =
      Printf.sprintf("point is outside boundaries of cloud: %f, %f", x, y);
    raise(Invalid_argument(msg));
  };

let init_f = (~avoid_edges=false, ~side, ~spacing=1, f) => {
  if (avoid_edges && side % spacing != 0) {
    invalid_argf(
      "when using avoid_edges, side (%d) must divide evenly by spacing (%d)",
      side,
      spacing,
      (),
    );
  };
  let points_per_side = (side - 1) / spacing + 1;
  let points = ref(Sparse_grid.make(points_per_side));
  let imin = avoid_edges ? 1 : 0;
  let imax = avoid_edges ? points_per_side - 2 : points_per_side - 1;
  let spacing_f = Float.of_int(spacing);
  for (yi in imin to imax) {
    for (xi in imin to imax) {
      let x = xi * spacing;
      let y = yi * spacing;
      let xf = Float.of_int(x) +. Random.float(spacing_f);
      let yf = Float.of_int(y) +. Random.float(spacing_f);
      /* Discard if the point is outside */
      if (is_within(side, xf, yf)) {
        let point = {px: xf, py: yf, value: f(~xf, ~yf, ~xi=x, ~yi=y)};
        points := Sparse_grid.put(points^, xi, yi, point);
      };
    };
  };
  {points: points^, spacing, side};
};
/**
  init creates a point cloud, randomizes the points, and initializes their
  value by calling `f(x, y)`.

  spacing determines how many points will fill the space. If side=10 and
  spacing=2, then there will be 5 points horizontally. If side=10 and
  spacing=3, then there will be 4 points horizontally--the division is
  rounded up to ensure the space gets filled completely.

  avoid_edges causes no points to be generated along the edges, ensuring that
  the nearest_with_edge would return a natural-looking edge along the edges
  of the point cloud. It defaults to false.

  For example, with side=12 and spacing=4:

  - If avoid_edges=true, points are generated at 2 + 4n ± 2, for n in {0, 1, 2}
  - If avoid_edges=false, points are generated at 2 + 4n ± 2, for n in {-1, 0, 1, 2, 3}
  - Any points that fall outside 0 <= p < 12 are discarded, regardless of
    avoid_edges setting.

  . 0 1 2 3 4 5 6 7 8 9 0 1
  0 - - - - - - - - - - - -
  1 - - - - - - - - - - - -
  2 - - o - - - o - - - o -
  3 - - - - - - - - - - - -
  4 - - - - ^ ^ ^ ^ ^ - - -
  5 - - - - < - - - > - - -
  6 - - o - < - o - > - o -
  7 - - - - < - - - > - - -
  8 - - - - v v v v v - - -
  9 - - - - - - - - - - - -
  0 - - o - - - o - - - o -
  1 - - - - - - - - - - - -
 */
let init = (~avoid_edges=?, ~side, ~spacing=?, f) =>
  init_f(~avoid_edges?, ~side, ~spacing?, (~xf as _, ~yf as _, ~xi, ~yi) =>
    f(xi, yi)
  );

let make_list = (~avoid_edges=?, ~side, ~spacing=?, ()) => {
  let cloud = init(~avoid_edges?, ~side, ~spacing?, (_, _) => ());
  Sparse_grid.fold(
    cloud.points,
    (_, {px, py, value: _}, ls) => [(px, py), ...ls],
    [],
  );
  /* |> List.rev; */
};

let make_int_list = (~avoid_edges=?, ~side, ~spacing=?, ()) => {
  let cloud = init(~avoid_edges?, ~side, ~spacing?, (_, _) => ());
  Sparse_grid.fold(
    cloud.points,
    (_, {px, py, value: _}, ls) => [Mg_util.Floats.(~~px, ~~py), ...ls],
    [],
  );
  /* |> List.rev; */
};

let distance2 = (ax, ay, bx, by) => Float.((ax - bx) ** 2. + (ay - by) ** 2.);

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
  | [first, ...rest] =>
    let first_distance = distance2(first.px, first.py, x, y);
    let (closest, closest_dist2) =
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
    (closest, sqrt(closest_dist2));
  | [] =>
    let next_radius = radius * 2;
    if (next_radius <= cloud.side) {
      closest_point(~radius=next_radius, cloud, x, y);
    } else {
      failwithf("could not find closest point for (%f, %f)", x, y, ());
    };
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
    let (a, a_dist2, b, b_dist2) =
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
    (a, sqrt(a_dist2), b, sqrt(b_dist2));
  | _ =>
    let next_radius = radius * 2;
    if (next_radius <= cloud.side) {
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
  let side_f = of_int(cloud.side);
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

let%expect_test "points" = {
  Random.init(283);
  let side = 8;
  let rec print_grid_of_points = (~x, ~y, p) =>
    if (y >= side) {
      ();
    } else if (x >= side) {
      print_endline("");
      print_grid_of_points(~x=0, ~y=y + 1, p);
    } else {
      if (List.exists(p, ~f=((here_x, here_y)) => here_x == x && here_y == y)) {
        Printf.printf("%02d", 10 * y + x);
      } else {
        print_string("..");
      };
      print_string(" ");
      print_grid_of_points(~x=x + 1, ~y, p);
    };
  let p = make_int_list(~side=8, ~spacing=2, ());
  print_grid_of_points(~x=0, ~y=0, p);

  %expect
  {|
    00 .. 02 .. .. .. .. 07
    .. .. .. .. .. 15 .. ..
    20 .. .. .. 24 .. .. ..
    .. .. .. 33 .. .. 36 ..
    .. .. .. .. 44 .. .. ..
    50 .. .. 53 .. .. .. 57
    .. .. .. .. 64 .. .. ..
    70 .. 72 .. .. .. .. 77 |};
};

let%expect_test "nearest_with_edge without avoid_edges" = {
  Random.init(283);
  let rec print_grid_of_nearest = (~x, ~y, p) =>
    if (y >= p.side) {
      ();
    } else if (x >= p.side) {
      print_endline("");
      print_grid_of_nearest(~x=0, ~y=y + 1, p);
    } else {
      let here =
        nearest_with_edge(p, "##", Float.of_int(x), Float.of_int(y));
      print_string(here);
      print_string(" ");
      print_grid_of_nearest(~x=x + 1, ~y, p);
    };
  let p =
    init(~side=8, ~spacing=2, (x, y) => Printf.sprintf("%02d", 10 * y + x));
  print_grid_of_nearest(~x=0, ~y=0, p);

  %expect
  {|
    ## ## ## ## ## ## ## ##
    ## ## 02 ## ## 04 04 06
    ## 20 20 24 24 04 04 ##
    ## 20 22 22 24 24 26 26
    ## ## 22 22 44 44 26 26
    ## ## 42 42 42 44 44 46
    ## ## 42 42 64 64 46 46
    ## 60 ## ## 64 ## ## 66 |};
};

let%expect_test "nearest_with_edge with avoid_edges" = {
  Random.init(283);
  let rec print_grid_of_nearest = (~x, ~y, p) =>
    if (y >= p.side) {
      ();
    } else if (x >= p.side) {
      print_endline("");
      print_grid_of_nearest(~x=0, ~y=y + 1, p);
    } else {
      let here =
        nearest_with_edge(p, "##", Float.of_int(x), Float.of_int(y));
      print_string(here);
      print_string(" ");
      print_grid_of_nearest(~x=x + 1, ~y, p);
    };
  let p =
    init(~avoid_edges=true, ~side=8, ~spacing=2, (x, y) =>
      Printf.sprintf("%02d", 10 * y + x)
    );
  print_grid_of_nearest(~x=0, ~y=0, p);

  %expect
  {|
    ## ## ## ## ## ## ## ##
    ## ## ## ## ## ## ## ##
    ## ## 22 22 24 24 24 ##
    ## ## 22 22 24 24 44 ##
    ## ## ## 42 42 44 44 ##
    ## ## 42 42 42 44 44 ##
    ## ## 42 42 42 44 44 ##
    ## ## ## ## ## ## ## ## |};
};