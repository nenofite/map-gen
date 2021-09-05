open! Core_kernel;

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
  offset: int,
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

let iter = (cloud, ~f) => {
  Sparse_grid.iter(cloud.points, (_, point) => {
    f(~x=point.px, ~y=point.py, point.value)
  });
};

let map = (cloud, ~f) => {
  let points =
    Sparse_grid.map(cloud.points, (_, p) => {...p, value: f(p.value)});
  {...cloud, points};
};

let init_f = (~cover_edges=true, ~side, ~spacing=1, ~edge_f=?, f) => {
  let points_per_side =
    if (side % spacing != 0 && cover_edges) {
      side / spacing + 1;
    } else {
      side / spacing;
    };
  let offset = (side - points_per_side * spacing) / 2;
  let points = ref(Sparse_grid.make(points_per_side));
  let imin = 0;
  let imax = points_per_side - 1;
  let spacing_f = Float.of_int(spacing);
  for (yi in imin to imax) {
    for (xi in imin to imax) {
      let xf =
        Float.of_int(xi * spacing + offset) +. Random.float(spacing_f);
      let yf =
        Float.of_int(yi * spacing + offset) +. Random.float(spacing_f);
      let x = Int.of_float(xf);
      let y = Int.of_float(yf);
      /* Discard if the point is outside */
      if (is_within(side, xf, yf)) {
        let is_edge = xi == imin || xi == imax || yi == imin || yi == imax;
        let f =
          switch (edge_f) {
          | Some(ef) when is_edge => ef
          | Some(_)
          | None => f
          };
        let point = {px: xf, py: yf, value: f(~xf, ~yf, ~xi=x, ~yi=y)};
        points := Sparse_grid.put(points^, xi, yi, point);
      };
    };
  };
  {points: points^, spacing, offset, side};
};
let init = (~cover_edges=?, ~side, ~spacing=?, ~edge_f=?, f) => {
  let simplify_f = (f, ~xf as _, ~yf as _, ~xi, ~yi) => f(xi, yi);
  let edge_f = Option.map(edge_f, ~f=simplify_f);
  let f = simplify_f(f);
  init_f(~cover_edges?, ~side, ~spacing?, ~edge_f?, f);
};

let of_griddable = (~grid_side, ~grid_get, ~spacing) => {
  let side = grid_side * spacing;
  init(~side, ~spacing, (x, z) => grid_get(~x=x / spacing, ~z=z / spacing));
};

let of_grid_mut = (grid, ~spacing) =>
  of_griddable(
    ~grid_side=Grid.Mut.side(grid),
    ~grid_get=Grid.Mut.get(grid),
    ~spacing,
  );

let side = t => t.side;

let make_list = (~cover_edges=?, ~side, ~spacing=?, ()) => {
  let cloud = init(~cover_edges?, ~side, ~spacing?, (_, _) => ());
  Sparse_grid.fold(
    cloud.points,
    (_, {px, py, value: _}, ls) => [(px, py), ...ls],
    [],
  );
};

let make_int_list = (~cover_edges=?, ~side, ~spacing=?, ()) => {
  let cloud = init(~cover_edges?, ~side, ~spacing?, (_, _) => ());
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
  let cx = (Int.of_float(x) - cloud.offset) / cloud.spacing;
  let cy = (Int.of_float(y) - cloud.offset) / cloud.spacing;
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

let rec n_closest_points = (~radius=1, ~n, cloud, x, y) => {
  /* Convert to cloud x, y */
  let cx = (Int.of_float(x) - cloud.offset) / cloud.spacing;
  let cy = (Int.of_float(y) - cloud.offset) / cloud.spacing;
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
  if (List.length(neighbors) >= n) {
    List.map(neighbors, ~f=point =>
      (distance2(point.px, point.py, x, y), point)
    )
    |> List.sort(~compare=((ad, _), (bd, _)) => Float.compare(ad, bd))
    |> List.take(_, n)
    |> List.map(~f=((d, p)) => (sqrt(d), p));
  } else {
    let next_radius = radius * 2;
    if (next_radius <= cloud.side) {
      n_closest_points(~radius=next_radius, ~n, cloud, x, y);
    } else {
      failwithf("could not find closest point for (%f, %f)", x, y, ());
    };
  };
};

let rec two_closest_points = (~radius=1, cloud, x, y) => {
  /* Convert to cloud x, y */
  let cx = (Int.of_float(x) - cloud.offset) / cloud.spacing;
  let cy = (Int.of_float(y) - cloud.offset) / cloud.spacing;
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

let nearest_int = (cloud, x, y) => nearest(cloud, float(x), float(y));

/**
  interpolate finds the two closest points and returns an interpolation based
  on the relative distance to each.
 */
let interpolate = (cloud, x, y) => {
  let (a, a_distance, b, b_distance) = two_closest_points(cloud, x, y);
  let frac = a_distance /. (a_distance +. b_distance);
  a.value *. (1. -. frac) +. b.value *. frac;
};

let interpolate4 = (cloud, x, y) => {
  let ns = n_closest_points(~n=4, cloud, x, y);
  let dist_total = List.sum((module Float), ns, ~f=((dist, _)) => dist);
  let inv_dist_total =
    List.sum((module Float), ns, ~f=((dist, _)) =>
      Float.(1. - dist / dist_total)
    );
  List.sum((module Float), ns, ~f=((dist, point)) => {
    Float.(of_int(point.value) * (1. - dist / dist_total) / inv_dist_total)
  })
  |> Int.of_float;
};

/**
  creates a new point cloud of the same dimensions but with different spacing. Each point in the new cloud samples from the nearest in the old cloud
  */
let subdivide = (~cover_edges=?, cloud, ~spacing) => {
  init_f(
    ~side=cloud.side, ~cover_edges?, ~spacing, (~xf, ~yf, ~xi as _, ~yi as _) =>
    nearest(cloud, xf, yf)
  );
};

/**
  creates a new point cloud of the same dimensions but with different spacing. Each point in the new cloud samples from the nearest in the old cloud
  */
let subdivide_with_edge = (~cover_edges=?, cloud, ~edge, ~spacing) => {
  init_f(
    ~side=cloud.side, ~cover_edges?, ~spacing, (~xf, ~yf, ~xi as _, ~yi as _) =>
    nearest_with_edge(cloud, edge, xf, yf)
  );
};

let subdivide_interpolate4 = (cloud, ~spacing) => {
  let fn = (~xf, ~yf, ~xi as _, ~yi as _) => interpolate4(cloud, xf, yf);
  init_f(~side=cloud.side, ~spacing, fn);
};

let scale = (cloud, ~s) => {
  let {side, spacing, offset, points} = cloud;
  let sf = float(s);
  let new_points =
    Sparse_grid.map(points, (_, {px, py, value}) =>
      {px: px *. sf, py: py *. sf, value}
    );
  {
    side: side * s,
    spacing: spacing * s,
    offset: offset * s,
    points: new_points,
  };
};
