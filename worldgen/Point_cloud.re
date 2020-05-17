type point('v) = {
  x: float,
  y: float,
  value: 'v,
};

type t('v) = {
  points: list(point('v)),
  width: int,
  height: int,
};

/**
  init creates a point cloud, randomizes the points, and initializes their
  value by calling `f(x, y)`.

  spacing determines how many points will fill the space. If width=10 and
  spacing=2, then there will be 5 points horizontally.
 */
let init = (~width, ~height, ~spacing=1, f) => {
  let points = ref([]);
  for (yi in 1 to height / spacing - 2) {
    for (xi in 1 to width / spacing - 2) {
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
      let point = {x: xf, y: yf, value: f(x, y)};
      points := [point, ...points^];
    };
  };
  {points: points^, width, height};
};

let assert_within = (width, height, x, y) =>
  if (!(
        0. <= x
        && x < float_of_int(width)
        && 0. <= y
        && y < float_of_int(height)
      )) {
    let msg =
      Printf.sprintf("point is outside boundaries of cloud: %f, %f", x, y);
    raise(Invalid_argument(msg));
  };

let distance = (ax, ay, bx, by) =>
  sqrt((ax -. bx) ** 2. +. (ay -. by) ** 2.);

let closest_point = (cloud, x, y) => {
  let first = List.hd(cloud.points);
  let first_distance = distance(first.x, first.y, x, y);
  List.fold_left(
    ((a, a_distance) as acc, point) => {
      let point_distance = distance(point.x, point.y, x, y);
      if (point_distance < a_distance) {
        (point, point_distance);
      } else {
        acc;
      };
    },
    (first, first_distance),
    cloud.points,
  );
};

let two_closest_points = (cloud, x, y) => {
  let first = List.hd(cloud.points);
  let first_distance = distance(first.x, first.y, x, y);
  List.fold_left(
    ((a, a_distance, b, b_distance) as acc, point) => {
      let point_distance = distance(point.x, point.y, x, y);
      switch (point_distance < a_distance, point_distance < b_distance) {
      | (true, _) => (point, point_distance, a, a_distance)
      | (false, true) => (a, a_distance, point, point_distance)
      | (false, false) => acc
      };
    },
    (first, first_distance, first, first_distance),
    cloud.points,
  );
};

/**
  nearest_with_edge returns the value of the nearest point, or edge_value if
  the edge is closer than any point.
 */
let nearest_with_edge = (cloud, edge_value, x, y) => {
  assert_within(cloud.width, cloud.height, x, y);
  let width_f = float_of_int(cloud.width);
  let height_f = float_of_int(cloud.height);
  let edge_distance = min(min(x, width_f -. x), min(y, height_f -. y));
  let (closest_point, closest_point_distance) = closest_point(cloud, x, y);
  if (closest_point_distance <= edge_distance) {
    closest_point.value;
  } else {
    edge_value;
  };
};

/**
  interpolate finds the two closest points and returns an interpolation based
  on the relative distance to each.
 */
let interpolate = (cloud, x, y) => {
  let (a, a_distance, b, b_distance) = two_closest_points(cloud, x, y);
  let frac = a_distance /. (a_distance +. b_distance);
  a.value *. frac +. b.value *. (1. -. frac);
};