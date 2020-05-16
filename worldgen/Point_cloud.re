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

/**
  nearest_with_edge returns the value of the nearest point, or edge_value if
  the edge is closer than any point.
 */
let nearest_with_edge = (cloud, edge_value, x, y) => {
  assert_within(cloud.width, cloud.height, x, y);
  let width_f = float_of_int(cloud.width);
  let height_f = float_of_int(cloud.height);
  let edge_distance = min(min(x, width_f -. x), min(y, height_f -. y));
  let closest_point =
    List.fold_left(
      (acc, point) => {
        let acc_distance = distance(acc.x, acc.y, x, y);
        let point_distance = distance(point.x, point.y, x, y);
        if (point_distance < acc_distance) {
          point;
        } else {
          acc;
        };
      },
      List.hd(cloud.points),
      cloud.points,
    );
  let closest_point_distance =
    distance(closest_point.x, closest_point.y, x, y);
  if (closest_point_distance <= edge_distance) {
    closest_point.value;
  } else {
    edge_value;
  };
};