open! Core_kernel;

/**
  t is a templated structure, tree, etc. that can be pasted into the block
  tree
 */
[@deriving (eq, bin_io)]
type t('a) = {
  blocks: list((int, int, int, Minecraft.Block.material)),
  bounds_x: (int, int),
  bounds_y: (int, int),
  bounds_z: (int, int),
  footprint: list((int, int)),
  marks: list((int, int, int, 'a)),
};

type mark = (int, int) => int;
type x_mark =
  | X(mark);
type y_mark =
  | Y(mark);
type z_mark =
  | Z(mark);

let empty = {
  blocks: [],
  bounds_x: (0, 0),
  bounds_y: (0, 0),
  bounds_z: (0, 0),
  footprint: [],
  marks: [],
};

let is_empty = t => List.is_empty(t.blocks);

let center = (a, b) => (a + b) / 2;
let zero = (_, _) => 0;
let shift = (mark, ~by, a, b) => mark(a, b) + by;

let x_size_of = template => {
  let (min, max) = template.bounds_x;
  max - min + 1;
};

let y_size_of = template => {
  let (min, max) = template.bounds_y;
  max - min + 1;
};

let z_size_of = template => {
  let (min, max) = template.bounds_z;
  max - min + 1;
};

let x_bounds_of = t => t.bounds_x;
let y_bounds_of = t => t.bounds_y;
let z_bounds_of = t => t.bounds_z;

let height_of = y_size_of;

let compare_coord_block = ((ax, ay, az, _), (bx, by, bz, _)) =>
  switch (Int.(compare(ax, bx), compare(ay, by), compare(az, bz))) {
  | (0, 0, 0) => 0
  | (0, 0, n) => n
  | (0, n, _) => n
  | (n, _, _) => n
  };

let combine_bounds = ((amin: int, amax: int), (bmin: int, bmax: int)) => (
  min(amin, bmin),
  max(amax, bmax),
);

let combine_footprints = (afootprint, bfootprint) =>
  Caml.List.sort_uniq(Poly.compare, afootprint @ bfootprint);

let translate = (base, x, y, z) => {
  let {
    blocks,
    bounds_x: (minx, maxx),
    bounds_y: (miny, maxy),
    bounds_z: (minz, maxz),
    footprint,
    marks,
  } = base;
  let blocks =
    List.map(
      ~f=((dx, dy, dz, material)) => (x + dx, y + dy, z + dz, material),
      blocks,
    );
  let bounds_x = (minx + x, maxx + x);
  let bounds_y = (miny + y, maxy + y);
  let bounds_z = (minz + z, maxz + z);
  let footprint = List.map(~f=((dx, dz)) => (x + dx, z + dz), footprint);
  let marks =
    List.map(marks, ~f=((dx, dy, dz, m)) => (x + dx, y + dy, z + dz, m));
  {blocks, bounds_x, bounds_y, bounds_z, footprint, marks};
};

let combine_all = pieces => {
  let blocks =
    List.dedup_and_sort(
      List.concat_map(pieces, ~f=t => t.blocks),
      ~compare=compare_coord_block,
    );
  let marks = List.concat_map(pieces, ~f=p => p.marks);
  let bounds_x =
    List.map(pieces, ~f=t => t.bounds_x) |> List.reduce_exn(~f=combine_bounds);
  let bounds_y =
    List.map(pieces, ~f=t => t.bounds_y) |> List.reduce_exn(~f=combine_bounds);
  let bounds_z =
    List.map(pieces, ~f=t => t.bounds_z) |> List.reduce_exn(~f=combine_bounds);
  let footprint =
    List.map(pieces, ~f=t => t.footprint)
    |> List.reduce_exn(~f=combine_footprints);
  {blocks, bounds_x, bounds_y, bounds_z, footprint, marks};
};

/**
  combine puts addition into base.

  No collision checking is performed. If two blocks have the same coords,
  addition will overwrite base.
 */
let combine = (base, addition) => {
  combine_all([base, addition]);
};

let check_collision_ignoring = (template, ~ignorable, ~x, ~y, ~z, r) => {
  List.exists(
    ~f=
      ((dx, dy, dz, block)) =>
        if (!Minecraft.Block.is_air(block)) {
          switch (
            Minecraft.Region.get_block_opt(~x=x + dx, ~y=y + dy, ~z=z + dz, r)
          ) {
          | Some(b) => !ignorable(b)
          | None => true
          };
        } else {
          false;
        },
    template.blocks,
  );
};

/**
  check_collision is true if the template would hit a non-Air block or go out
  of the region. Ignores Air in the template.
 */
let check_collision = (template, ~x, ~y, ~z, r) => {
  check_collision_ignoring(
    template,
    ~ignorable=Minecraft.Block.is_air,
    ~x,
    ~y,
    ~z,
    r,
  );
};

/**
  place_overwrite pastes a template at the given coordinate. Collisions are
  ignored.
 */
let place_overwrite = (template, ~x, ~y, ~z, r) => {
  List.iter(
    ~f=
      ((dx, dy, dz, material)) => {
        Minecraft.Region.set_block(
          ~x=dx + x,
          ~y=dy + y,
          ~z=dz + z,
          material,
          r,
        )
      },
    template.blocks,
  );
};

let place_overwrite_opt = (template, ~x, ~y, ~z, r) => {
  List.iter(
    ~f=
      ((dx, dy, dz, material)) => {
        Minecraft.Region.set_block_opt(
          ~x=dx + x,
          ~y=dy + y,
          ~z=dz + z,
          material,
          r,
        )
      },
    template.blocks,
  );
};

/**
  place attempts to paste a template at the given coordinate. If any of the
  blocks are not air or are outside the region, it fails and returns false
 */
let place_ignoring = (template, ~ignorable, ~x, ~y, ~z, r) =>
  if (!check_collision_ignoring(template, ~ignorable, ~x, ~y, ~z, r)) {
    place_overwrite(template, ~x, ~y, ~z, r);
    true;
  } else {
    false;
  };

/**
  place attempts to paste a template at the given coordinate. If any of the
  blocks are not air or are outside the region, it fails and returns false
 */
let place = (template, ~x, ~y, ~z, r) =>
  place_ignoring(template, ~ignorable=Minecraft.Block.is_air, ~x, ~y, ~z, r);

/**
  footprint is the list of unique (x, z) coordinates where this template has at
  least one block, regardless of its y. Ignores Air blocks in the template.
  */
let calc_footprint = blocks => {
  List.fold(
    blocks,
    ~f=
      (footprint, (x, _y, z, block: Minecraft.Block.material)) => {
        switch (block) {
        | Air => footprint
        | _ =>
          let coord = (x, z);
          if (!List.mem(footprint, coord, ~equal=Poly.equal)) {
            [coord, ...footprint];
          } else {
            footprint;
          };
        }
      },
    ~init=[],
  );
};

let%expect_test "calc_footprint" = {
  let blocks =
    Minecraft.Block.[
      (0, 0, 0, Cobblestone),
      (0, 1, 0, Cobblestone),
      (1, 1, 0, Air),
      (1, 1, 1, Log(Oak_log, Y)),
    ];
  calc_footprint(blocks)
  |> List.iter(~f=((x, z)) => Printf.printf("%d, %d\n", x, z));
  %expect
  {|
    1, 1
    0, 0
  |};
};

/**
  stack is a convenience over combine. It finds the highest block in base,
  then puts addition one above that.
 */
let stack = (base, addition) => {
  let (_, highest_y) = base.bounds_y;
  translate(addition, 0, highest_y + 1, 0) |> combine(base, _);
};

let stack_all = ls => {
  List.reduce_exn(ls, ~f=stack);
};

let calc_mark = (t, ~on) => {
  let (X(fx), Y(fy), Z(fz)) = t;
  let apply_f = (f, (min, max)) => f(min, max + 1);
  let x = apply_f(fx, on.bounds_x);
  let y = apply_f(fy, on.bounds_y);
  let z = apply_f(fz, on.bounds_z);
  (x, y, z);
};

let align_with = (a, ~other as b, ~my, ~their) => {
  let (ax, ay, az) = calc_mark(my, ~on=a);
  let (bx, by, bz) = calc_mark(their, ~on=b);
  translate(a, bx - ax, by - ay, bz - az);
};

let align_with' =
    (a, ~other as b, ~x as (mx, ox), ~y as (my, oy), ~z as (mz, oz)) => {
  let (ax, ay, az) = calc_mark((X(mx), Y(my), Z(mz)), ~on=a);
  let (bx, by, bz) = calc_mark((X(ox), Y(oy), Z(oz)), ~on=b);
  translate(a, bx - ax, by - ay, bz - az);
};

let align_with_point = (a, ~point, ~my) => {
  let (px, py, pz) = point;
  let (ax, ay, az) = calc_mark(my, ~on=a);
  translate(a, px - ax, py - ay, pz - az);
};

let align_with_origin = (a, ~my) => {
  align_with_point(a, ~my, ~point=(0, 0, 0));
};

let align_with_origin' = (a, ~x, ~y, ~z) => {
  let (ax, ay, az) = calc_mark((X(x), Y(y), Z(z)), ~on=a);
  translate(a, 0 - ax, 0 - ay, 0 - az);
};

/**
  Align the template so its X and Z mins are at the origin. Does not change Y
  coordinates.  This is useful eg. after rotating a template so it can be placed
  predictably.
 */
let normalize_on_origin = a =>
  align_with_origin(a, ~my=(X(min), Y(zero), Z(min)));

let of_blocks = (blocks, ~marks) =>
  if (List.is_empty(blocks)) {
    empty;
  } else {
    /* Get starter values */
    let (sx, sy, sz, _) = List.hd_exn(blocks);
    let bounds_x =
      List.fold(
        blocks,
        ~f=
          ((amin, amax), (x, _y, _z, _b)) => {
            let amin = min(amin, x);
            let amax = max(amax, x);
            (amin, amax);
          },
        ~init=(sx, sx),
      );
    let bounds_y =
      List.fold(
        blocks,
        ~f=
          ((amin, amax), (_x, y, _z, _b)) => {
            let amin = min(amin, y);
            let amax = max(amax, y);
            (amin, amax);
          },
        ~init=(sy, sy),
      );
    let bounds_z =
      List.fold_left(
        blocks,
        ~f=
          ((amin, amax), (_x, _y, z, _b)) => {
            let amin = min(amin, z);
            let amax = max(amax, z);
            (amin, amax);
          },
        ~init=(sz, sz),
      );
    let footprint = calc_footprint(blocks);
    {blocks, bounds_x, bounds_y, bounds_z, footprint, marks};
  };

let flip_y = template => {
  let (_, max_y) = template.bounds_y;
  let blocks =
    List.map(template.blocks, ~f=((x, y, z, block)) =>
      (x, max_y - y, z, block)
    );
  let marks =
    List.map(template.marks, ~f=((x, y, z, mark)) =>
      (x, max_y - y, z, mark)
    );
  {...template, blocks, marks};
};

let rotate_90_cw_once = template => {
  let rotate_block = ((x, y, z, block)) => (
    - z,
    y,
    x,
    Minecraft.Block.rotate_cw(block, ~times=1),
  );
  let rotate_mark = ((x, y, z, mark)) => (- z, y, x, mark);
  of_blocks(
    List.map(template.blocks, ~f=rotate_block),
    ~marks=List.map(template.marks, ~f=rotate_mark),
  );
};

let rec rotate_90_cw = (template, ~times) =>
  if (times > 0) {
    rotate_90_cw(rotate_90_cw_once(template), ~times=times - 1);
  } else {
    template;
  };

let rect = (material, ~xs, ~ys, ~zs): t(_) => {
  let blocks =
    Mg_util.Range.(
      map(0, zs - 1, z =>
        map(0, xs - 1, x => map(0, ys - 1, y => (x, y, z, material)))
      )
    )
    |> List.concat
    |> List.concat;
  of_blocks(blocks, ~marks=[]);
};

let at = (t: t(_), ~x: int, ~y: int, ~z: int) => {
  List.find(t.blocks, ~f=((hx, hy, hz, _)) => hx == x && hy == y && hz == z)
  |> Option.map(~f=((_, _, _, b)) => b);
};

let clear_at = (t, ~x, ~y, ~z): t(_) => {
  let blocks =
    List.filter(t.blocks, ~f=((hx, hy, hz, _)) =>
      !(hx == x && hy == y && hz == z)
    );
  of_blocks(blocks, ~marks=t.marks);
};

let set_at =
    (t: t(_), ~x: int, ~y: int, ~z: int, ~block: Minecraft.Block.material)
    : t(_) => {
  let blocks =
    List.filter(t.blocks, ~f=((hx, hy, hz, _)) =>
      !(hx == x && hy == y && hz == z)
    );
  let blocks = [(x, y, z, block), ...blocks];
  of_blocks(blocks, ~marks=t.marks);
};

let find_blocks = (t: t(_), ~f) => {
  List.filter(t.blocks, ~f);
};

let get_mark = (t: t('a), ~mark: 'a) => {
  Core_kernel.(
    List.find_map(t.marks, ~f=((x, y, z, m)) =>
      if (Poly.(m == mark)) {
        Some((x, y, z));
      } else {
        None;
      }
    )
  );
};

let get_marks = (t: t('a), ~mark: 'a) => {
  Core_kernel.(
    List.filter_map(t.marks, ~f=((x, y, z, m)) =>
      if (Poly.(m == mark)) {
        Some((x, y, z));
      } else {
        None;
      }
    )
  );
};

let add_mark = (t: t('a), ~x, ~y, ~z, ~mark: 'a) => {
  {...t, marks: [(x, y, z, mark), ...t.marks]};
};

let strip_marks = (t: t(_)): t(_) => {
  {...t, marks: []};
};