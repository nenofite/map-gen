open! Core_kernel;

/**
  t is a templated structure, tree, etc. that can be pasted into the block
  tree
 */
[@deriving (eq, bin_io)]
type t = {
  blocks: list((int, int, int, Minecraft.Block.material)),
  bounds_x: (int, int),
  bounds_y: (int, int),
  bounds_z: (int, int),
  footprint: list((int, int)),
};

type mark = (int, int) => int;
type x_mark =
  | X(mark);
type y_mark =
  | Y(mark);
type z_mark =
  | Z(mark);

let center = (a, b) => (a + b) / 2;
let zero = (_, _) => 0;

let height_of = template => {
  let (miny, maxy) = template.bounds_y;
  maxy - miny + 1;
};

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
  {blocks, bounds_x, bounds_y, bounds_z, footprint};
};

let combine_all = pieces => {
  let blocks =
    List.dedup_and_sort(
      List.concat_map(pieces, ~f=t => t.blocks),
      ~compare=compare_coord_block,
    );
  let bounds_x =
    List.map(pieces, ~f=t => t.bounds_x) |> List.reduce_exn(~f=combine_bounds);
  let bounds_y =
    List.map(pieces, ~f=t => t.bounds_y) |> List.reduce_exn(~f=combine_bounds);
  let bounds_z =
    List.map(pieces, ~f=t => t.bounds_z) |> List.reduce_exn(~f=combine_bounds);
  let footprint =
    List.map(pieces, ~f=t => t.footprint)
    |> List.reduce_exn(~f=combine_footprints);
  {blocks, bounds_x, bounds_y, bounds_z, footprint};
};

/**
  combine puts addition into base.

  No collision checking is performed. If two blocks have the same coords,
  addition will overwrite base.
 */
let combine = (base, addition) => {
  combine_all([base, addition]);
};

/**
  check_collision is true if the template would hit a non-Air block or go out
  of the region. Ignores Air in the template.
 */
let check_collision = (template, ~x, ~y, ~z, r) => {
  List.exists(
    ~f=
      ((dx, dy, dz, block)) =>
        if (!Minecraft.Block.(equal_material(block, Air))) {
          switch (
            Minecraft.Region.get_block_opt(~x=x + dx, ~y=y + dy, ~z=z + dz, r)
          ) {
          | Some(Air) => false
          | None => true
          | Some(_not_air) => true
          };
        } else {
          false;
        },
    template.blocks,
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

/**
  place attempts to paste a template at the given coordinate. If any of the
  blocks are not air or are outside the region, it fails and returns false
 */
let place = (template, ~x, ~y, ~z, r) =>
  if (!check_collision(template, ~x, ~y, ~z, r)) {
    place_overwrite(template, ~x, ~y, ~z, r);
    true;
  } else {
    false;
  };

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

let align_with_origin = (a, ~my) => {
  let (ax, ay, az) = calc_mark(my, ~on=a);
  translate(a, 0 - ax, 0 - ay, 0 - az);
};

let of_blocks = (blocks: _): t => {
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
  {blocks, bounds_x, bounds_y, bounds_z, footprint};
};

let flip_y = template => {
  let (_, max_y) = template.bounds_y;
  let blocks =
    List.map(template.blocks, ~f=((x, y, z, block)) =>
      (x, max_y - y, z, block)
    );
  {...template, blocks};
};

let rotate_90_cw_once = template => {
  let map_coord = ((x, y, z, m)) => (- z, y, x, m);
  of_blocks(List.map(template.blocks, ~f=map_coord));
};

let rec rotate_90_cw = (template, ~times) =>
  if (times > 0) {
    rotate_90_cw(rotate_90_cw_once(template), ~times=times - 1);
  } else {
    template;
  };

let rect = (material, ~xs, ~ys, ~zs) => {
  let blocks =
    Mg_util.Range.(
      map(0, zs - 1, z =>
        map(0, xs - 1, x => map(0, ys - 1, y => (x, y, z, material)))
      )
    )
    |> List.concat
    |> List.concat;
  of_blocks(blocks);
};

let clear_at = (t, ~x, ~y, ~z) => {
  let blocks =
    List.filter(t.blocks, ~f=((hx, hy, hz, _)) =>
      !(hx == x && hy == y && hz == z)
    );
  of_blocks(blocks);
};
