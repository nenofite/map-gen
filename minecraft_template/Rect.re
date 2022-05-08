open! Core;
open Core;

type rect = {
  min_x: int,
  max_x: int,
  min_y: int,
  max_y: int,
  min_z: int,
  max_z: int,
};
type t = list(rect);

let rect = (~xs, ~ys, ~zs): t => {
  [
    {
      min_x: 0,
      min_y: 0,
      min_z: 0,
      max_x: xs - 1,
      max_y: ys - 1,
      max_z: zs - 1,
    },
  ];
};

let fill = (t: t, ~material: Minecraft.Block.material) => {
  List.concat_map(
    t,
    ~f=r => {
      let {min_x, max_x, min_y, max_y, min_z, max_z} = r;
      Mg_util.Range.(
        map(min_z, max_z, z =>
          map(min_x, max_x, x => map(min_y, max_y, y => (x, y, z, material)))
        )
      )
      |> List.concat
      |> List.concat;
    },
  )
  |> of_blocks(~marks=[]);
};

let must_be_singular = (t: t) =>
  switch (t) {
  | [r] => r
  | [] => failwith("Needed singular Rect.t but received empty")
  | rs =>
    failwithf(
      "Needed singular Rect.t but received length=%d",
      List.length(rs),
      (),
    )
  };

let floor = (t: t) => {
  let {min_x, max_x, min_y, max_y: _, min_z, max_z} = must_be_singular(t);
  [{min_x, max_x, min_y, max_y: min_y, min_z, max_z}];
};

let columns = (t: t) => {
  let {min_x, max_x, min_y, max_y, min_z, max_z} = must_be_singular(t);
  let column = (x, z) => {
    min_x: x,
    max_x: x,
    min_y,
    max_y,
    min_z: z,
    max_z: z,
  };
  [
    column(min_x, min_z),
    column(max_x, min_z),
    column(min_x, max_z),
    column(max_x, max_z),
  ];
};

let corners = (t: t) => {
  let b = (x, y, z) => {
    min_x: x,
    max_x: x,
    min_y: y,
    max_y: y,
    min_z: z,
    max_z: z,
  };
  let {min_x, max_x, min_y, max_y, min_z, max_z} = must_be_singular(t);
  [
    b(min_x, min_y, min_z),
    b(max_x, min_y, min_z),
    b(min_x, min_y, max_z),
    b(max_x, min_y, max_z),
    b(min_x, max_y, min_z),
    b(max_x, max_y, min_z),
    b(min_x, max_y, max_z),
    b(max_x, max_y, max_z),
  ];
};