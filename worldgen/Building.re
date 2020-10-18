type usage =
  | Common
  | Bedroom
  | Work;

type direction =
  | N
  | E
  | S
  | W;

let xz_of_direction = direction =>
  switch (direction) {
  | N => (0, (-1))
  | E => (1, 0)
  | S => (0, 1)
  | W => ((-1), 0)
  };

type piece = {
  template: Minecraft.Template.t,
  open_sides: list(direction),
  usage,
};

type assembly = {
  center_piece: (int, int),
  pieces: list(((int, int), piece)),
};

exception Building_failure(string);

let piece_side = 10;

let sum_elevation_in_piece =
    (args: Minecraft_converter.region_args, x, z): int => {
  let sum = ref(0);
  for (z in z to z + piece_side - 1) {
    for (x in x to x + piece_side - 1) {
      sum := sum^ + Minecraft.Region.height_at(~x, ~z, args.region);
    };
  };
  sum^;
};

/** gets the mean ground elevation in the area covered by this assembly */
let average_elevation_in_assembly =
    (args: Minecraft_converter.region_args, assembly): int => {
  let {center_piece: (x, z), pieces} = assembly;
  let (sum, div) =
    List.fold_left(
      ((sum, div), ((dx, dz), _piece)) => {
        let sum =
          sum
          + sum_elevation_in_piece(
              args,
              x + dx * piece_side,
              z + dz * piece_side,
            );
        let div = div + piece_side * piece_side;
        (sum, div);
      },
      (0, 0),
      pieces,
    );
  sum / div;
};

let raise_lower_elev = (args: Minecraft_converter.region_args, x, z, elev) => {
  let current_elev = Minecraft.Region.height_at(~x, ~z, args.region);
  if (elev <= current_elev) {
    /* Dig down then finish with Cobblestone */
    for (y in elev + 1 to current_elev) {
      Minecraft.Region.set_block(~x, ~y, ~z, Air, args.region);
    };
    Minecraft.Region.set_block(~x, ~y=elev, ~z, Cobblestone, args.region);
  } else if (elev > current_elev) {
    /* Build up with Cobblestone */
    for (y in current_elev + 1 to elev) {
      Minecraft.Region.set_block(~x, ~y, ~z, Cobblestone, args.region);
    };
  };
};

let raise_lower_elev_match =
    (args: Minecraft_converter.region_args, x, z, elev) => {
  let current_elev = Minecraft.Region.height_at(~x, ~z, args.region);
  let current_mat =
    Minecraft.Region.get_block(~x, ~y=current_elev, ~z, args.region);
  if (elev <= current_elev) {
    /* Dig down */
    for (y in elev + 1 to current_elev) {
      Minecraft.Region.set_block(~x, ~y, ~z, Air, args.region);
    };
    Minecraft.Region.set_block(~x, ~y=elev, ~z, current_mat, args.region);
  } else if (elev > current_elev) {
    /* Build up with matching material */
    for (y in current_elev + 1 to elev) {
      Minecraft.Region.set_block(~x, ~y, ~z, current_mat, args.region);
    };
  };
};

/** creates a flat base on which to build the assembly, and returns the assembly elevation */
let flatten_assembly_base =
    (args: Minecraft_converter.region_args, assembly): int => {
  let {center_piece: (x, z), pieces} = assembly;
  let elev = average_elevation_in_assembly(args, assembly);
  List.iter(
    (((dx, dz), _piece)) => {
      let x = x + dx * piece_side;
      let z = z + dz * piece_side;
      for (z in z to z + piece_side - 1) {
        for (x in x to x + piece_side - 1) {
          raise_lower_elev(args, x, z, elev);
        };
      };
    },
    pieces,
  );
  elev + 1;
};

let flatten_footprint =
    (args: Minecraft_converter.region_args, ~x, ~z, footprint): int => {
  /* Get ground height at each coord */
  let with_heights =
    List.map(
      ((dx, dz)) => {
        let x = dx + x;
        let z = dz + z;
        let y = Minecraft.Region.height_at(~x, ~z, args.region);
        (x, y, z);
      },
      footprint,
    );
  /* Calculate average */
  let foundation_elev =
    List.fold_left((sum, (_x, y, _z)) => sum + y, 0, with_heights)
    / List.length(with_heights)
    - 1;
  /* Apply foundation */
  List.iter(
    ((x, y, z)) =>
      if (foundation_elev <= y) {
        /* Dig down then finish with Cobblestone */
        for (y in foundation_elev + 1 to y) {
          Minecraft.Region.set_block(~x, ~y, ~z, Air, args.region);
        };
        Minecraft.Region.set_block(
          ~x,
          ~y=foundation_elev,
          ~z,
          Cobblestone,
          args.region,
        );
      } else if (foundation_elev > y) {
        /* Build up with Cobblestone */
        for (y in y + 1 to foundation_elev) {
          Minecraft.Region.set_block(~x, ~y, ~z, Cobblestone, args.region);
        };
      },
    with_heights,
  );
  foundation_elev + 1;
};

let apply_piece =
    (args: Minecraft_converter.region_args, ~x, ~y, ~z, ~dx, ~dz, piece): unit => {
  let x = x + dx * piece_side;
  let z = z + dz * piece_side;
  let success =
    Minecraft.Template.place(piece.template, args.region, ~x, ~y, ~z);
  if (!success) {
    raise(Building_failure("collision while placing piece"));
  };
};

/** levels the ground and places an assemly */
let apply_assembly = (args: Minecraft_converter.region_args, assembly): unit => {
  let {center_piece: (x, z), pieces} = assembly;
  let y = flatten_assembly_base(args, assembly);
  List.iter(
    (((dx, dz), piece)) => apply_piece(args, ~x, ~y, ~z, ~dx, ~dz, piece),
    pieces,
  );
};

let apply_template_y =
    (
      args: Minecraft_converter.region_args,
      ~x,
      ~z,
      template: Minecraft.Template.t,
    )
    : int => {
  /* Flatten the footprint and get an elevation */
  let y = flatten_footprint(args, ~x, ~z, template.footprint);
  /* Apply at the given elevation */
  let success = Minecraft.Template.place(template, args.region, ~x, ~y, ~z);
  if (!success) {
    raise(Building_failure("collision while applying template"));
  };
  y;
};
/** levels the ground and places a single template */
let apply_template =
    (args: Minecraft_converter.region_args, ~x, ~z, template): unit =>
  apply_template_y(args: Minecraft_converter.region_args, ~x, ~z, template)
  |> ignore;

let add_piece = (piece, ~dx, ~dz, assembly) => {
  if (List.mem_assoc((dx, dz), assembly.pieces)) {
    let msg = Printf.sprintf("piece already exists at (%d, %d)", dx, dz);
    raise(Building_failure(msg));
  };
  {...assembly, pieces: [((dx, dz), piece), ...assembly.pieces]};
};

let is_piece_closed = (assembly, ~dx, ~dz, piece) => {
  List.for_all(
    dir => {
      let (ndx, ndz) = xz_of_direction(dir);
      let ndx = dx + ndx;
      let ndz = dz + ndz;
      List.mem_assoc((ndx, ndz), assembly.pieces);
    },
    piece.open_sides,
  );
};

let is_assembly_closed = assembly => {
  List.for_all(
    (((dx, dz), piece)) => is_piece_closed(assembly, ~dx, ~dz, piece),
    assembly.pieces,
  );
};

let rec column_down =
        (args: Minecraft_converter.region_args, ~x, ~y, ~z, block): unit => {
  switch (Minecraft.Region.get_block_opt(~x, ~y, ~z, args.region)) {
  | Some(Air) =>
    Minecraft.Region.set_block(~x, ~y, ~z, block, args.region);
    column_down(args, ~x, ~y=y - 1, ~z, block);
  | Some(_)
  | None => ()
  };
};

let rectangle_foundation = (args, ~minx, ~maxx, ~y, ~minz, ~maxz): unit => {
  for (z in minz to maxz) {
    for (x in minx to maxx) {
      column_down(args, ~x, ~y, ~z, Cobblestone);
      /* Also do the level below in case a building is here */
      column_down(args, ~x, ~y=y - 1, ~z, Cobblestone);
    };
  };
};

let rec lay_stairs =
        (args: Minecraft_converter.region_args, ~x, ~y, ~z, block, ~dx, ~dz)
        : unit => {
  switch (Minecraft.Region.get_block_opt(~x, ~y, ~z, args.region)) {
  | Some(Air) =>
    Minecraft.Region.set_block(~x, ~y, ~z, block, args.region);
    column_down(args, ~x, ~y=y - 1, ~z, Minecraft.Block.Cobblestone);
    lay_stairs(args, ~x=x + dx, ~y=y - 1, ~z=z + dz, block, ~dx, ~dz);
  | Some(_)
  | None => ()
  };
};

let stair_foundation = (args, ~minx, ~maxx, ~y, ~minz, ~maxz): unit => {
  /* Fill the base rectangle */
  rectangle_foundation(args, ~minx, ~maxx, ~y, ~minz, ~maxz);
  /* Lay each side of stairs */
  for (x in minx to maxx) {
    /* N stairs */
    lay_stairs(
      args,
      ~x,
      ~y,
      ~z=minz - 1,
      Minecraft.Block.Cobblestone_stairs(S),
      ~dx=0,
      ~dz=-1,
    );
    /* S stairs */
    lay_stairs(
      args,
      ~x,
      ~y,
      ~z=maxz + 1,
      Minecraft.Block.Cobblestone_stairs(N),
      ~dx=0,
      ~dz=1,
    );
  };
  for (z in minz to maxz) {
    /* E stairs */
    lay_stairs(
      args,
      ~x=maxx + 1,
      ~y,
      ~z,
      Minecraft.Block.Cobblestone_stairs(W),
      ~dx=1,
      ~dz=0,
    );
    /* W stairs */
    lay_stairs(
      args,
      ~x=minx - 1,
      ~y,
      ~z,
      Minecraft.Block.Cobblestone_stairs(E),
      ~dx=-1,
      ~dz=0,
    );
  };
};