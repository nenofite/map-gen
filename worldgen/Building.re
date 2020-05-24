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
      sum := sum^ + Minecraft.Block_tree.height_at(args.region, x, z);
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
  let current_elev = Minecraft.Block_tree.height_at(args.region, x, z);
  if (elev <= current_elev) {
    /* Dig down then finish with Cobblestone */
    for (y in elev + 1 to current_elev) {
      Minecraft.Block_tree.set_block(args.region, x, y, z, Air);
    };
    Minecraft.Block_tree.set_block(args.region, x, elev, z, Cobblestone);
  } else if (elev > current_elev) {
    /* Build up with Cobblestone */
    for (y in current_elev + 1 to elev) {
      Minecraft.Block_tree.set_block(args.region, x, y, z, Cobblestone);
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
        let y = Minecraft.Block_tree.height_at(args.region, x, z);
        (x, y, z);
      },
      footprint,
    );
  /* Calculate average */
  let avg_height =
    List.fold_left((sum, (_x, y, _z)) => sum + y, 0, with_heights)
    / List.length(with_heights);
  /* Apply average height */
  List.iter(
    ((x, y, z)) =>
      if (avg_height <= y) {
        /* Dig down then finish with Cobblestone */
        for (y in avg_height + 1 to y) {
          Minecraft.Block_tree.set_block(args.region, x, y, z, Air);
        };
        Minecraft.Block_tree.set_block(
          args.region,
          x,
          avg_height,
          z,
          Cobblestone,
        );
      } else if (avg_height > y) {
        /* Build up with Cobblestone */
        for (y in y + 1 to avg_height) {
          Minecraft.Block_tree.set_block(args.region, x, y, z, Cobblestone);
        };
      },
    with_heights,
  );
  avg_height + 1;
};

let apply_piece =
    (args: Minecraft_converter.region_args, ~x, ~y, ~z, ~dx, ~dz, piece): unit => {
  let x = x + dx * piece_side;
  let z = z + dz * piece_side;
  let success =
    Minecraft.Template.place(piece.template, args.region, x, y, z);
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

/** levels the ground and places a single template */
let apply_template =
    (args: Minecraft_converter.region_args, ~x, ~z, template): unit => {
  /* Get the footprint of the template */
  let footprint = Minecraft.Template.footprint(template);
  /* Flatten the footprint and get an elevation */
  let y = flatten_footprint(args, ~x, ~z, footprint);
  /* Apply at the given elevation */
  let success = Minecraft.Template.place(template, args.region, x, y, z);
  if (!success) {
    raise(Building_failure("collision while applying template"));
  };
};

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