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
      ((sum, div), ((dx, dz), piece)) => {
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

let apply_assembly = (args: Minecraft_converter.region_args, assembly): unit => {
  let {center_piece: (x, z), pieces} = assembly;
  let y = flatten_assembly_base(args, assembly);
  List.iter(
    (((dx, dz), piece)) => apply_piece(args, ~x, ~y, ~z, ~dx, ~dz, piece),
    pieces,
  );
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