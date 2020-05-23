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
  elevation: int,
  pieces: list(((int, int), piece)),
};

exception Building_failure(string);

let piece_side = 10;

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
  let {center_piece: (x, z), elevation, pieces} = assembly;
  List.iter(
    (((dx, dz), piece)) =>
      apply_piece(args, ~x, ~y=elevation, ~z, ~dx, ~dz, piece),
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