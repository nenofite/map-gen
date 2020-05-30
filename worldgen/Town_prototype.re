type input = {
  elevation: Grid.t(int),
  obstacles: Sparse_grid.t(unit),
};

type output = {add_elevation: Grid.t(int)};

let draw = (output, file) => {
  open Images;
  open OImages;
  let side = 10; /* Grid.(output.side);*/
  let img = (new rgb24)(side, side);
  img#set(2, 2, {r: 255, g: 0, b: 0});
  img#save(file, Some(Png), []);
  img#destroy;
  ignore(output);
  ();
};

let test = () => {
  draw((), "town_proto.png");
};