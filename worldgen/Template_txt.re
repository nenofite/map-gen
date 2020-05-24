type palette = list((string, option(Minecraft.Block.material)));

let default_palette: palette =
  Minecraft.Block.[
    (".", None),
    ("-", Some(Air)),
    ("X", Some(Cobblestone)),
    ("=", Some(Planks)),
    ("#", Some(Glass)),
    ("D", Some(Wooden_door)),
  ];

let rec read_slice = (fin, palette, blocks, ~y, ~z) => {
  switch (input_line(fin) |> String.trim) {
  | "" => blocks
  | line =>
    let blocks =
      String.split_on_char(' ', line)
      |> List.mapi((i, sym) => (i, sym), _)
      |> List.fold_left(
           (blocks, (x, sym)) => {
             switch (List.assoc(sym, palette)) {
             | Some(block) => [(x, y, z, block), ...blocks]
             | None => blocks
             }
           },
           blocks,
           _,
         );
    read_slice(fin, palette, blocks, ~y, ~z=z + 1);
  | exception End_of_file => blocks
  };
};
let read_slice = (fin, palette, ~y) => {
  read_slice(fin, palette, [], ~y, ~z=0);
};

let rec read_template = (fin, palette, blocks, ~y) => {
  switch (read_slice(fin, palette, ~y)) {
  | [] => blocks
  | slice =>
    let blocks = slice @ blocks;
    read_template(fin, palette, blocks, ~y=y + 1);
  };
};
let read_template = (~palette=default_palette, fin) => {
  let blocks = read_template(fin, palette, [], ~y=0);
  Minecraft.Template.{blocks: blocks};
};