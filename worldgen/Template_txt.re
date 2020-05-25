type palette = list((string, option(Minecraft.Block.material)));

let default_palette: palette =
  Minecraft.Block.[
    (".", None),
    ("-", Some(Air)),
    ("X", Some(Cobblestone)),
    ("=", Some(Planks)),
    ("#", Some(Glass)),
    ("D", Some(Wooden_door)),
    ("O", Some(Log)),
  ];

let parse_line = (~palette, ~y, ~z, blocks, line) => {
  line
  |> String.split_on_char(' ')
  |> List.mapi((x, sym) => (x, List.assoc(sym, palette)))
  |> List.fold_left(
       (blocks, (x, block)) => {
         switch (block) {
         | Some(block) => [(x, y, z, block), ...blocks]
         | None => blocks
         }
       },
       blocks,
       _,
     );
};

let rec parse_template = (~palette, ~y, ~z, blocks, lines) => {
  switch (lines) {
  | [] => blocks
  | ["", ...lines] => parse_template(~palette, ~y=y + 1, ~z=0, blocks, lines)
  | [line, ...lines] =>
    let blocks = parse_line(~palette, ~y, ~z, blocks, line);
    parse_template(~palette, ~y, ~z=z + 1, blocks, lines);
  };
};
let parse_template = (~palette=default_palette, s) => {
  let lines =
    s |> String.trim |> String.split_on_char('\n') |> List.map(String.trim);
  let blocks = parse_template(~palette, ~y=0, ~z=0, [], lines);
  Minecraft.Template.of_blocks(blocks);
};

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
  Minecraft.Template.of_blocks(blocks);
};