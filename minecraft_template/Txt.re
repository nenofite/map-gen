type palette = list((string, option(Minecraft.Block.material)));

exception Template_txt_failure(string);

let default_palette: palette =
  Minecraft.Block.[
    (".", None),
    ("-", Some(Air)),
    ("X", Some(Cobblestone)),
    ("=", Some(Oak_planks)),
    ("#", Some(Glass)),
    ("O", Some(Log(Oak_log, Y))),
    ("v", Some(Stairs(Stone_stairs, N))),
    ("<", Some(Stairs(Stone_stairs, E))),
    ("^", Some(Stairs(Stone_stairs, S))),
    (">", Some(Stairs(Stone_stairs, W))),
    ("s", Some(Wall_torch(N))),
    ("w", Some(Wall_torch(E))),
    ("n", Some(Wall_torch(S))),
    ("e", Some(Wall_torch(W))),
    ("i", Some(Torch)),
  ];

let parse_line = (~palette, ~y, ~z, blocks, line) => {
  line
  |> String.split_on_char(' ')
  |> List.mapi((x, sym) =>
       (
         x,
         switch (List.assoc_opt(sym, palette)) {
         | Some(block) => block
         | None =>
           raise(Template_txt_failure("Unknown template symbol: " ++ sym))
         },
       )
     )
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
let parse_template = (~palette=[], s) => {
  let palette = palette @ default_palette;
  let lines =
    s |> String.trim |> String.split_on_char('\n') |> List.map(String.trim);
  let blocks = parse_template(~palette, ~y=0, ~z=0, [], lines);
  Core.of_blocks(blocks) |> Core.flip_y;
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
  Core.of_blocks(blocks);
};
