module Space = {
  type space('a) =
    | Empty
    | Filled(Minecraft.Block.material)
    | Marked('a, space('a));
};

/**
  Includes space and Minecraft blocks. This is useful to open when defining a
  palette.
 */
module Palette_incl = {
  include Minecraft.Block;
  include Space;
};

include Space;

type palette('a) = list((string, space('a)));

type marks('a) = list((int, int, int, 'a));

exception Template_txt_failure(string);

let default_palette: palette('a) =
  Minecraft.Block.[
    (".", Empty),
    ("-", Filled(Air)),
    ("X", Filled(Cobblestone)),
    ("=", Filled(Oak_planks)),
    ("#", Filled(Glass)),
    ("O", Filled(Log(Oak_log, Y))),
    ("v", Filled(Stairs(Stone_stairs, N))),
    ("<", Filled(Stairs(Stone_stairs, E))),
    ("^", Filled(Stairs(Stone_stairs, S))),
    (">", Filled(Stairs(Stone_stairs, W))),
    ("s", Filled(Wall_torch(N))),
    ("w", Filled(Wall_torch(E))),
    ("n", Filled(Wall_torch(S))),
    ("e", Filled(Wall_torch(W))),
    ("i", Filled(Torch)),
  ];

let eval_space = (space: space('a), ~x: int, ~y: int, ~z: int) => {
  let rec go = (space, marks) =>
    switch (space) {
    | Empty => ([], marks)
    | Filled(b) => ([(x, y, z, b)], marks)
    | Marked(m, rest_space) => go(rest_space, [(x, y, z, m), ...marks])
    };
  go(space, []);
};

let parse_line = (~palette, ~y, ~z, blocks, marks, line) => {
  open Core_kernel;
  let marks = ref(marks);
  let blocks = ref(blocks);
  List.iteri(String.split_on_chars(line, ~on=[' ']), ~f=(x, sym) =>
    switch (List.Assoc.find(palette, ~equal=String.equal, sym)) {
    | None => raise(Template_txt_failure("Unknown template symbol: " ++ sym))
    | Some(space) =>
      let (new_blocks, new_marks) = eval_space(space, ~x, ~y, ~z);
      marks := new_marks @ marks^;
      blocks := new_blocks @ blocks^;
    }
  );
  (blocks^, marks^);
};

let parse_template = (~palette=[], s) => {
  let palette = palette @ default_palette;
  let rec go = (~y, ~z, blocks, marks, lines) => {
    switch (lines) {
    | [] => (blocks, marks)
    | ["", ...lines] => go(~y=y + 1, ~z=0, blocks, marks, lines)
    | [line, ...lines] =>
      let (blocks, marks) =
        parse_line(~palette, ~y, ~z, blocks, marks, line);
      go(~y, ~z=z + 1, blocks, marks, lines);
    };
  };

  let lines =
    s |> String.trim |> String.split_on_char('\n') |> List.map(String.trim);
  let (blocks, marks) = go(~y=0, ~z=0, [], [], lines);
  let template = Core.of_blocks(blocks) |> Core.flip_y;
  let height = Core.y_size_of(template);
  let marks =
    Core_kernel.List.map(marks, ~f=((x, y, z, m)) =>
      (x, height - y - 1, z, m)
    );
  (template, marks);
};

let get_mark = (marks: marks('a), ~mark: 'a) => {
  Core_kernel.(
    List.find_map(marks, ~f=((x, y, z, m)) =>
      if (Poly.(m == mark)) {
        Some((x, y, z));
      } else {
        None;
      }
    )
  );
};

let%expect_test "parse a template with marks" = {
  open Core_kernel;
  let palette =
    Palette_incl.[
      ("-", Filled(Air)),
      (".", Empty),
      ("*", Marked(`Foo, Filled(Bedrock))),
      ("&", Marked(`Bar, Empty)),
    ];
  let (template, marks) = parse_template(~palette, {|
- -
& -

. .
* -
|});

  let print_at = (~x, ~y, ~z) =>
    print_s(
      sexp_of_option(
        Minecraft.Block.sexp_of_material,
        Core.at(template, ~x, ~y, ~z),
      ),
    );
  print_at(~x=0, ~y=1, ~z=0);
  print_at(~x=0, ~y=1, ~z=1);
  print_at(~x=1, ~y=0, ~z=0);
  print_at(~x=0, ~y=0, ~z=1);
  %expect
  {|
  (Air)
  ()
  ()
  (Bedrock)
  |};

  let (fx, fy, fz) = Option.value_exn(get_mark(marks, ~mark=`Foo));
  Printf.printf("%d %d %d\n", fx, fy, fz);
  %expect
  "0 0 1";
  let (bx, by, bz) = Option.value_exn(get_mark(marks, ~mark=`Bar));
  Printf.printf("%d %d %d\n", bx, by, bz);
  %expect
  "0 1 1";
};

let%expect_test "parse a template with repeated marks" = {
  ();
};