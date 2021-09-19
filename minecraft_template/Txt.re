open! Core_kernel;

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
  Minecraft.Block.[(".", Empty), ("-", Filled(Air))];

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
    s
    |> Caml.String.trim
    |> String.split_on_chars(~on=['\n'])
    |> List.map(~f=Caml.String.trim);
  let (blocks, marks) = go(~y=0, ~z=0, [], [], lines);
  Core.of_blocks(blocks, ~marks) |> Core.flip_y;
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
  let template = parse_template(~palette, {|
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

  let (fx, fy, fz) = Option.value_exn(Core.get_mark(template, ~mark=`Foo));
  Printf.printf("%d %d %d\n", fx, fy, fz);
  %expect
  "0 0 1";
  let (bx, by, bz) = Option.value_exn(Core.get_mark(template, ~mark=`Bar));
  Printf.printf("%d %d %d\n", bx, by, bz);
  %expect
  "0 1 1";
};