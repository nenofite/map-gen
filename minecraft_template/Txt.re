open! Core;

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

let parse_template = (~palette, ~has_spaces=true, s) => {
  let palette = palette @ default_palette;

  let grid = Mg_util.Parse_grid.parse_3d(~has_spaces, s);
  let blocks = ref([]);
  let marks = ref([]);
  List.iteri(grid, ~f=(y, layer) => {
    List.iteri(layer, ~f=(z, line) => {
      List.iteri(line, ~f=(x, sym) => {
        switch (List.Assoc.find(palette, ~equal=String.equal, sym)) {
        | None =>
          raise(Template_txt_failure("Unknown template symbol: " ++ sym))
        | Some(space) =>
          let (new_blocks, new_marks) = eval_space(space, ~x, ~y, ~z);
          marks := new_marks @ marks^;
          blocks := new_blocks @ blocks^;
        }
      })
    })
  });
  Template_core.of_blocks(blocks^, ~marks=marks^) |> Template_core.flip_y;
};

let%expect_test "parse a template with marks" = {
  open Core;
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
      [%sexp_of: option(Minecraft.Block.material)](
        Template_core.at(template, ~x, ~y, ~z),
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

  let (fx, fy, fz) =
    Option.value_exn(Template_core.get_mark(template, ~mark=`Foo));
  Printf.printf("%d %d %d\n", fx, fy, fz);
  %expect
  "0 0 1";
  let (bx, by, bz) =
    Option.value_exn(Template_core.get_mark(template, ~mark=`Bar));
  Printf.printf("%d %d %d\n", bx, by, bz);
  %expect
  "0 1 1";
};