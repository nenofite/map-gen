open Gin;
open Worldgen;

let string_of_grid = (grid, string_of_elem) => {
  Grid.scan_fold(
    grid,
    "",
    ~row_f=(acc, _y) => acc ++ "\n",
    (acc, _x, _y, here) => acc ++ string_of_elem(here) ++ ",",
  );
};

let string_of_coord = (x, y) => string_of_int(x) ++ ":" ++ string_of_int(y);

run(
  "Grid",
  [
    "init"
    >: (
      () => {
        let grid = Grid.init(4, string_of_coord);

        let expected = "0:0,1:0,2:0,3:0,
0:1,1:1,2:1,3:1,
0:2,1:2,2:2,3:2,
0:3,1:3,2:3,3:3,
";
        Alcotest.(check(string))(
          "inits correctly",
          expected,
          string_of_grid(grid, s => s),
        );
      }
    ),
    "map"
    >: (
      () => {
        let grid =
          Grid.init(4, (x, y) => (x, y))
          |> Grid.map(
               _,
               (x, y, here) => {
                 let (here_x, here_y) = here;
                 Alcotest.(check(int))("coords match", here_x, x);
                 Alcotest.(check(int))("coords match", here_y, y);
                 (x, y);
               },
             );

        let expected = "0:0,1:0,2:0,3:0,
0:1,1:1,2:1,3:1,
0:2,1:2,2:2,3:2,
0:3,1:3,2:3,3:3,
";
        Alcotest.(check(string))(
          "maps correctly",
          expected,
          string_of_grid(grid, ((x, y)) => string_of_coord(x, y)),
        );
      }
    ),
    "neighbors"
    >: (
      () => {
        let grid = Grid.init(4, string_of_coord);

        [
          ((0, 0), ["3:3", "0:3", "1:3", "1:0", "1:1", "0:1", "3:1", "3:0"]),
        ]
        |> List.iter(
             (((x, y), expected)) => {
               let neighbors = Grid.neighbors(grid, x, y);
               Alcotest.(check(list(string)))(
                 "correct neighbors",
                 expected,
                 neighbors,
               );
             },
             _,
           );
      }
    ),
  ],
);