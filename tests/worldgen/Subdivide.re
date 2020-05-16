open Gin;
open Worldgen;

run(
  "Subdivide",
  [
    "subdivide_with_fill"
    >: (
      () => {
        /*
          0  1  2  3
          4  5  6  7
          8  9 10 11
         12 13 14 15
         */
        let grid =
          Grid.init(4, (x, y) => [x + y * 4])
          |> Subdivide.subdivide_with_fill(_, (a, b, c, d) => a @ b @ c @ d);

        Alcotest.(check(list(int)))(
          "0,0 (old)",
          [0],
          Grid.at(grid, 0, 0),
        );
        Alcotest.(check(list(int)))(
          "1,1 (diamond)",
          [1, 6, 9, 4],
          Grid.at(grid, 0, 0),
        );
        Alcotest.(check(list(int)))(
          "1,0 (offset square)",
          [8, 10, 2, 0, 2, 0, 2, 10, 8, 0],
          Grid.at(grid, 0, 0),
        );
        Alcotest.(check(list(int)))(
          "0,1 (center square)",
          [0, 0, 2, 10, 8, 8, 2, 0, 8, 10],
          Grid.at(grid, 0, 0),
        );
      }
    ),
  ],
);