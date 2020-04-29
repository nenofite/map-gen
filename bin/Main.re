Printf.printf("%s\n", Lib.Common.foo);

open Lib.Grid;

let g = make(2, 2, 1);
let f = g =>
  Lib.Subdivide.subdivide(g, (a, b, c, d) => (a + b + c + d) mod 10);
let g = g |> f |> f |> f |> f |> f;
Lib.Print.print(g, string_of_int);