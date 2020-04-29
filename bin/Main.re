open Grid;

let g = make(2, 2, 1);
let f = g => Subdivide.subdivide(g, (a, b, c, d) => (a + b + c + d) mod 10);
let g = g |> f |> f |> f |> f |> f;
Print.print(g, string_of_int);