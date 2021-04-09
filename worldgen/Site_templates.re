let cavern_entrance =
  Template_txt.parse_template(
    ~palette=
      Minecraft.Block.[
        ("Q", Some(Smooth_stone)),
        ("O", Some(Stone_bricks)),
        ("U", Some(Chiseled_stone_bricks)),
        ("v", Some(Stone_brick_stairs(N))),
        ("<", Some(Stone_brick_stairs(E))),
        ("^", Some(Stone_brick_stairs(S))),
        (">", Some(Stone_brick_stairs(W))),
      ],
    {|
X - - - - - - - X
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
X - - - - - - - X

X - - - - - - - X
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
X - - - - - - - X

X - - - - - - - X
- - - - - - - - -
- - O O O O O - -
- - O U # U O - -
- - O # # # O - -
- - O U # U O - -
- - O O O O O - -
- - - - - - - - -
X - - - - - - - X

X - - - - - - - X
- - - - - - - - -
- - O w - e O - -
- - n - - - n - -
- - - - - - - - -
- - s - - - s - -
- - O w - e O - -
- - - - - - - - -
X - - - - - - - X

X X - - - - - X X
X - - - - - - - X
- - O - - - O - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - O - - - O - -
X - - - - - - - X
X X - - - - - X X

X X X X X X X X X
X X X X X X X X X
X X O O O O O X X
X X O O > - O X X
X X O - - - O X X
X X O - - - O X X
X X O O O O O X X
X X X X X X X X X
X X X X X X X X X
|},
  );

let cavern_entrance_tube =
  Template_txt.parse_template(
    ~palette=
      Minecraft.Block.[
        ("Q", Some(Smooth_stone)),
        ("O", Some(Stone_bricks)),
        ("U", Some(Chiseled_stone_bricks)),
        ("v", Some(Stone_brick_stairs(N))),
        ("<", Some(Stone_brick_stairs(E))),
        ("^", Some(Stone_brick_stairs(S))),
        (">", Some(Stone_brick_stairs(W))),
      ],
    {|
. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - O X . .
. . X w - v X . .
. . X - - - X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - n - X . .
. . X - - - X . .
. . X - < O X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - - X . .
. . X ^ - e X . .
. . X O - - X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X O > - X . .
. . X - - - X . .
. . X - s - X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .
|},
  );

let cavern_entrance_base =
  Template_txt.parse_template(
    ~palette=
      Minecraft.Block.[
        ("N", Some(Cobblestone_stairs(Nd))),
        ("E", Some(Cobblestone_stairs(Ed))),
        ("S", Some(Cobblestone_stairs(Sd))),
        ("W", Some(Cobblestone_stairs(Wd))),
        ("Q", Some(Smooth_stone)),
        ("O", Some(Stone_bricks)),
        ("U", Some(Chiseled_stone_bricks)),
        ("v", Some(Stone_brick_stairs(N))),
        ("<", Some(Stone_brick_stairs(E))),
        ("^", Some(Stone_brick_stairs(S))),
        (">", Some(Stone_brick_stairs(W))),
      ],
    {|
. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - O X . .
. . X - - v X . .
. . X - - - X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X W - E X . .
. . N - - - N . .
. . - - - - - . .
. . S - < O S . .
. . X W - E X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X - - - X . .
. . - - - - - . .
. . - ^ - - - . .
. . - O - - - . .
. . X - - - X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X - i - X . .
. . - O > - - . .
. . i - - - i . .
. . - - - - - . .
. . X - i - X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X O O O X . .
. . X O O O X . .
. . X O O O X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .
|},
  );

let gate = (~thickness, ~width, ~height, ~rotation) => {
  open Minecraft_template;
  let mat = Minecraft.Block.Obsidian;
  let base =
    rect(mat, ~xs=width, ~ys=thickness, ~zs=thickness)
    |> align_with_origin(~my=(X(min), Y(max), Z(min)));
  let post = rect(mat, ~xs=thickness, ~ys=height, ~zs=thickness);
  let left_post =
    post
    |> align_with(
         ~other=base,
         ~my=(X(min), Y(min), Z(min)),
         ~their=(X(min), Y(max), Z(min)),
       );
  let right_post =
    post
    |> align_with(
         ~other=base,
         ~my=(X(max), Y(min), Z(min)),
         ~their=(X(max), Y(max), Z(min)),
       );
  let top =
    rect(mat, ~xs=width, ~ys=thickness, ~zs=thickness)
    |> align_with(
         ~other=left_post,
         ~my=(X(min), Y(min), Z(min)),
         ~their=(X(min), Y(max), Z(min)),
       );
  let whole = combine_all([base, left_post, right_post, top]);
  rotate_90_cw(whole, ~times=rotation);
};
