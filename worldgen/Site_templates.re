let cavern_entrance =
  Template_txt.parse_template(
    ~palette=
      Minecraft.Block.[
        ("Q", Some(Smooth_stone)),
        ("O", Some(Stone_bricks)),
        ("U", Some(Chiseled_stone_bricks)),
        ("v", Some(Stairs(Stone_brick_stairs, N))),
        ("<", Some(Stairs(Stone_brick_stairs, E))),
        ("^", Some(Stairs(Stone_brick_stairs, S))),
        (">", Some(Stairs(Stone_brick_stairs, W))),
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
        ("v", Some(Stairs(Stone_brick_stairs, N))),
        ("<", Some(Stairs(Stone_brick_stairs, E))),
        ("^", Some(Stairs(Stone_brick_stairs, S))),
        (">", Some(Stairs(Stone_brick_stairs, W))),
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
        ("N", Some(Stairs(Cobblestone_stairs, Nd))),
        ("E", Some(Stairs(Cobblestone_stairs, Ed))),
        ("S", Some(Stairs(Cobblestone_stairs, Sd))),
        ("W", Some(Stairs(Cobblestone_stairs, Wd))),
        ("Q", Some(Smooth_stone)),
        ("O", Some(Stone_bricks)),
        ("U", Some(Chiseled_stone_bricks)),
        ("v", Some(Stairs(Stone_brick_stairs, N))),
        ("<", Some(Stairs(Stone_brick_stairs, E))),
        ("^", Some(Stairs(Stone_brick_stairs, S))),
        (">", Some(Stairs(Stone_brick_stairs, W))),
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
