let cavern_entrance =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("#", Filled(Glass)),
        ("s", Filled(Wall_torch(N))),
        ("w", Filled(Wall_torch(E))),
        ("n", Filled(Wall_torch(S))),
        ("e", Filled(Wall_torch(W))),
        ("i", Filled(Torch)),
        ("Q", Filled(Smooth_stone)),
        ("O", Filled(Stone_bricks)),
        ("U", Filled(Chiseled_stone_bricks)),
        ("v", Filled(Stairs(Stone_brick_stairs, N))),
        ("<", Filled(Stairs(Stone_brick_stairs, E))),
        ("^", Filled(Stairs(Stone_brick_stairs, S))),
        (">", Filled(Stairs(Stone_brick_stairs, W))),
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
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("#", Filled(Glass)),
        ("s", Filled(Wall_torch(N))),
        ("w", Filled(Wall_torch(E))),
        ("n", Filled(Wall_torch(S))),
        ("e", Filled(Wall_torch(W))),
        ("i", Filled(Torch)),
        ("Q", Filled(Smooth_stone)),
        ("O", Filled(Stone_bricks)),
        ("U", Filled(Chiseled_stone_bricks)),
        ("v", Filled(Stairs(Stone_brick_stairs, N))),
        ("<", Filled(Stairs(Stone_brick_stairs, E))),
        ("^", Filled(Stairs(Stone_brick_stairs, S))),
        (">", Filled(Stairs(Stone_brick_stairs, W))),
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
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("#", Filled(Glass)),
        ("s", Filled(Wall_torch(N))),
        ("w", Filled(Wall_torch(E))),
        ("n", Filled(Wall_torch(S))),
        ("e", Filled(Wall_torch(W))),
        ("i", Filled(Torch)),
        ("N", Filled(Stairs(Cobblestone_stairs, Nd))),
        ("E", Filled(Stairs(Cobblestone_stairs, Ed))),
        ("S", Filled(Stairs(Cobblestone_stairs, Sd))),
        ("W", Filled(Stairs(Cobblestone_stairs, Wd))),
        ("Q", Filled(Smooth_stone)),
        ("O", Filled(Stone_bricks)),
        ("U", Filled(Chiseled_stone_bricks)),
        ("v", Filled(Stairs(Stone_brick_stairs, N))),
        ("<", Filled(Stairs(Stone_brick_stairs, E))),
        ("^", Filled(Stairs(Stone_brick_stairs, S))),
        (">", Filled(Stairs(Stone_brick_stairs, W))),
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
