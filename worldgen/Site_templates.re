let cavern_entrance =
  Template_txt.parse_template(
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
- - X X X X X - -
- - X X # X X - -
- - X # # # X - -
- - X X # X X - -
- - X X X X X - -
- - - - - - - - -
X - - - - - - - X

X - - - - - - - X
- - - - - - - - -
- - X w - e X - -
- - n - - - n - -
- - - - - - - - -
- - s - - - s - -
- - X w - e X - -
- - - - - - - - -
X - - - - - - - X

X X - - - - - X X
X - - - - - - - X
- - X - - - X - -
- - - - - - - - -
- - - - - - - - -
- - - - - - - - -
- - X - - - X - -
X - - - - - - - X
X X - - - - - X X

X X X X X X X X X
X X X X X X X X X
X X X X X X X X X
X X X X > - X X X
X X X - - - X X X
X X X - - - X X X
X X X X X X X X X
X X X X X X X X X
X X X X X X X X X
|},
  );

let cavern_entrance_tube =
  Template_txt.parse_template(
    {|
. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - X X . .
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
. . X - < X X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - - X . .
. . X ^ - e X . .
. . X X - - X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X X > - X . .
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
        ("N", Some(Stone_stairs(Nd))),
        ("E", Some(Stone_stairs(Ed))),
        ("S", Some(Stone_stairs(Sd))),
        ("W", Some(Stone_stairs(Wd))),
      ],
    {|
. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X - - X X . .
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
. . S - < X S . .
. . X W - E X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X - - - X . .
. . - - - - - . .
. . - ^ - - - . .
. . - X - - - . .
. . X - - - X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X - i - X . .
. . - X > - - . .
. . i - - - i . .
. . - - - - - . .
. . X - i - X . .
. . . . . . . . .
. . . . . . . . .

. . . . . . . . .
. . . . . . . . .
. . X X X X X . .
. . X X X X X . .
. . X X X X X . .
. . X X X X X . .
. . X X X X X . .
. . . . . . . . .
. . . . . . . . .
|},
  );