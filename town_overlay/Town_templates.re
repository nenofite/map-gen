open! Core;
open Town_overlay_i;

let make = t => {template: Minecraft_template.normalize_on_origin(t)};

let house_1 =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("v", Filled(Stairs(Oak_stairs, S))),
        ("=", Filled(Oak_planks)),
        ("i", Filled(Torch)),
        ("s", Filled(Wall_torch(N))),
        ("B", Filled(Orange_bed(N, Head))),
        ("b", Filled(Orange_bed(N, Foot))),
        ("D", Filled(Oak_door(N, Upper))),
        ("d", Filled(Oak_door(N, Lower))),
        ("~", Filled(Log(Oak_log, X))),
        ("O", Filled(Log(Oak_log, Y))),
        ("I", Filled(Log(Oak_log, Z))),
        ("#", Filled(Stone_bricks)),
        ("W", Marked(`Worksite, Filled(Air))),
        ("1", Marked(`Road, Empty)),
        ("V", Marked(`Villager, Filled(Air))),
      ],
    {|
. . . . . . .
. . . . . . .
. . . . . . .
O = = = = = O
= - - - - - =
= - i - - - =
= - - - - - =
= - - - - - =
= - - - - - =
O = = = = = O

. . . . . . .
. . . . . . .
. . . . . . .
O ~ ~ ~ ~ ~ O
I = = = = - I
I = = = = - I
I = = = = - I
I = = = = - I
I = = = = v I
O ~ ~ ~ ~ ~ O

. . . . . . .
. . . . . . .
. . . . . . .
O = # # # = O
~ - - - - - ~
= - - - - - =
= - - - - - =
= - - - - v =
~ - - s - - ~
O I = = = I O

. . . . . . .
. . . . . . .
. . . . . . .
O = # D # = O
= - - - - - =
= - - - - - =
= - - - - v =
= - - - - - =
= - - - - - =
O = = = = = O

. . . . . . .
. . . . . . .
. . . . . . .
O X # d # X O
X - - - - - X
X B - - - v X
X b - V - - X
X - - - - - X
X - W - - - X
O X X X X X O

. . . 1 . . .
. . . 1 . . .
. . . . . . .
X X X X X X X
X = = = = = X
X = = = = = X
X = = = = = X
X = = = = = X
X = = = = = X
X X X X X X X
    |},
  )
  |> make;

let house_2 =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("=", Filled(Oak_planks)),
        ("^", Filled(Stairs(Stone_stairs, N))),
        (">", Filled(Stairs(Stone_stairs, E))),
        ("v", Filled(Stairs(Stone_stairs, S))),
        ("i", Filled(Torch)),
        ("B", Filled(Orange_bed(N, Head))),
        ("b", Filled(Orange_bed(N, Foot))),
        ("D", Filled(Oak_door(N, Upper))),
        ("d", Filled(Oak_door(N, Lower))),
        ("#", Filled(Glass)),
        ("O", Filled(Log(Oak_log, Y))),
        ("~", Filled(Log(Oak_log, X))),
        ("I", Filled(Log(Oak_log, Z))),
        ("W", Marked(`Worksite, Filled(Air))),
        ("1", Marked(`Road, Empty)),
        ("V", Marked(`Villager, Filled(Air))),
      ],
    ~has_spaces=false,
    {|

...........
...........
...........
I==I...I==I
I==I...I==I
~~~~~~~~~~~
I====I====I
I====I====I
I====I====I
~~~~~~~~~~~

...........
...........
...........
O==O...O==O
=--=...=--=
#--O===O--#
#---------#
#---------#
=---------=
O=#=###=#=O

...........
...........
...........
O==O...O==O
=--=...=--=
#--O===O--#
#---------#
#---------#
=---------=
O=#=###=#=O

...........
...........
...........
O==O...O==O
=--I...I===
#--I===I==#
#--I===I==#
#--I===I==#
=->I===I===
O=#I###I#=O

...........
...........
...........
O==O...O==O
====...=--=
#=^O===O--#
#=--------#
#=--------#
==--------=
O=#=###=#=O

...........
...........
...........
O==O...O==O
====...=--=
===O=D=O--=
==^-------=
==--------=
==--------=
O=========O

...........
...........
...........
O~~O...O~~O
I==I...I-BI
I==O~d~O-bI
I==-------I
I=^----V--I
I=---W----I
O~~~~~~~~~O

.....1.....
.....1.....
.....1.....
XXXX.1.XXXX
X==X...X==X
X==XXXXX==X
X=========X
X=========X
X=========X
XXXXXXXXXXX
    |},
  )
  |> make;

let house_3 =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("=", Filled(Oak_planks)),
        ("^", Filled(Stairs(Oak_stairs, N))),
        (">", Filled(Stairs(Oak_stairs, E))),
        ("v", Filled(Stairs(Oak_stairs, S))),
        ("i", Filled(Torch)),
        ("B", Filled(Orange_bed(W, Head))),
        ("b", Filled(Orange_bed(W, Foot))),
        ("D", Filled(Oak_door(N, Upper))),
        ("d", Filled(Oak_door(N, Lower))),
        ("#", Filled(Glass)),
        ("O", Filled(Log(Oak_log, Y))),
        ("~", Filled(Log(Oak_log, X))),
        ("I", Filled(Log(Oak_log, Z))),
        ("W", Marked(`Worksite, Filled(Air))),
        ("1", Marked(`Road, Empty)),
        ("V", Marked(`Villager, Filled(Air))),
      ],
    ~has_spaces=false,
    {|

I=====I=====I=====I=====I
I=====I=====I=====I=====I
I=====I=====I=====I=====I
~~~~~~~~~~~~~~~~~~~~~~~~~
I=====I=====I=====I=====I
I=====I=====I=====I=====I
I=====I=====I=====I=====I

O=====O=====O=====O=====O
=-----=-----------=-----=
=-----=-----------=-----=
=-----=-----------=-----=
=-----=-----------=-----=
=-----------------=-----=
O=====O=====O=====O=====O

O=====O=====O=====O=====O
=-----=-----------=-----=
=-----=-----------=-----=
=-----=-----------=-----=
=-----=-----------=-----=
=-----------------=-----=
O=====O=====O=====O=====O

I=====I=====O=====I=====I
I=====I-----------I====-I
I=====I-----------I====-I
I=====I-----------I====-I
I=====I-----------I====vI
I=====Iv----------I=====I
I=====I=====O=====I=====I

O=====O====DO=====O=====O
=-----=-----------=-----=
=-----------------=-----=
=-----=----------------v=
=-----=v----------=-----=
=-----=-----------=-----=
O=====O====DO=====O=====O

XXXXXXXXXXXdXXXXXXXXXXXXX
X-----=-----------=-----X
X-----------------=----vX
X-----=v---VW-----------X
X-----=-----------=-----X
XBb---=-----------=-----X
XXXXXXXXXXXdXXXXXXXXXXXXX

XXXXXXXXXXXXXXXXXXXXXXXXX
X=====XXXXXXXXXXXXX=====X
X=====XXXXXXXXXXXXX=====X
X=====XXXXXXXXXXXXX=====X
X=====XXXXXXXXXXXXX=====X
X=====XXXXXXXXXXXXX=====X
XXXXXXXXXXXXXXXXXXXXXXXXX
    |},
  )
  |> make;

let houses = [house_1, house_2, house_3];
