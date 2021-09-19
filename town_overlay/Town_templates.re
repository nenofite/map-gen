open! Core_kernel;
open Town_overlay_i;

let make = t => {template: Minecraft_template.normalize_on_origin(t)};

let house_1 =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("X", Filled(Cobblestone)),
        ("^", Filled(Stairs(Stone_stairs, S))),
        ("=", Filled(Oak_planks)),
        ("i", Filled(Torch)),
        ("s", Filled(Wall_torch(N))),
        ("B", Filled(Orange_bed(N, Head))),
        ("b", Filled(Orange_bed(N, Foot))),
        ("D", Filled(Oak_door(N, Upper))),
        ("d", Filled(Oak_door(N, Lower))),
        ("W", Marked(`Worksite, Filled(Air))),
        ("1", Marked(`Road, Empty)),
        ("V", Marked(`Villager, Filled(Air))),
      ],
    {|
. . . . . . .
. . . . . . .
. . . . . . .
X X X X X X X
X - - - - - X
X - i - - - X
X - - - - - X
X - - - - - X
X - - - - - X
X X X X X X X

. . . . . . .
. . . . . . .
. . . . . . .
X X X X X X X
X = = = = - X
X = = = = - X
X = = = = - X
X = = = = - X
X = = = = ^ X
X X X X X X X

. . . . . . .
. . . . . . .
. . . . . . .
X X X X X X X
X - - - - - X
X - - - - - X
X - - - - - X
X - - - - ^ X
X - - s - X X
X X X X X X X

. . . . . . .
. . . . . . .
. . . . . . .
X X X D X X X
X - - - - - X
X - - - - - X
X - - - - ^ X
X - - - - X X
X - - - - - X
X X X X X X X

. . . . . . .
. . . . . . .
. . . . . . .
X X X d X X X
X - - - - - X
X B - - - ^ X
X b - V - X X
X - - - - - X
X - W - - - X
X X X X X X X

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
    {|

. . . . . . . . . . .
. . . . . . . . . . .
. . . . . . . . . . .
O = = O . . . O = = O
= - - = . . . = - - =
# - - O = = = O - - #
# - - - - - - - - - #
# - - - - - - - - - #
= - - - - - - - - - =
O = # = # # # = # = O

. . . . . . . . . . .
. . . . . . . . . . .
. . . . . . . . . . .
O = = O . . . O = = O
= - - = . . . = - - =
= - - O = D = O - - =
= - - - - - - - - - =
= - - - - - - - - - =
= - - - - - - - - - =
O = = = = = = = = = O

. . . . . . . . . . .
. . . . . . . . . . .
. . . . . . . . . . .
O ~ ~ O . . . O ~ ~ O
I - - I . . . I - - I
I - - O ~ d ~ O - - I
I - - - - - - - - - I
I - - - V - - - - - I
I - - - - W - - - - I
O ~ ~ ~ ~ ~ ~ ~ ~ ~ O

. . . . . 1 . . . . .
. . . . . 1 . . . . .
. . . . . 1 . . . . .
X X X X . 1 . X X X X
X = = X . . . X = = X
X = = X X X X X = = X
X = = = = = = = = = X
X = = = = = = = = = X
X = = = = = = = = = X
X X X X X X X X X X X
    |},
  )
  |> make;

let house_3 = {
  open Minecraft_template;
  open Minecraft_template.Rect;
  let b = rect(~xs=14, ~ys=5, ~zs=21);
  let f = floor(b) |> fill(~material=Cobblestone);
  let c = columns(b) |> fill(~material=Log(Oak_log, Y));
  combine_all([f, c]) |> add_mark(~x=7, ~y=1, ~z=20, ~mark=`Villager) |> make;
};

let houses = [/*house_1, house_2,*/ house_3];