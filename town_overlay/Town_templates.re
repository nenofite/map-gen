open! Core_kernel;
open Town_overlay_i;

let bedroom_1_t =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft_template.Txt.Palette_incl.[
        ("B", Filled(Orange_bed(N, Head))),
        ("b", Filled(Orange_bed(N, Foot))),
        ("D", Filled(Oak_door(N, Upper))),
        ("d", Filled(Oak_door(N, Lower))),
        ("W", Marked(`Worksite, Filled(Air))),
      ],
    {|
X X X X X X X
X - - - - - X
X - i - - - X
X - - - - - X
X - - - - - X
X - - - - - X
X X X X X X X

X X X X X X X
X = = = = - X
X = = = = - X
X = = = = - X
X = = = = - X
X = = = = ^ X
X X X X X X X

X X X X X X X
X - - - - - X
X - - - - - X
X - - - - - X
X - - - - ^ X
X - - s - X X
X X X X X X X

X X X D X X X
X - - - - - X
X - - - - - X
X - - - - ^ X
X - - - - X X
X - - - - - X
X X X X X X X

X X X d X X X
X - - - - - X
X B - - - ^ X
X b - - - X X
X - - - - - X
X - W - - - X
X X X X X X X

X X X X X X X
X = = = = = X
X = = = = = X
X = = = = = X
X = = = = = X
X = = = = = X
X X X X X X X
    |},
  );
let bedroom_1 = {template: bedroom_1_t};

let houses = [bedroom_1];