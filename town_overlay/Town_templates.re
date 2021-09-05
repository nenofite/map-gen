let bedroom_1 =
  Minecraft_template.Txt.parse_template(
    ~palette=
      Minecraft.Block.[
        ("B", Some(Orange_bed(N, Head))),
        ("b", Some(Orange_bed(N, Foot))),
        ("D", Some(Oak_door(N, Upper))),
        ("d", Some(Oak_door(N, Lower))),
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
X - - - - - X
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
