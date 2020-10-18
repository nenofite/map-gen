let bedroom_1 =
  Template_txt.parse_template(
    ~palette=
      Minecraft.Block.
        [
          /* TODO beds */
          ("B", Some(Stone)),
          ("b", Some(Stone)),
        ],
        /* ("B", Some(Bed(N, Head))),
           ("b", Some(Bed(N, Foot))), */
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

X X X D X X X
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