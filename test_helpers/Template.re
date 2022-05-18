open! Core;
open Text_grid;

let show_template_top_down =
    (~show_block, ~resolve_multi=List.hd_exn, t: Minecraft_template.t(_))
    : text_grid => {
  let side =
    max(Minecraft_template.x_size_of(t), Minecraft_template.z_size_of(t));
  let (min_x, _) = Minecraft_template.x_bounds_of(t);
  let (min_z, _) = Minecraft_template.z_bounds_of(t);
  let top_down_at = (~x, ~z) => {
    let x = x + min_x;
    let z = z + min_z;
    let matches =
      Minecraft_template.find_blocks(t, ~f=((bx, _by, bz, _)) =>
        x == bx && z == bz
      )
      |> List.sort(~compare=((_, ay, _, _), (_, by, _, _))
           // Reverse compare so the highest y comes first
           => Int.compare(by, ay))
      |> List.filter_map(~f=((_, _, _, b)) => show_block(b));
    switch (matches) {
    | [] => " "
    | [single] => single
    | multi => resolve_multi(multi)
    };
  };
  Grid.Mut.init(~side, ~f=top_down_at, "");
};

let show_template_south_north =
    (~show_block, ~resolve_multi=List.hd_exn, t: Minecraft_template.t(_))
    : text_grid => {
  let side =
    max(Minecraft_template.x_size_of(t), Minecraft_template.y_size_of(t));
  let (min_x, _) = Minecraft_template.x_bounds_of(t);
  let (min_y, _) = Minecraft_template.y_bounds_of(t);
  let south_north_at = (~x, ~z as y) => {
    let x = x + min_x;
    let y = side - y - min_y - 1;
    let matches =
      Minecraft_template.find_blocks(t, ~f=((bx, by, _bz, _)) =>
        x == bx && y == by
      )
      |> List.sort(~compare=((_, _, az, _), (_, _, bz, _))
           // Reverse compare so the highest z (most south) comes first
           => Int.compare(bz, az))
      |> List.filter_map(~f=((_, _, _, b)) => show_block(b));
    switch (matches) {
    | [] => " "
    | [single] => single
    | multi => resolve_multi(multi)
    };
  };
  Grid.Mut.init(~side, ~f=south_north_at, "");
};