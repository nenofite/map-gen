let split_rgb = color => {
  let r = (color land 0xFF0000) lsr 16;
  let g = (color land 0x00FF00) lsr 8;
  let b = color land 0x0000FF;
  (r, g, b);
};

let unsplit_rgb = (r, g, b) => {
  (r land 0xFF) lsl 16 lor (g land 0xFF) lsl 8 lor (b land 0xFF);
};

let blend = (a, b, fraction) => {
  open Mg_util.Floats;

  let (ar, ag, ab) = split_rgb(a);
  let (br, bg, bb) = split_rgb(b);
  let fraction = min(1., max(0., fraction));

  let r = ~~(~.ar *. (1. -. fraction) +. ~.br *. fraction);
  let g = ~~(~.ag *. (1. -. fraction) +. ~.bg *. fraction);
  let b = ~~(~.ab *. (1. -. fraction) +. ~.bb *. fraction);
  unsplit_rgb(r, g, b);
};