type color = {
  r: int,
  g: int,
  b: int,
};

let color_invariant = color => {
  let {r, g, b} = color;
  assert(0 <= r && r <= 255 && 0 <= g && g <= 255 && 0 <= b && b <= 255);
  color;
};

let color_of_int = color => {
  let r = (color land 0xFF0000) lsr 16;
  let g = (color land 0x00FF00) lsr 8;
  let b = color land 0x0000FF;
  color_invariant({r, g, b});
};

let int_of_color = ({r, g, b}) => {
  (r land 0xFF) lsl 16 lor (g land 0xFF) lsl 8 lor (b land 0xFF);
};

let blend = (a, b, fraction) => {
  let fraction = min(1., max(0., fraction));
  let r =
    int_of_float(
      float_of_int(a.r) *. (1. -. fraction) +. float_of_int(b.r) *. fraction,
    );
  let g =
    int_of_float(
      float_of_int(a.g) *. (1. -. fraction) +. float_of_int(b.g) *. fraction,
    );
  let b =
    int_of_float(
      float_of_int(a.b) *. (1. -. fraction) +. float_of_int(b.b) *. fraction,
    );
  color_invariant({r, g, b});
};