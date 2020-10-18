type t = {
  longs: list(int64),
  remaining_bits: int,
};

let empty = {longs: [], remaining_bits: 0};

let packed_longs = (t: t) => List.rev(t.longs);

let rec add_bits = (value: int64, ~bits: int, t: t) => {
  let {longs, remaining_bits} = t;
  switch (bits, remaining_bits, longs) {
  | (0, _, _) => t
  | (bits, 0, longs) =>
    add_bits(value, ~bits, {remaining_bits: 64, longs: [0L, ...longs]})
  | (bits, remaining_bits, [head, ...longs]) =>
    let leftover_bits = max(0, bits - remaining_bits);
    let added = {
      let shift = 64 - remaining_bits;
      let head = Base.Int64.(value lsl shift lor head);
      {
        remaining_bits: max(0, remaining_bits - bits),
        longs: [head, ...longs],
      };
    };
    if (leftover_bits > 0) {
      let leftover_value = Base.Int64.(value lsr remaining_bits);
      add_bits(leftover_value, ~bits=leftover_bits, added);
    } else {
      added;
    };
  | (_, remaining_bits, []) =>
    failwith(
      Printf.sprintf("longs=[] but remaining_bits=%d", remaining_bits),
    )
  };
};