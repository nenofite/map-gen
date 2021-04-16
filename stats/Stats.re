type stat = [
  | `In_buffer
  | `Out_buffer
  | `Region_time
  | `Palette_size
  | `Failed_river_length
  | `River_length
  | `C(string)
];

let file: ref(option(out_channel)) = ref(None);

let init = (): unit => {
  if (Option.is_some(file^)) {
    invalid_arg("Stats.init was already called");
  };
  let f = open_out("stats.csv");
  output_string(f, "Stat,Value\n");
  file := Some(f);
};

let unwrap_file = () => {
  switch (file^) {
  | Some(f) => f
  | None => invalid_arg("Stats.init was not called")
  };
};

let flush = (): unit => {
  let f = unwrap_file();
  flush(f);
};

let finalize = (): unit => {
  let f = unwrap_file();
  close_out(f);
  file := None;
};

let name: stat => string =
  fun
  | `In_buffer => "in buffer (B)"
  | `Out_buffer => "out buffer (B)"
  | `Region_time => "region elapsed time (s)"
  | `Palette_size => "palette size"
  | `Failed_river_length => "failed river length"
  | `River_length => "river length"
  | `C(s) => s;

let record = (stat: stat, value: int): unit => {
  switch (file^) {
  | Some(f) => Printf.fprintf(f, "\"%s\",%d\n", name(stat), value)
  | None => ()
  };
};

let recordf = (stat: stat, value: float): unit => {
  switch (file^) {
  | Some(f) => Printf.fprintf(f, "\"%s\",%f\n", name(stat), value)
  | None => ()
  };
};
