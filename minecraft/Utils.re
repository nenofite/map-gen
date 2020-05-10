let time_ms = () => Int64.of_float(Unix.gettimeofday() *. 1000.);

let write_file = (path, f) => {
  print_string("Writing file " ++ path ++ "...");
  flush(stdout);
  let file = open_out(path);
  f(file);
  close_out(file);
  print_endline(" done.");
};

/** mkdir creates all directories in path, using the shell command [mkdir -p <path>] */
let mkdir = (path: string): unit => {
  Unix.system("mkdir -p " ++ path) |> ignore;
};

/** output_int32_be writes an int without allocating any buffers */
let output_int32_be = (f: out_channel, i: int32): unit => {
  output_byte(f, Int32.(logand(i, 0xFFl) |> to_int));
  output_byte(f, Int32.(shift_right(i, 8) |> logand(_, 0xFFl) |> to_int));
  output_byte(f, Int32.(shift_right(i, 16) |> logand(_, 0xFFl) |> to_int));
  output_byte(f, Int32.(shift_right(i, 24) |> logand(_, 0xFFl) |> to_int));
};

/** output_int64_be writes an int without allocating any buffers */
let output_int64_be = (f: out_channel, i: int64): unit => {
  output_byte(f, Int64.(logand(i, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 8) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 16) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 24) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 32) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 40) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 48) |> logand(_, 0xFFL) |> to_int));
  output_byte(f, Int64.(shift_right(i, 56) |> logand(_, 0xFFL) |> to_int));
};