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