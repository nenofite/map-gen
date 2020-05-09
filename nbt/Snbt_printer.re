/** print_payload prints a payload in human-readable SNBT format */
let rec print_payload = (channel: out_channel, payload: Node.payload): unit =>
  switch (payload) {
  | Byte(int) => Printf.fprintf(channel, "%db", int)
  | Short(int) => Printf.fprintf(channel, "%ds", int)
  | Int(int32) => Printf.fprintf(channel, "%ld", int32)
  | Long(int64) => Printf.fprintf(channel, "%Ldl", int64)
  | Float(float) => Printf.fprintf(channel, "%ff", float)
  | Double(float) => Printf.fprintf(channel, "%fd", float)
  | Byte_array(byte_array) =>
    output_string(channel, "[B;");
    let size = Bigarray.Array1.dim(byte_array);
    for (i in 0 to pred(size)) {
      if (i > 0) {
        output_string(channel, ",");
      };
      Printf.fprintf(channel, "%db", byte_array.{i});
    };
    output_string(channel, "]");
  | String(string) => Printf.fprintf(channel, "%S", string)
  | List(list) =>
    output_string(channel, "[");
    List.iteri(
      (i, elem) => {
        if (i > 0) {
          output_string(channel, ",");
        };
        print_payload(channel, elem);
      },
      list,
    );
    output_string(channel, "]");
  | Compound(list) =>
    output_string(channel, "{\n");
    List.iteri(
      (i, elem) => {
        if (i > 0) {
          output_string(channel, ",\n");
        };
        print_node(channel, elem);
      },
      list,
    );
    output_string(channel, "\n}");
  | Int_array(int_array) =>
    output_string(channel, "[I;");
    let size = Bigarray.Array1.dim(int_array);
    for (i in 0 to pred(size)) {
      if (i > 0) {
        output_string(channel, ",");
      };
      Printf.fprintf(channel, "%ld", int_array.{i});
    };
    output_string(channel, "]");
  | Long_array(long_array) =>
    output_string(channel, "[I;");
    let size = Bigarray.Array1.dim(long_array);
    for (i in 0 to pred(size)) {
      if (i > 0) {
        output_string(channel, ",");
      };
      Printf.fprintf(channel, "%Ldl", long_array.{i});
    };
    output_string(channel, "]");
  }

/** print_node prints a node in human-readable SNBT format */
and print_node = (channel, node) => {
  Printf.fprintf(channel, "%S: ", node.name);
  print_payload(channel, node.payload);
};

let test = () => {
  let n =
    Node.{
      name: "root",
      payload:
        Compound([
          "something" >: make_byte_array([|1, 2, 3|]),
          "else" >: List([String("hello"), String("world")]),
        ]),
    };
  print_node(stdout, n);
  print_newline();
};