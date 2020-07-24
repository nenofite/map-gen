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
    List.iteri(
      (i, el) => {
        if (i > 0) {
          output_string(channel, ",");
        };
        Printf.fprintf(channel, "%db", el);
      },
      byte_array,
    );
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
    List.iteri(
      (i, el) => {
        if (i > 0) {
          output_string(channel, ",");
        };
        Printf.fprintf(channel, "%ld", el);
      },
      int_array,
    );
    output_string(channel, "]");
  | Long_array(long_array) =>
    output_string(channel, "[I;");
    List.iteri(
      (i, el) => {
        if (i > 0) {
          output_string(channel, ",");
        };
        Printf.fprintf(channel, "%Ldl", el);
      },
      long_array,
    );
    output_string(channel, "]");
  }

/** print_node prints a node in human-readable SNBT format */
and print_node = (channel, node) => {
  Printf.fprintf(channel, "%S: ", node.name);
  print_payload(channel, node.payload);
};

let%expect_test "snbt" = {
  let n =
    Node.(
      "root"
      >: Compound([
           "something" >: Byte_array([1, 2, 3]),
           "else" >: List([String("hello"), String("world")]),
           "many \"things\""
           >: Compound([
                "one" >: Int(1l),
                "two" >: Float(2.),
                "three" >: Double(Float.pi),
              ]),
         ])
    );
  print_node(stdout, n);
  print_newline();

  %expect
  {|
    "root": {
    "something": [B;1b,2b,3b],
    "else": ["hello","world"],
    "many \"things\"": {
    "one": 1,
    "two": 2.000000f,
    "three": 3.141593d
    }
    }
  |};
};