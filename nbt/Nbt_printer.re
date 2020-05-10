/** print_tag prints the one-byte representation of a tag */
let print_tag = (buffer: Buffer.t, tag: Node.tag): unit => {
  let tag_byte: int = Obj.magic(tag);
  Buffer.add_int8(buffer, tag_byte);
};

/** print_payload_contents prints a payload without its tag  */
let rec print_payload_contents =
        (buffer: Buffer.t, payload: Node.payload): unit =>
  switch (payload) {
  | Byte(int) => Buffer.add_int8(buffer, int)
  | Short(int) => Buffer.add_int16_be(buffer, int)
  | Int(int32) => Buffer.add_int32_be(buffer, int32)
  | Long(int64) => Buffer.add_int64_be(buffer, int64)
  | Float(float) => Buffer.add_int32_be(buffer, Int32.bits_of_float(float))
  | Double(float) => Buffer.add_int64_be(buffer, Int64.bits_of_float(float))
  | Byte_array(byte_array) =>
    let size = Bigarray.Array1.dim(byte_array);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      Buffer.add_int8(buffer, byte_array.{i});
    };
  | String(string) =>
    print_payload_contents(buffer, Short(String.length(string)));
    Buffer.add_string(buffer, string);
  | List(list) =>
    let tag = Node.check_list_type(list);
    print_tag(buffer, tag);
    print_payload_contents(buffer, Int(List.length(list) |> Int32.of_int));
    List.iter(elem => print_payload_contents(buffer, elem), list);
  | Compound(list) =>
    List.iter(print_node(buffer, _), list);
    print_tag(buffer, End_tag);
  | Int_array(int_array) =>
    let size = Bigarray.Array1.dim(int_array);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      print_payload_contents(buffer, Int(int_array.{i}));
    };
  | Long_array(long_array) =>
    let size = Bigarray.Array1.dim(long_array);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      print_payload_contents(buffer, Long(long_array.{i}));
    };
  }

/** print_node prints the tag, node name, then node payload */
and print_node = (buffer: Buffer.t, node: Node.t): unit => {
  print_tag(buffer, Node.tag_of_payload(node.payload));
  print_payload_contents(buffer, Short(String.length(node.name)));
  Buffer.add_string(buffer, node.name);
  print_payload_contents(buffer, node.payload);
};

/** print_node_f is a convenience over print_node which handles the Buffer for you */
let print_node_f = (file: out_channel, node: Node.t): unit => {
  let buffer = Buffer.create(16);
  print_node(buffer, node);
  Buffer.output_buffer(file, buffer);
};