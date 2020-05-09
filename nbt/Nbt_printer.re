/** print_tag prints the one-byte representation of a tag */
let print_tag = (channel: out_channel, tag: Node.tag): unit => {
  let tag_byte: int = Obj.magic(tag);
  output_byte(channel, tag_byte);
};

/** print_payload_contents prints a payload without its tag  */
let rec print_payload_contents =
        (channel: out_channel, payload: Node.payload): unit =>
  switch (payload) {
  | Byte(int) => output_byte(channel, int)
  | Short(int) =>
    let bytes = Bytes.make(2, Char.chr(0));
    Bytes.set_int16_be(bytes, 0, int);
    output_bytes(channel, bytes);
  | Int(int32) =>
    let bytes = Bytes.make(4, Char.chr(0));
    Bytes.set_int32_be(bytes, 0, int32);
    output_bytes(channel, bytes);
  | Long(int64) =>
    let bytes = Bytes.make(8, Char.chr(0));
    Bytes.set_int64_be(bytes, 0, int64);
    output_bytes(channel, bytes);
  | Float(float) =>
    let bytes = Bytes.make(4, Char.chr(0));
    Int32.bits_of_float(float) |> Bytes.set_int32_be(bytes, 0, _);
    output_bytes(channel, bytes);
  | Double(float) =>
    let bytes = Bytes.make(8, Char.chr(0));
    Int64.bits_of_float(float) |> Bytes.set_int64_be(bytes, 0, _);
    output_bytes(channel, bytes);
  | Byte_array(byte_array) =>
    let size = Bigarray.Array1.dim(byte_array);
    print_payload_contents(channel, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      output_byte(channel, byte_array.{i});
    };
  | String(string) =>
    print_payload_contents(channel, Short(String.length(string)));
    output_string(channel, string);
  | List(list) =>
    let tag = Node.check_list_type(list);
    print_tag(channel, tag);
    print_payload_contents(channel, Int(List.length(list) |> Int32.of_int));
    List.iter(elem => print_payload_contents(channel, elem), list);
  | Compound(list) =>
    List.iter(print_node(channel, _), list);
    print_tag(channel, End_tag);
  | Int_array(int_array) =>
    let size = Bigarray.Array1.dim(int_array);
    print_payload_contents(channel, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      print_payload_contents(channel, Int(int_array.{i}));
    };
  | Long_array(long_array) =>
    let size = Bigarray.Array1.dim(long_array);
    print_payload_contents(channel, Int(Int32.of_int(size)));
    for (i in 0 to pred(size)) {
      print_payload_contents(channel, Long(long_array.{i}));
    };
  }

/** print_node prints the tag, node name, then node payload */
and print_node = (channel: out_channel, node: Node.t): unit => {
  print_tag(channel, Node.tag_of_payload(node.payload));
  print_payload_contents(channel, Short(String.length(node.name)));
  output_string(channel, node.name);
  print_payload_contents(channel, node.payload);
};