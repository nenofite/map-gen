/** tag is a single byte to mark the payload type of a node */
type tag =
  | End_tag
  | Byte_tag
  | Short_tag
  | Int_tag
  | Long_tag
  | Float_tag
  | Double_tag
  | Byte_array_tag
  | String_tag
  | List_tag
  | Compound_tag
  | Int_array_tag
  | Long_array_tag;

type byte_array =
  Bigarray.Array1.t(char, Bigarray.int8_signed_elt, Bigarray.c_layout);
type int_array =
  Bigarray.Array1.t(int32, Bigarray.int32_elt, Bigarray.c_layout);
type long_array =
  Bigarray.Array1.t(int64, Bigarray.int64_elt, Bigarray.c_layout);

/** payload represents both the tag and payload contents */
type payload =
  | Byte(int)
  | Short(int)
  | Int(int32)
  | Long(int64)
  | Float(float)
  | Double(float)
  | Byte_array(byte_array)
  | String(string)
  | List(list(payload)) /* all members must be the same type, but we can enforce that at runtime for now */
  | Compound(list(t))
  | Int_array(int_array)
  | Long_array(long_array)

/** t is a full node with name, tag, and payload */
and t = {
  name: string,
  payload,
};

/**
  Malformed_nbt is raised when an NBT node is invalid in some way, for example
  if a list contains children of different types
 */
exception Malformed_nbt(payload);

let tag_of_payload =
  fun
  | Byte(_) => Byte_tag
  | Short(_) => Short_tag
  | Int(_) => Int_tag
  | Long(_) => Long_tag
  | Float(_) => Float_tag
  | Double(_) => Double_tag
  | Byte_array(_) => Byte_array_tag
  | String(_) => String_tag
  | List(_) => List_tag
  | Compound(_) => Compound_tag
  | Int_array(_) => Int_array_tag
  | Long_array(_) => Long_array_tag;

/** print_tag prints the one-byte representation of a tag */
let print_tag = (channel, tag: tag) => {
  let tag_byte: int = Obj.magic(tag);
  output_byte(channel, tag_byte);
};

/** print_payload_contents prints a payload without its tag  */
let rec print_payload_contents = (channel, payload) =>
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
      output_byte(channel, byte_array.{i} |> int_of_char);
    };
  | String(string) =>
    print_payload_contents(channel, Short(String.length(string)));
    output_string(channel, string);
  | List(list) =>
    let tag =
      switch (list) {
      | [hd, ..._] => tag_of_payload(hd)
      | [] => End_tag
      };
    if (List.exists(elem => tag_of_payload(elem) != tag, list)) {
      raise(Malformed_nbt(payload));
    };
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
and print_node = (channel, node) => {
  print_tag(channel, tag_of_payload(node.payload));
  print_payload_contents(channel, Short(String.length(node.name)));
  output_string(channel, node.name);
  print_payload_contents(channel, node.payload);
};