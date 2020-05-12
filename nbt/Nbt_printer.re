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

type big_string =
  Bigarray.Array1.t(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout);

/**
  nbt_printer_memory contains the large allocated objects which can be re-used
  between calls to print_nbt in order to reduce GC pressure. If calling
  print_nbt in a hot loop, consider allocating memory once with
  [create_memory()]
 */
type nbt_printer_memory = {
  input_buffer: Buffer.t,
  zlib_gzip: Zlib.t(Zlib.deflate),
  zlib_deflate: Zlib.t(Zlib.deflate),
};

let create_memory = () => {
  input_buffer: Buffer.create(16),
  zlib_gzip: Zlib.create_deflate(~window_bits=15 + 16, ()),
  zlib_deflate: Zlib.create_deflate(~window_bits=15, ()),
};

/**
  print_nbt prints a full NBT file, Gzips it, and returns the bytes

  If gzip is false, the output is deflated rather than gzip'd
 */
let print_nbt =
    (~memory=create_memory(), ~gzip=true, node: Node.t): big_string => {
  let {input_buffer, zlib_gzip, zlib_deflate} = memory;

  /* Print uncompressed NBT to a buffer */
  Buffer.clear(input_buffer);
  print_node(input_buffer, node);

  /* Buffer -> Bigarray */
  let input_length = Buffer.length(input_buffer);
  let input = Bigarray.(Array1.create(Char, C_layout, input_length));
  for (i in 0 to pred(input_length)) {
    input.{i} = Buffer.nth(input_buffer, i);
  };

  /*
     Run Zlib. window_bits > 15 to ask it to produce Gzip headers, negative
     window_bits to ask it to do a raw deflate
   */
  let zlib = if (gzip) {zlib_gzip} else {zlib_deflate};
  Zlib.reset(zlib);
  /*
    Zlib.reset seems broken/misdocumented, we have to manually reset the
    mutable fields. These match the initial state
   */
  zlib.in_ofs = 0;
  zlib.out_ofs = 0;
  zlib.in_len = (-1);
  zlib.out_len = (-1);
  zlib.in_total = 0;
  zlib.out_total = 0;
  zlib.data_type = 2;
  zlib.cksum = 0l;
  /* Add 1KB breathing room for Gzip header--should be more than enough */
  let output_upper_bound =
    Zlib.deflate_bound(zlib.state, input_length) + 1024;
  zlib.in_buf = input;
  zlib.out_buf = Bigarray.(Array1.create(Char, C_layout, output_upper_bound));
  let status = Zlib.(flate(zlib, Finish));
  assert(status == Zlib.Stream_end);

  /* Trim output */
  Bigarray.Array1.sub(zlib.out_buf, 0, zlib.out_total);
};

/** print_nbt_f runs print_nbt and outputs the result */
let print_nbt_f = (~gzip=?, f: out_channel, node: Node.t): unit => {
  let bigstr = print_nbt(~gzip?, node);
  for (i in 0 to pred(Bigarray.Array1.dim(bigstr))) {
    output_char(f, bigstr.{i});
  };
};

/** test prints every possible tag type to make sure it works */
let test = () => {
  let n =
    Node.(
      ""
      >: Compound([
           "compound"
           >: Compound([
                "byte" >: Byte(37),
                "short" >: Short(427),
                "int" >: Int(3030l),
                "long" >: Long(12345L),
                "float" >: Float(1.3),
                "double" >: Double(1.4),
                "byte array" >: make_byte_array([|1, 2, 3|]),
                "string" >: String("hello world"),
                "list" >: List([String("a"), String("b"), String("c")]),
                "int array" >: make_int_array([|4l, 5l, 6l|]),
                "long array" >: make_long_array([|7L, 8L, 9L|]),
                "empty list" >: List([]),
              ]),
         ])
    );
  let f = open_out("test.nbt");
  print_nbt_f(f, n);
  close_out(f);
};