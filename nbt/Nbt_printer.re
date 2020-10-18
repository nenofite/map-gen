/** print_tag prints the one-byte representation of a tag */
let print_tag = (buffer: Buffer.t, tag: Node.tag): unit => {
  let tag_byte =
    switch (tag) {
    | End_tag => 0
    | Byte_tag => 1
    | Short_tag => 2
    | Int_tag => 3
    | Long_tag => 4
    | Float_tag => 5
    | Double_tag => 6
    | Byte_array_tag => 7
    | String_tag => 8
    | List_tag => 9
    | Compound_tag => 10
    | Int_array_tag => 11
    | Long_array_tag => 12
    };
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
  | Byte_array(bytes) =>
    let size = List.length(bytes);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    List.iter(b => Buffer.add_int8(buffer, b), bytes);
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
  | Int_array(ints) =>
    let size = List.length(ints);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    List.iter(i => print_payload_contents(buffer, Int(i)), ints);
  | Long_array(longs) =>
    let size = List.length(longs);
    print_payload_contents(buffer, Int(Int32.of_int(size)));
    List.iter(l => print_payload_contents(buffer, Long(l)), longs);
  }

/** print_node prints the tag, node name, then node payload */
and print_node = (buffer: Buffer.t, node: Node.t): unit => {
  print_tag(buffer, Node.tag_of_payload(node.payload));
  print_payload_contents(buffer, Short(String.length(node.name)));
  Buffer.add_string(buffer, node.name);
  print_payload_contents(buffer, node.payload);
};

type bigstring =
  Bigarray.Array1.t(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout);

/**
  nbt_printer_memory contains the large allocated objects which can be re-used
  between calls to print_nbt in order to reduce GC pressure. If calling
  print_nbt in a hot loop, consider allocating memory once with
  [create_memory()]
 */
type nbt_printer_memory = {
  buffer: Buffer.t,
  window: De.window,
  queue: De.Queue.t,
  input_bs: bigstring,
  output_bs: bigstring,
};

let buffer_size = 128 * 1024;
let create_memory = () => {
  buffer: Buffer.create(buffer_size),
  window: De.make_window(~bits=15),
  queue: De.Queue.create(4096),
  input_bs: De.bigstring_create(buffer_size),
  output_bs: De.bigstring_create(buffer_size),
};

/**
  print_nbt prints a full NBT file, Gzips it, and returns the bytes

  The returned buffer is `memory.buffer`, so be sure to copy out the contents
  before calling print_nbt again with the same memory.

  If gzip is false, the output is deflated rather than gzip'd
 */
let print_nbt = (~memory=create_memory(), ~gzip=true, node: Node.t): Buffer.t => {
  let {buffer, window, queue, input_bs, output_bs} = memory;

  /* Print uncompressed NBT to a buffer */
  Buffer.clear(buffer);
  print_node(buffer, node);

  /* Buffer -> Bigarray */
  let input_length = Buffer.length(buffer);
  Stats.record(`In_buffer, input_length);
  for (i in 0 to pred(input_length)) {
    input_bs.{i} = Buffer.nth(buffer, i);
  };

  /* Clear the buffer to prepare for output */
  Buffer.clear(buffer);

  /* Run Decompress */
  let consumed_i = ref(false);
  let refill = _i =>
    if (! consumed_i^) {
      consumed_i := true;
      input_length;
    } else {
      0;
    };
  let flush = (o, out_length) => {
    for (i in 0 to pred(out_length)) {
      Buffer.add_char(buffer, o.{i});
    };
  };
  if (gzip) {
    let config = Gz.Higher.configuration(Gz.Unix, () => 1l);
    Gz.Higher.compress(
      ~level=0,
      ~w=window,
      ~q=queue,
      ~i=input_bs,
      ~o=output_bs,
      ~refill,
      ~flush,
      (),
      config,
    );
  } else {
    Zl.Higher.compress(
      ~level=0,
      ~w=window,
      ~q=queue,
      ~i=input_bs,
      ~o=output_bs,
      ~refill,
      ~flush,
    );
  };

  Stats.record(`Out_buffer, Buffer.length(buffer));
  buffer;
};

/** print_nbt_f runs print_nbt and outputs the result */
let print_nbt_f = (~memory=?, ~gzip=?, f: out_channel, node: Node.t): unit => {
  let buffer = print_nbt(~memory?, ~gzip?, node);
  Buffer.output_buffer(f, buffer);
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
                "byte array" >: Byte_array([1, 2, 3]),
                "string" >: String("hello world"),
                "list" >: List([String("a"), String("b"), String("c")]),
                "int array" >: Int_array([4l, 5l, 6l]),
                "long array" >: Long_array([7L, 8L, 9L]),
                "empty list" >: List([]),
              ]),
         ])
    );
  let f = open_out("test.nbt");
  print_nbt_f(f, n);
  close_out(f);
};