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
  window: De.Lz77.window,
  queue: De.Queue.t,
  input_bs: bigstring,
  output_bs: bigstring,
};

let buffer_size = 128 * 1024;
let create_memory = () => {
  buffer: Buffer.create(buffer_size),
  window: De.Lz77.make_window(~bits=15),
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
      ~refill,
      ~flush,
      (),
      config,
      input_bs,
      output_bs,
    );
  } else {
    Zl.Higher.compress(
      ~level=0,
      ~w=window,
      ~q=queue,
      ~refill,
      ~flush,
      input_bs,
      output_bs,
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
let test =
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

let%expect_test "printing" = {
  let print_hex = buffer => {
    Buffer.to_seqi(buffer)
    |> Seq.iter(((i, c)) => {
         if (i mod 12 == 0) {
           print_newline();
         };
         Printf.printf("%02x ", Char.code(c));
       });
    flush(stdout);
  };

  print_nbt(~gzip=false, test) |> print_hex;
  %expect
  "
    78 01 e3 62 60 e0 62 e0 48 ce cf 2d
    c8 2f cd 4b 61 64 60 49 aa 2c 49 55
    65 62 60 2d ce c8 2f 2a 61 5c cd cc
    c0 9c 99 57 c2 c0 c0 7d 8d 85 81 25
    27 3f 2f 9d 01 0c 0c 2c 59 19 58 d3
    72 f2 13 4b ec 97 a5 a5 b1 31 b0 a5
    e4 97 26 e5 a4 da 7f 4b 03 03 76 06
    2e 90 39 0a 89 45 45 89 95 40 e5 cc
    8c 4c cc 1c 0c 6c c5 25 45 99 40 13
    b8 33 52 73 72 f2 15 ca f3 8b 72 52
    38 81 a6 66 16 97 70 80 14 31 30 26
    32 30 26 31 30 26 73 33 70 02 2d 45
    e8 06 62 16 20 66 05 62 36 1e 06 2e
    90 33 50 24 41 80 1d 42 31 80 4c 02
    01 4e 4e 06 ae d4 dc 82 92 4a 05 90
    f9 10 31 06 00 cf d9 30 8b
  ";

  print_nbt(~gzip=true, test) |> print_hex;
  %expect
  "
    1f 8b 08 00 00 00 00 01 00 03 e3 62
    60 e0 62 e0 48 ce cf 2d c8 2f cd 4b
    61 64 60 49 aa 2c 49 55 65 62 60 2d
    ce c8 2f 2a 61 5c cd cc c0 9c 99 57
    c2 c0 c0 7d 8d 85 81 25 27 3f 2f 9d
    01 0c 0c 2c 59 19 58 d3 72 f2 13 4b
    ec 97 a5 a5 b1 31 b0 a5 e4 97 26 e5
    a4 da 7f 4b 03 03 76 06 2e 90 39 0a
    89 45 45 89 95 40 e5 cc 8c 4c cc 1c
    0c 6c c5 25 45 99 40 13 b8 33 52 73
    72 f2 15 ca f3 8b 72 52 38 81 a6 66
    16 97 70 80 14 31 30 26 32 30 26 31
    30 26 73 33 70 02 2d 45 e8 06 62 16
    20 66 05 62 36 1e 06 2e 90 33 50 24
    41 80 1d 42 31 80 4c 02 01 4e 4e 06
    ae d4 dc 82 92 4a 05 90 f9 10 31 06
    00 4a cd ff 1d ee 00 00 00
  ";
};