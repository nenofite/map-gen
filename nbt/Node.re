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
  Bigarray.Array1.t(int, Bigarray.int8_signed_elt, Bigarray.c_layout);
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

/* Ergonomics */

let (>:) = (name, payload) => {name, payload};

let make_byte_array = arr => {
  let ba = Bigarray.(Array1.of_array(Int8_signed, C_layout, arr));
  Byte_array(ba);
};