/** t is an array of nibbles, ie. 4 bits */
type t = array(int);

let init = (len, f) => {
  assert(len mod 2 == 0);
  Array.init(
    len / 2,
    i => {
      let first = f(i * 2) land 0xF;
      let second = f(i * 2 + 1) land 0xF;
      /* big endian, so second goes in the more significant bits */
      second lsl 4 lor first;
    },
  );
};

let get = (array, i) => {
  let byte = array[i / 2];
  if (i mod 2 == 0) {
    byte land 0xF;
  } else {
    byte lsr 4 land 0xF;
  };
};