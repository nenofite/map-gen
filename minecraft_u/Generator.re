type t =
  | Flat;

let name =
  fun
  | Flat => "flat";

/* TODO https://github.com/MorbZ/J2Blocks/blob/d8745985069dbd190c30d44e9ce02040f3501aad/src/net/morbz/minecraft/level/FlatGenerator.java#L73 */
let options = (_generator: t): option(string) => None;