/**
  Gin is a nicer frontend for Alcotest
 */

let run = Alcotest.run;

/** A test */
let (>:) = (name, test) =>
  Alcotest.(name, [test_case(name, `Quick, test)]);

/** A slow (optional) test */
let (>:?) = (name, test) =>
  Alcotest.(name, [test_case(name, `Slow, test)]);