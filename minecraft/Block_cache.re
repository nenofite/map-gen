open! Core_kernel;

module H =
  Hashtbl.Make_plain({
    [@deriving (ord, hash, sexp)]
    type t = Block.material;
  });

type t = {cache: H.t(Block.material)};

let create = () => {cache: H.create()};

let clear = (t): unit => {
  Hashtbl.clear(t.cache);
};

let memo = (b: Block.material, ~cache: t) => {
  Hashtbl.findi_or_add(cache.cache, b, ~default=Fn.id);
};

let%expect_test "memoizes blocks" = {
  let force_copy = (a: 'a): 'a => Obj.repr(a) |> Obj.dup |> Obj.obj;
  let check_same = (a, b) => print_s([%sexp_of: bool](phys_equal(a, b)));
  let check_equal = (a, b) =>
    print_s([%sexp_of: bool](Block.equal_material(a, b)));

  let cache = create();
  let b1 =
    Block.(
      Fence(
        Oak_fence,
        {north: true, east: false, south: false, west: false},
        Dry,
      )
    );
  let b2 = force_copy(b1);

  check_same(b1, b2);
  %expect
  "false";

  check_same(memo(b1, ~cache), memo(b2, ~cache));
  %expect
  "true";

  check_equal(memo(b1, ~cache), b1);
  %expect
  "true";

  check_equal(memo(b2, ~cache), b2);
  %expect
  "true";

  check_equal(memo(Air, ~cache), Air);
  %expect
  "true";
};