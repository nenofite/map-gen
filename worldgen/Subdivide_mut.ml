open Core_kernel

let subdivide_with_fill ~fill m =
  let open Grid.Mut in
  (*
     old grid:
     1-2-3-
     ------
     4-5-6-
     ------
     7-8-9-
     ------
     *)
  let old_side = side m in
  expand_for_subdivide m ;
  (*
     diamonds:
     =-=-=-
     -1-2-3
     =-=-=-
     -4-5-6
     =-=-=-
     -7-8-9
     *)
  for zi = 0 to old_side - 1 do
    let z = (zi * 2) + 1 in
    for xi = 0 to old_side - 1 do
      let x = (xi * 2) + 1 in
      let nw = get_wrap ~x:(x - 1) ~z:(z - 1) m in
      let ne = get_wrap ~x:(x + 1) ~z:(z - 1) m in
      let sw = get_wrap ~x:(x - 1) ~z:(z + 1) m in
      let se = get_wrap ~x:(x + 1) ~z:(z + 1) m in
      set ~x ~z (fill nw ne sw se) m
    done
  done ;
  (*
     squares:
     =1=2=3
     4=5=6=
     =7=8=9
     0=1=2=
     =3=4=5
     6=7=8=
     *)
  for z = 0 to (old_side * 2) - 1 do
    let x_off = (z + 1) % 2 in
    for xi = 0 to old_side - 1 do
      let x = (xi * 2) + x_off in
      let n = get_wrap ~x ~z:(z - 1) m in
      let e = get_wrap ~x:(x + 1) ~z m in
      let s = get_wrap ~x ~z:(z + 1) m in
      let w = get_wrap ~x:(x - 1) ~z m in
      set ~x ~z (fill n e s w) m
    done
  done

let subdivide m = subdivide_with_fill ~fill:Fill.random m

let overwrite_subdivide_with_fill ~fill m =
  let open Grid.Mut in
  let old_side = side m in
  subdivide_with_fill ~fill m ;
  for zi = 0 to old_side - 1 do
    let z = zi * 2 in
    for xi = 0 to old_side - 1 do
      let x = xi * 2 in
      let n = get_wrap ~x ~z:(z - 1) m in
      let e = get_wrap ~x:(x + 1) ~z m in
      let s = get_wrap ~x ~z:(z + 1) m in
      let w = get_wrap ~x:(x - 1) ~z m in
      set ~x ~z (fill n e s w) m
    done
  done

let magnify m =
  let open Grid.Mut in
  (*
     old grid:
     1-2-3-
     ------
     4-5-6-
     ------
     7-8-9-
     ------
     *)
  let old_side = side m in
  expand_for_subdivide m ;
  (*
     magnify:
     =1=2=3
     112233
     =4=5=6
     445566
     =7=8=9
     778899
     *)
  for zi = 0 to old_side - 1 do
    let z = zi * 2 in
    for xi = 0 to old_side - 1 do
      let x = xi * 2 in
      let here = get ~x ~z m in
      set ~x:(x + 1) ~z here m ;
      set ~x ~z:(z + 1) here m ;
      set ~x:(x + 1) ~z:(z + 1) here m
    done
  done
