open Core_kernel

let grid_of_txt ~palette txt = (
  let parse_line lines_rev line_txt =
    let words = String.split ~on:' ' (String.strip line_txt) in
    let cells_rev = List.fold words
        ~init:[]
        ~f:(fun cells word -> List.Assoc.find_exn palette ~equal:String.equal word :: cells)
    in
    List.rev cells_rev :: lines_rev
  in
  let lines = List.rev (List.fold (String.split_lines @@ String.strip txt) ~init:[] ~f:parse_line) in
  let height = List.length lines in
  let width = List.length (List.hd_exn lines) in
  assert (height = width);
  let empty = List.hd_exn (List.hd_exn lines) in
  let grid = Grid.Mut.create ~side:width empty in
  List.iteri lines ~f:(fun z line ->
      List.iteri line ~f:(fun x cell -> 
          Grid.Mut.set ~x ~z cell grid;
        )
    );
  grid
)