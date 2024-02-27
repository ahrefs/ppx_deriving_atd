type x = { abc : string option; def : int; ghi : bool } [@@deriving atd]
type y = { x : x } [@@deriving atd]

open Test2

type zz = { z : z } [@@deriving atd]

let () =
  let x = { abc = None; def = 1; ghi = true } in
  let x_str = string_of_x x in
  print_endline x_str;
  let x = x_of_string x_str in
  print_endline @@ string_of_x x;
  print_endline @@ string_of_y { x };
  let z = { zz = None; zzz = 1; zzzz = false } in
  print_endline @@ Test2.string_of_z z;
  print_endline @@ string_of_zz { z }
