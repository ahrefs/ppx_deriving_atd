type x = { abc : string option; def : int; ghi : bool } [@@deriving atd_j]
type y = { x : x } [@@deriving atd_j]

open Test2

type zz = { z : z; y : y } [@@deriving atd_j]

let () =
  let x = { abc = None; def = 1; ghi = true } in
  let x_str = json_string_of_x x in
  print_endline x_str;
  let x = x_of_json_string x_str in
  print_endline @@ json_string_of_x x;
  (* print_endline @@ string_of_y { x }; *)
  let z = { zz = ""; zzz = 1; zzzz = false } in
  print_endline @@ json_string_of_zz { z; y = { x } };
  print_endline @@ Test2.json_string_of_z z
