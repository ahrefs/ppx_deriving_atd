type u = { x : int } [@@deriving atd_j]

type x = { abc : string option; def : int; ghi : bool [@default "false"] }
[@@deriving atd_j]

type y = { x : x } [@@deriving atd_j]
type zz = { z : Test2.z; y : y } [@@deriving atd_j]
type at = { a : Test2.T.t } [@@deriving atd_j]

let () =
  let x_str = {|{"def":1}|} in
  let x = x_of_json_string x_str in
  print_endline @@ json_string_of_x x;
  print_endline @@ json_string_of_y { x };
  let x' = create_x ~def:2 () in
  print_endline @@ json_string_of_x x';
  let z = Test2.{ zz = ""; zzz = 1; zzzz = false } in
  print_endline @@ Test2.json_string_of_z z;
  let zz = { z; y = { x } } in
  print_endline @@ Readme.json_string;
  print_endline @@ json_string_of_zz zz
