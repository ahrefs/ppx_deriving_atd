type x = {
 abc: string option;
 def: int;
 ghi: bool;
}[@@deriving atd]


let () =
  let x = {abc = None; def = 1; ghi = true} in
  let x_str = string_of_x x in
  print_endline x_str ;
  let x = x_of_string x_str in
  print_endline @@ string_of_x x

