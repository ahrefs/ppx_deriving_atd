type x = {
 def: int;
 ghi: int;
}[@@deriving atd]


let () =
  {|{'def':5, 'ghi': 11}|} |> x_of_string |> string_of_x |> print_endline

