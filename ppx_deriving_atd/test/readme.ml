type my_type = { field1 : string; field2 : int } [@@deriving atd_j]

(* Serialization *)
let my_value = { field1 = "value1"; field2 = 123 }

let json_string =
  json_string_of_my_type my_value (* {"field1":"value1","field2":123} *)

(* Deserialization *)
let deserialized_value = my_type_of_json_string json_string

type my_type2 = { field1 : string option; field2 : int option [@required] }
[@@deriving atd_j]

(* Serialization *)
let my_value2 = { field1 = None; field2 = None }
let json_string = json_string_of_my_type2 my_value2

(* Deserialization *)
let _deserialized_value = my_type2_of_json_string json_string

(* **** *)
let () =
  print_endline json_string;
  (* {"field1":"value1","field2":123} *)
  (* let _ = my_type_of_json_string {|{}|} in *)
  ()
