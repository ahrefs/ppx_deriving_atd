type my_type = { field1 : string; field2 : int } [@@deriving atd_j]

(* Serialization *)
let my_value = { field1 = "value1"; field2 = 123 }
let json_string = json_string_of_my_type my_value

(* Deserialization *)
let deserialized_value = my_type_of_json_string json_string

(* **** *)
let () = print_endline json_string
(* {"field1":"value1","field2":123} *)
