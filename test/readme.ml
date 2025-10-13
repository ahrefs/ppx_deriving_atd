type my_type = { field1 : string; field2 : int } [@@deriving atd]

type my_type2 = { field1 : string option; field2 : int option [@required] }
[@@deriving atd]

type my_type_default = {
  field1: string;
  field2: int [@default "999"];
} [@@deriving atd]

type my_type_required = {
  field1: string option;
  field2: int option [@required];
} [@@deriving atd]
