# ppx\_deriving\_atd

## Overview
This package introduces a PPX (Preprocessor Extension) for OCaml that automates the generation of serialization and deserialization functions for your OCaml types using [ATD](https://github.com/ahrefs/atd) without creating an ATD file.  

Currently supporting JSON derivation only:
- `json_string_of_t`: Serializes an OCaml value to a JSON string.
- `t_of_json_string`: Deserializes a JSON string back into an OCaml value.

## Usage

```ocaml
type my_type = {
  field1: string;
  field2: int;
} [@@deriving atd_j]

(* Serialization *)
let my_value = { field1 = "value1"; field2 = 123 }
let json_string = json_string_of_my_type my_value (* {"field1":"value1","field2":123} *)

(* Deserialization *)
let deserialized_value = my_type_of_json_string json_string

```

### Supported Annoations
#### default
Annotating a record field with `[@default v]` will allow the generated reader to give default value for the field (and so the field no longer have an optional type).  Note `v` must be in a stringify form: `"999"` instead of `999`.

```ocaml
type my_type = {
  field1: string;
  field2: int [@default "999"];
} [@@deriving atd_j]


(* Deserialization *)
let deserialized_value = my_type_of_json_string {|{"field1":"value1"}|}
(* { field1 = "value1"; field2 = 999 } *)

```

#### required
All option typed fields are "optional" by default, i.e. if the value is `None`, it will not be included when serialization and when deserializing, ATD will assume the value is `None` if the field is not present.  

Annotating a record field with `[@required]` will always keep the optional value in the serialized record and ATD will keep looking for it when deserializing the records.

```ocaml
type my_type = {
  field1: string option;
  field2: int option [@required];
} [@@deriving atd_j]

(* Serialization *)
let my_value = { field1 = None; field2 = None }
let json_string = json_string_of_my_type my_value (* {"field2":"None"} *)

(* Deserialization *)
let deserialized_value = my_type_of_json_string "{}"
(* Fatal error: exception Atdgen_runtime.Oj_run.Error("Line 1:\nMissing record field field2") *)

```
