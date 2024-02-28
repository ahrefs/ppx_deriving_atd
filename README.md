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
let deserialized_value = t_of_json_string_my_type json_string

```
