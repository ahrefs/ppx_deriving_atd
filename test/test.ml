type u = { x : int } [@@deriving atd]

type x = {
  abc : string option;
  def : int;
  ghi : bool; [@default "false"] [@json.name "something_else"]
}
[@@deriving atd]

type y = { x : x } [@@deriving atd]
type zz = { z : Test2.z; y : y } [@@deriving atd]
type at = { a : Test2.T.t } [@@deriving atd]
type az = { b : int } [@@deriving atd]
type poly = string [@@deriving atd]
type 'poly poly_t = { x : 'poly; y : poly } [@@deriving atd]
type ('a, 'b) poly_t2 = { x : 'a; y : 'b } [@@deriving atd]
type constrained = (string, int) poly_t2 [@@deriving atd]

type 'a one_constrained = ('a, int) poly_t2
[@@deriving atd] [@@attr "deriving show {with_path = false}, eq"]

type constrained_again = poly one_constrained
[@@deriving atd] [@@attr "deriving show {with_path = false}, eq"]

type with_attrs =
  | Bad_value [@json.name "bad_value"]
  | Good_value of int [@json.name "good_value"]
[@@deriving atd]
[@@repr "classic"]
[@@attr "deriving show {with_path = false}, eq"]

type str_int [@@from "Test2"] [@@deriving atd] (* it would be better to link to the module and then the compiler can check for errors before ATD catches *)

type str_int_wrap = string wrap [@module "Test2"] [@@deriving atd]
