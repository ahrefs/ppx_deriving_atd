type u = { x : int } [@@deriving atd]

type x = { abc : string option; def : int; ghi : bool [@default "false"] }
[@@deriving atd]

type y = { x : x } [@@deriving atd]
type zz = { z : Test2.z; y : y } [@@deriving atd]
type at = { a : Test2.T.t } [@@deriving atd]
type az = { b : int } [@@deriving atd]

type poly = string [@@deriving atd]

type 'poly poly_t = { x : 'poly; y : poly } [@@deriving atd]

type ('a, 'b) poly_t2 = { x : 'a; y : 'b } [@@deriving atd]

type constrained = (string, int) poly_t2 [@@deriving atd]

type 'a one_constrained = ('a, int) poly_t2 [@@deriving atd]

type constrained_again = poly one_constrained [@@deriving atd]
