type u = { x : int } [@@deriving atd]

type x = { abc : string option; def : int; ghi : bool [@default "false"] }
[@@deriving atd]

type y = { x : x } [@@deriving atd]
type zz = { z : Test2.z; y : y } [@@deriving atd]
type at = { a : Test2.T.t } [@@deriving atd]
type az = { b : int } [@@deriving atd]

type poly = string [@@deriving atd]

type 'poly poly_t = { x : 'poly; y : poly } [@@deriving atd]
