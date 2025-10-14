type z = { zz : string; zzz : int; zzzz : bool } [@@deriving atd]

module T = struct
  type t = { a : int } [@@deriving atd]
end

module Str_int = struct
  type t = int

  let wrap = int_of_string
  let unwrap = string_of_int

  let of_json x =
    match x with
    | `String s -> wrap s
    | _ -> raise (Invalid_argument "Str_int.of_json")

  let to_json x = `String (unwrap x)
end
