type z = { zz : string; zzz : int; zzzz : bool } [@@deriving atd]

module T = struct
  type t = { a : int } [@@deriving atd]
end
