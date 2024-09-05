type z = { zz : string; zzz : int; zzzz : bool } [@@deriving atd_j]

module T = struct
  type t = { a : int } [@@deriving atd_j]
end
