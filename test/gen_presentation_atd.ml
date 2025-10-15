(* Motivation *)
(* Some cool polymorphic types *)
type ('a, 'b, 'c) t1 = { a : 'a; b : 'b; c : 'c; d : bool } [@@deriving atd]
type var1 = A | B of (int, string, bool) t1 [@@deriving atd]

(* But I don't like this name...? *)
type ('a, 'b) t1_constrained = ('a, 'b, int) t1 [@@deriving atd]

(* type ('a, 'b) t1_int = ('a, 'b, int) t1
[@@deriving atd] *)

type ('a, 'b) t1_string = ('a, 'b, string) t1 [@@deriving atd]

(* Then I also want an extra polymorphic field *)
(* type ('a, 'b) t1_string_string = ('a, 'b, string, string) t1 [@@deriving atd] *)

let () = export_atd_file "presentation.atd.out"
