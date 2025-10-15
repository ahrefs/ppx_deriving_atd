(* Motivation *)
(* Some cool polymorphic types *)
type ('a, 'b, 'c) t1 = { a : 'a; b : 'b; c : 'c; d : bool } [@@deriving atd]
type var1 = A | B of (int, string, bool) t1 [@@deriving atd]

(* But I don't like this name...? *)
type ('a, 'b) t1_constrained = ('a, 'b, int) t1 [@@deriving atd]

(* type ('a, 'b) t1_int = ('a, 'b, int) t1
[@@deriving atd] *)

type ('a, 'b) t1_string = ('a, 'b, string) t1 [@@deriving atd]

type ('a) t1_constrained_again  = ('a, string ) t1_constrained [@@deriving atd]

(* Then I also want an extra polymorphic field *)
(* type ('a, 'b) t1_string_string = ('a, 'b, string, string) t1 [@@deriving atd] *)

(* Some drawbacks *)
(* currently stub types are not type checked by OCaml compiler (but will be caught once ATD compiles) e.g. *)
type t2 [@@ocaml.from "SomeNonExistentModule"] [@@deriving atd]
type t3_wrap = string wrap [@ocaml.module "SomeNonExistentModule"] [@@deriving atd]

(* Inherits are not supported *)
(* type a = { a : bool }
   type b = { inherit a; b : int } :( *)

(* current setup is: OCaml -> ATD -> OCaml again, it would be better to just derive straight to OCaml *)

(* Have to call the export function *)
let () = export_atd_file "presentation.atd.out"
