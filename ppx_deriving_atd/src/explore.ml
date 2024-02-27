open Ppxlib
open Ast_helper
open Ast_builder.Default

let suf_to = "j_to_string"
let suf_of = "j_of_string"

let mangle_name_label suff label =
  if label = "t" then suff else label ^ "_" ^ suff

let check_rec_type rec_flag typ =
  let check =
    object
      inherit type_is_recursive rec_flag typ
    end
  in
  check#go

(* we need to manipulate the parsetree *)
let explore () =
  let (head, m0), _ =
    Atd.Util.load_string
      {|
type x = {
 ?abc: string nullable;
 def: int;
 ~ghi <ocaml default="true">: bool;
}

type y = x list
|}
  in
  (* parsetree: the annotations will be attributes/extenders; *)
  (* we can either
     1. convert the attributes + type back into atd then run the atdgen pipeline
     2. figure out the arguments needed to run `Ob_emit.make_ml` so it results in the same types *)
  let m1 = Atd.Util.tsort m0 in
  let ocaml_typedefs =
    Atdgen_emit.Ocaml.ocaml_of_atd ~target:Biniou ~type_aliases:None (head, m1)
  in
  (* generate the actual ocaml types--not needed because will be valid ocaml syntax first *)
  print_endline ocaml_typedefs;
  let parsetree = Parse.implementation @@ Lexing.from_string ocaml_typedefs in
  Format.printf "%a@." Pprintast.structure parsetree;
  ()
