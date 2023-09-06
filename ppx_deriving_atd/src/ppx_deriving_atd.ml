open Ppxlib
open Ast_helper
open Ast_builder.Default

let suf_to = "to_string"
let suf_of = "of_string"

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
  let ((head, m0), _) = Atd.Util.load_string {|
type x = {
 ?abc: string nullable;
 def: int;
 ~ghi <ocaml default="true">: bool;
}
|}
  in
  (* parsetree: the annotations will be attributes/extenders; *)
  (* we can either
   1. convert the attributes + type back into atd then run the atdgen pipeline
   2. figure out the arguments needed to run `Ob_emit.make_ml` so it results in the same types *)
  let m1 = Atd.Util.tsort m0 in
  let ocaml_typedefs = Atdgen_emit.Ocaml.ocaml_of_atd ~target:Biniou ~type_aliases:None (head, m1) in (* generate the actual ocaml types--not needed because will be valid ocaml syntax first *)
  print_endline ocaml_typedefs;
  let parsetree = Parse.implementation @@ Lexing.from_string ocaml_typedefs in
  Format.printf "%a@." Pprintast.structure parsetree;
  ()

let default =
  Attribute.declare "atd.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let to_atd =
  Attribute.declare "atd.to_atd" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let of_atd =
  Attribute.declare "atd.of_atd" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)


let impl_generator_to impl =
  Deriving.Generator.V2.make_noarg
    ~attributes:
      [
        Attribute.T default
      ]
    impl

let impl_generator_of impl =
  Deriving.Generator.V2.make
    ~attributes:
      [
        Attribute.T default
      ]
    Deriving.Args.(empty +> flag "skip_unknown")
    impl

let print_location {loc_start; loc_end; _ } = Atd.Ast.string_of_loc (loc_start, loc_end)

let generate_impl_atd ~ctxt (rec_flag, type_decls) skip_unknown =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat
    (List.map
       (fun typ_decl ->
         match typ_decl with
         | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
             let typ_str = Pprintast.string_of_structure [ { pstr_desc = (Ast.Pstr_type (rec_flag, [ { typ_decl with ptype_attributes = [] }])); pstr_loc = ptype_loc } ] in
             print_endline typ_str;
             let ((head, m0), _) = Atd.Util.load_string typ_str in
             let m1 = Atd.Util.tsort m0 in
             let defs1 = Atdgen_emit.Ob_mapping.defs_of_atd_modules m1 in
             Atdgen_emit.Xb_emit.check defs1;
             let (m1', original_types) =
               Atd.Expand.expand_module_body ~keep_poly:true m0 in
             let m2 = Atd.Util.tsort m1' in
             let defs = Atdgen_emit.Ob_mapping.defs_of_atd_modules m2 in
             let ml =
               Atdgen_emit.Ob_emit.make_ml ~header:"" ~opens:[] ~with_typedefs:false ~with_create:true ~with_fundefs:true
                 ~original_types ~ocaml_version:None typ_str
                 (Atdgen_emit.Mapping.make_deref defs) defs
             in
             print_endline ml;
             Printf.printf "ptype_loc: %s\n" (print_location ptype_loc);
             List.map (fun str_item -> {str_item with pstr_loc = ptype_loc}) (Parse.implementation (Lexing.from_string ml))
         | _ ->
             Location.raise_errorf ~loc "Cannot derive anything for this type")
       type_decls)

let deriver =
    Deriving.add "atd"
      ~str_type_decl:(impl_generator_of generate_impl_atd)
