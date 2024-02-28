open Ppxlib

(* Some design decisions:
   - ignoring names lookup: each deriving is done on a particular type so will not have stubs defined for any non-primitive types. checks on types are done by the OCaml compiler.
   - to generate ATD ASTs from OCaml AST? or emit OCaml functions directly? Generating ATD ASTs is slightly more disciplined and easier.
   - do we need another level of module now? X_j.of_string and X_j.to_string? or stick to top-level x_j_to_string and x_j_of_string. Keeping it top-level for ease of use.
*)

let json =
  Attribute.declare "atd.json" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let impl_generator_to attr impl =
  Deriving.Generator.V2.make_noarg ~attributes:[ Attribute.T attr ] impl

let impl_generator_of attr impl =
  Deriving.Generator.V2.make ~attributes:[ Attribute.T attr ]
    Deriving.Args.(empty +> flag "skip_unknown")
    impl

let print_location { loc_start; loc_end; _ } =
  Atd.Ast.string_of_loc (loc_start, loc_end)

let generate_impl_atd ~ctxt (_rec_flag, type_decls) _skip_unknown =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_defs =
    List.map (Convert.type_def_of_type_declaration loc) type_decls
  in
  let atd_loc = Convert.atd_loc_of_parsetree_loc loc in
  let head, m0 = ((atd_loc, [] (*TODO: annotations*)), type_defs) in
  let m1', original_types =
    Atd.Expand.expand_module_body ~keep_builtins:false ~keep_poly:true m0
  in
  let m1 = Atd.Util.tsort m1' in
  let defs = Atdgen_emit.Oj_mapping.defs_of_atd_modules m1 ~target:Json in
  let ocaml_typedefs =
    Atdgen_emit.Ocaml.ocaml_of_atd ~target:Json ~type_aliases:None (head, m1)
  in
  let ml =
    Atdgen_emit.Oj_emit.make_ml ~header:"" ~opens:[] ~with_typedefs:false
      ~with_create:true ~with_fundefs:true ~std:true ~unknown_field_handler:None
      ~force_defaults:true ~preprocess_input:None ~original_types
      ~ocaml_version:None ocaml_typedefs
      (Atdgen_emit.Mapping.make_deref defs)
      defs
  in
  List.map
    (Convert.fold_loc_structure_item loc)
    (Parse.implementation (Lexing.from_string ml))

let deriver =
  Deriving.add "atd_j" ~str_type_decl:(impl_generator_of json generate_impl_atd)
