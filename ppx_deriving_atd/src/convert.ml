open Ppxlib
open Printf
open Atd.Ast
open Common

let ml_string_of_atd_module_items loc type_defs =
  let atd_loc = atd_loc_of_parsetree_loc loc in
  let head, m0 = ((atd_loc, [] (*TODO: annotations*)), type_defs) in
  let m1', original_types =
    Atd.Expand.expand_module_body ~keep_builtins:false ~keep_poly:true m0
  in
  let m1 = Atd.Util.tsort m1' in
  let defs = Atdgen_emit.Oj_mapping.defs_of_atd_modules m1 ~target:Json in
  let ocaml_typedefs =
    Atdgen_emit.Ocaml.ocaml_of_atd ~target:Json ~type_aliases:None (head, m1)
  in
  Atdgen_emit.Oj_emit.make_ml ~header:"" ~opens:[] ~with_typedefs:false
    ~with_create:true ~with_fundefs:true ~std:true ~unknown_field_handler:None
    ~force_defaults:true ~preprocess_input:None ~original_types
    ~ocaml_version:None ocaml_typedefs
    (Atdgen_emit.Mapping.make_deref defs)
    defs

let record_type_of_attributes loc type_decl type_expr pld_attributes =
  (* might be better to type-check default payload here then do %S *)
  let extract_single_string = Ast_pattern.(single_expr_payload (estring __)) in
  let extract_default_payload ~type_decl p =
    Ast_pattern.parse extract_single_string loc
      ~on_error:(fun () ->
        illegal_derivation loc
          "only stringify default values are accepted (e.g: \"1\" instead of \
           1): "
        ^ string_of_type_decl type_decl)
      p Fun.id
  in
  let attrs =
    List.map
      (fun { attr_name = { txt; _ }; attr_payload; _ } -> (txt, attr_payload))
      pld_attributes
    |> List.to_seq |> Hashtbl.of_seq
  in
  match Hashtbl.find_opt attrs "default" with
  | Some payload ->
      (With_default, Some (extract_default_payload ~type_decl payload))
  | None -> (
      match type_expr with
      | (Option (_, _, _) : Atd.Ast.type_expr) when Hashtbl.mem attrs "required"
        ->
          (Required, None)
      | (Option (_, _, _) : Atd.Ast.type_expr) ->
          (Optional, None)
          (* optional types are optional by default unlessa annotated with required *)
      | _ when Hashtbl.mem attrs "optional" ->
          illegal_derivation loc
            ("must be an option type to have optional annotation: "
            ^ string_of_type_decl type_decl)
      | _ -> (Required, None))

let rec type_def_of_type_declaration loc type_decl =
  let loc = atd_loc_of_parsetree_loc loc in
  let type_expr, extras = type_expr_of_type_declaration ~extras:[] type_decl in
  Type (loc, (type_decl.ptype_name.txt, [] (*TODO: type params*), []), type_expr)
  :: extras

and type_expr_of_type_declaration ~extras type_decl =
  let loc', loc =
    (type_decl.ptype_loc, atd_loc_of_parsetree_loc type_decl.ptype_loc)
  in
  match type_decl with
  | Parsetree.
      { ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ->
      type_expr_of_core_type ~extras loc' core_type
  | { ptype_kind = Ptype_record tys; _ } ->
      let extras', type_exprs =
        List.fold_left
          (fun (extras, type_exprs) { pld_type; pld_name; pld_attributes; _ } ->
            let loc = atd_loc_of_parsetree_loc pld_name.loc in
            let type_expr, extras' =
              type_expr_of_core_type ~extras loc' pld_type
            in
            let field_type, payload =
              record_type_of_attributes loc' type_decl type_expr pld_attributes
            in
            let annots =
              match field_type with
              | With_default ->
                  [ ("ocaml", (loc, [ ("default", (loc, payload)) ])) ]
              | _ -> []
            in
            ( extras',
              `Field (loc, (pld_name.txt, field_type, annots), type_expr)
              :: type_exprs ))
          (extras, []) tys
      in
      (Record (loc, List.rev type_exprs, [] (* TODO: annotations *)), extras')
  | { ptype_kind = Ptype_variant tys; _ } as type_decl ->
      let extras', type_exprs =
        List.fold_left
          (fun (extras, type_exprs) -> function
            | { pcd_name; pcd_args = Pcstr_tuple []; _ } ->
                ( extras,
                  (Variant (loc, (pcd_name.txt, [] (* TODO: annot *)), None)
                    : variant)
                  :: type_exprs )
            | { pcd_name; pcd_args = Pcstr_tuple l; _ } ->
                let type_expr, extras' =
                  type_expr_of_core_type ~extras loc'
                    Parsetree.
                      {
                        ptyp_desc = Ptyp_tuple l;
                        ptyp_loc = pcd_name.loc;
                        ptyp_loc_stack = [ pcd_name.loc ];
                        ptyp_attributes = [];
                      }
                in
                ( extras' @ extras,
                  Variant
                    (loc, (pcd_name.txt, [] (* TODO: annot *)), Some type_expr)
                  :: type_exprs )
            | _ -> unsupported_derivation loc' (string_of_type_decl type_decl))
          (extras, []) tys
      in
      (Sum (loc, List.rev type_exprs, [] (* TODO: annotations *)), extras')
  | _ ->
      unsupported_derivation type_decl.ptype_loc (string_of_type_decl type_decl)

and type_expr_of_core_type ~extras loc' core_type =
  let loc = atd_loc_of_parsetree_loc loc' in
  match core_type.ptyp_desc with
  | Ptyp_var x -> (Tvar (loc, x), extras)
  | Ptyp_tuple tys ->
      with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
      Tuple (loc, type_exprs, [] (* TODO: annotations *))
  | Ptyp_constr
      ( { txt = Lident "list" (* TODO: qualified names e.g. Module.x *); loc },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      with_core_ty_extras ~extras ~ty loc' @@ fun ~type_expr ->
      List (loc, type_expr, [] (* TODO: annotations *))
  | Ptyp_constr (id, tys) ->
      type_expr_of_ptyp_constr ~extras loc' core_type (id, tys)
  | _ ->
      (* Shard and Wrap doesn't exist anymore *)
      unsupported_derivation loc' (string_of_core_type core_type)

and type_expr_of_ptyp_constr ~extras loc' core_type = function
  | ( { txt = Lident "option" (* TODO: qualified names e.g. Module.x *); loc },
      [ ty ] (* only supports 1 type in option*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      with_core_ty_extras ~extras ~ty loc' @@ fun ~type_expr ->
      Option (loc, type_expr, [] (* TODO: annotations *))
  | { txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, _
    when List.mem id [ "list"; "option" ] ->
      (* TODO: support (string, json> list and allow conversion to object *)
      unsupported_derivation loc
        (sprintf "%s: has multiple types for %s (invalid)"
           (string_of_core_type core_type)
           id)
  | { txt = Ldot (prefix, id); loc }, tys ->
      let loc = atd_loc_of_parsetree_loc loc in
      let f_cell (_loc, type_expr, _annot) = type_expr in
      let type_expr, extras =
        with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
        Name
          ( loc,
            (loc, id, List.map f_cell type_exprs),
            [] (* TODO: annotations *) )
      in
      let (_head, type_decls), _original_types =
        Atd.Util.load_string
        @@ sprintf {|type %s <ocaml from=%S> = abstract|} id
             (Longident.name prefix)
      in
      (type_expr, type_decls @ extras)
  | { txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, tys ->
      let loc = atd_loc_of_parsetree_loc loc in
      let f_cell (_loc, type_expr, _annot) = type_expr in
      with_core_tys_extras ~extras ~tys loc' @@ fun ~type_exprs ->
      Name
        (loc, (loc, id, List.map f_cell type_exprs), [] (* TODO: annotations *))
  | _ -> unsupported_derivation loc' (string_of_core_type core_type)

and with_core_tys_extras ~extras ~tys loc' f =
  let loc = atd_loc_of_parsetree_loc loc' in
  let extras', type_exprs =
    List.fold_left
      (fun (extras, type_exprs) core_type ->
        let type_expr, extras' =
          type_expr_of_core_type ~extras loc' core_type
        in
        ( extras' @ extras,
          (loc, type_expr, [] (* TODO: annotations *)) :: type_exprs ))
      (extras, []) tys
  in
  (f ~type_exprs:(List.rev type_exprs), extras')

and with_core_ty_extras ~extras ~ty loc' f =
  let type_expr, extras' = type_expr_of_core_type ~extras loc' ty in
  (f ~type_expr, extras' @ extras)
