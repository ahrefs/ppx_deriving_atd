open Ppxlib
open Printf
open Atd.Ast
open Common

let record_type_of_attributes loc type_decl type_expr pld_attributes =
  let extract_default_payload = function
    (* we only accept 1 argument default payload as a string  *)
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
            _;
          };
        ] ->
        Some s
    | PStr [] -> None
    | PStr s ->
        illegal_derivation loc
          ("only stringify default values are accepted (e.g: \"1\" instead of \
            1), given: " ^ string_of_structure s)
    | _ -> illegal_derivation loc (string_of_type_decl type_decl)
  in
  let attrs, attrs_with_payload =
    List.split
    @@ List.map
         (fun { attr_name = { txt; _ }; attr_payload; _ } ->
           (txt, (txt, extract_default_payload attr_payload)))
         pld_attributes
  in
  match type_expr with
  | _ when List.mem "default" attrs ->
      (With_default, List.assoc "default" attrs_with_payload)
  | (Option (_, _, _) : Atd.Ast.type_expr) when List.mem "required" attrs ->
      (Required, None)
  | (Option (_, _, _) : Atd.Ast.type_expr) ->
      (Optional, None)
      (* optional types are optional by default unlessa annotated with required *)
  | _ when List.mem "optional" attrs ->
      illegal_derivation loc
        ("must be an option type to have optional annotation: "
        ^ string_of_type_decl type_decl)
  | _ -> (Required, None)

let rec type_def_of_type_declaration loc type_decl =
  let loc = atd_loc_of_parsetree_loc loc in
  Type
    ( loc,
      (type_decl.ptype_name.txt, [] (*TODO: type params*), []),
      type_expr_of_type_declaration type_decl )

and type_expr_of_type_declaration type_decl =
  let loc', loc =
    (type_decl.ptype_loc, atd_loc_of_parsetree_loc type_decl.ptype_loc)
  in
  match type_decl with
  | Parsetree.
      { ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ->
      type_expr_of_core_type loc' core_type
  | { ptype_kind = Ptype_record l; _ } ->
      Record
        ( loc,
          List.map
            (fun { pld_type; pld_name; pld_attributes; _ } ->
              let loc = atd_loc_of_parsetree_loc pld_name.loc in
              let type_expr = type_expr_of_core_type loc' pld_type in
              let field_type, payload =
                record_type_of_attributes loc' type_decl type_expr
                  pld_attributes
              in
              let annots =
                match field_type with
                | With_default ->
                    [ ("ocaml", (loc, [ ("default", (loc, payload)) ])) ]
                | _ -> []
              in
              `Field (loc, (pld_name.txt, field_type, annots), type_expr))
            l,
          [] (* TODO: annotations *) )
  | { ptype_kind = Ptype_variant l; _ } as type_decl ->
      Sum
        ( loc,
          List.map
            (function
              | { pcd_name; pcd_args = Pcstr_tuple []; _ } ->
                  (Variant (loc, (pcd_name.txt, [] (* TODO: annot *)), None)
                    : variant)
              | { pcd_name; pcd_args = Pcstr_tuple l; _ } ->
                  Variant
                    ( loc,
                      (pcd_name.txt, [] (* TODO: annot *)),
                      Some
                        (type_expr_of_core_type loc'
                           Parsetree.
                             {
                               ptyp_desc = Ptyp_tuple l;
                               ptyp_loc = pcd_name.loc;
                               ptyp_loc_stack = [ pcd_name.loc ];
                               ptyp_attributes = [];
                             }) )
              | _ -> unsupported_derivation loc' (string_of_type_decl type_decl))
            l,
          [] (* TODO: annotations *) )
  | _ ->
      unsupported_derivation type_decl.ptype_loc (string_of_type_decl type_decl)

and type_expr_of_core_type loc' core_type =
  let loc = atd_loc_of_parsetree_loc loc' in
  match core_type.ptyp_desc with
  | Ptyp_var x -> Tvar (loc, x)
  | Ptyp_tuple l ->
      Tuple
        ( loc,
          List.map
            (fun core_type ->
              ( loc,
                type_expr_of_core_type loc' core_type,
                [] (* TODO: annotations *) ))
            l,
          [] (* TODO: annotations *) )
  | Ptyp_constr
      ( { txt = Lident "list" (* TODO: qualified names e.g. Module.x *); loc },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      List (loc, type_expr_of_core_type loc' ty, [] (* TODO: annotations *))
  | Ptyp_constr
      ( { txt = Lident "option" (* TODO: qualified names e.g. Module.x *); loc },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      Option (loc, type_expr_of_core_type loc' ty, [] (* TODO: annotations *))
  | Ptyp_constr
      ({ txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, _)
    when List.mem id [ "list"; "option" ] ->
      unsupported_derivation loc
        (sprintf "%s: has multiple types for %s (invalid)"
           (string_of_core_type core_type)
           id)
  | Ptyp_constr
      ({ txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, l) ->
      let loc = atd_loc_of_parsetree_loc loc in
      Name
        ( loc,
          (loc, id, List.map (type_expr_of_core_type loc') l),
          [] (* TODO: annotations *) )
  | _ ->
      (* Shard and Wrap doesn't exist anymore *)
      print_endline @@ string_of_core_type core_type;
      unsupported_derivation loc' (string_of_core_type core_type)
