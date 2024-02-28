open Ppxlib
open Printf

let atd_loc_of_parsetree_loc { loc_start; loc_end; _ } = (loc_start, loc_end)

(* PARSETREE AST UTILS *)
let rec fold_loc_exp_desc loc exp_desc =
  match exp_desc with
  | Parsetree.Pexp_ident e -> Pexp_ident { e with loc }
  | Pexp_let (flag, value_bindings, e) ->
      Pexp_let
        (flag, fold_loc_value_bindings loc value_bindings, fold_loc_exp loc e)
  | Pexp_apply (e, args) ->
      Pexp_apply
        ( fold_loc_exp loc e,
          List.map (fun (a, e) -> (a, fold_loc_exp loc e)) args )
  | Pexp_fun (arg, e0, p, e1) ->
      Pexp_fun
        ( arg,
          Option.map (fold_loc_exp loc) e0,
          fold_loc_pat loc p,
          fold_loc_exp loc e1 )
  | Pexp_sequence (e1, e2) ->
      Pexp_sequence (fold_loc_exp loc e1, fold_loc_exp loc e2)
  | _ -> exp_desc

and fold_loc_pat_desc loc = function
  | Ppat_var l -> Ppat_var { l with loc }
  | p -> p

and fold_loc_value_bindings loc =
  List.map (fun v ->
      {
        v with
        pvb_loc = loc;
        pvb_expr = fold_loc_exp loc v.pvb_expr;
        pvb_pat = fold_loc_pat loc v.pvb_pat;
      })

and fold_loc_pat loc pat = { pat with ppat_loc = loc; ppat_loc_stack = [ loc ] }

and fold_loc_exp loc exp =
  {
    exp with
    pexp_loc_stack = loc :: exp.pexp_loc_stack;
    pexp_loc = loc;
    pexp_desc = fold_loc_exp_desc loc exp.pexp_desc;
  }

let fold_loc_structure_item loc x =
  let pstr_desc =
    match x.pstr_desc with
    | Parsetree.Pstr_eval (e, attrs) -> Pstr_eval (fold_loc_exp loc e, attrs)
    | Pstr_value (flag, value_bindings) ->
        Pstr_value (flag, fold_loc_value_bindings loc value_bindings)
    | e -> e
  in
  { pstr_loc = loc; pstr_desc }

let string_of_parse_tree ff x =
  let open Format in
  ignore (flush_str_formatter ());
  let f = str_formatter in
  ff f x;
  flush_str_formatter ()

let string_of_core_type = string_of_parse_tree Pprintast.core_type
let string_of_type_decl = string_of_parse_tree Pprintast.type_declaration
(* *********** *)

let raise_errorf ?sub ?loc fmt =
  let module Location = Ocaml_common.Location in
  let raise_msg str =
    let err = Location.error ?sub ?loc str in
    raise (Location.Error err)
  in
  ksprintf raise_msg fmt

let unsupported_derivation loc msg =
  raise_errorf ~loc "PPX Deriving ATD: Unsupported derivation: %s at %s" msg
    (loc |> atd_loc_of_parsetree_loc |> Atd.Ast.string_of_loc)

open Atd.Ast

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
            (fun { pld_type; pld_name; _ } ->
              let loc = atd_loc_of_parsetree_loc pld_name.loc in
              let type_expr = type_expr_of_core_type loc' pld_type in
              `Field
                ( loc,
                  ( pld_name.txt,
                    (match type_expr with
                    | (Option (_, _, _) : Atd.Ast.type_expr) -> Optional
                    | _ ->
                        Required
                        (* TODO: better way?, need more annotation logic*)),
                    [] (* TODO: annotations *) ),
                  type_expr ))
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
