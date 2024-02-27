open Ppxlib
open Ast_helper
open Ast_builder.Default
open Printf

exception Not_implemented
exception Unsupported_derivation of core_type

let string_of_parse_tree ff x =
  let open Format in
  ignore (flush_str_formatter ());
  let f = str_formatter in
  ff f x;
  flush_str_formatter ()

let string_of_core_type = string_of_parse_tree Pprintast.core_type
let string_of_type_decl = string_of_parse_tree Pprintast.type_declaration
let unsupported_derivation core_type = raise (Unsupported_derivation core_type)

(* let rec mapping_of_type_declaration loc = function *)
(*   | Parsetree.{ ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _} -> mapping_of_core_type_desc loc core_type *)
(*   | _ -> raise Not_implemented *)
(* and mapping_of_core_type_desc loc core_type = *)
(*   match core_type.Parsetree.ptyp_desc with *)
(*   | Ptyp_var x -> Tvar (loc, x) *)
(*   | Ptyp_arrow _ -> unsupported_derivation "function type" *)
(*   | Ptyp_tuple l -> Tuple (loc, List.map cell_mapping_of_core_type l |> Array.of_list,) *)
(*                            | _ -> print_endline @@ string_of_core_type core_type; *)
(*                            unsupported_derivation core_type *)

let atd_loc_of_parsetree_loc { loc_start; loc_end; _ } = (loc_start, loc_end)

open Atd.Ast

let rec type_expr_of_type_declaration type_decl =
  let loc = atd_loc_of_parsetree_loc type_decl.ptype_loc in
  match type_decl with
  | Parsetree.
      { ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ->
      type_expr_of_core_type loc core_type
  | { ptype_kind = Ptype_record l; _ } ->
      Record
        ( loc,
          List.map
            (fun { pld_type; pld_loc; pld_name; _ } ->
              let loc = atd_loc_of_parsetree_loc pld_name.loc in
              `Field
                ( loc,
                  (pld_name.txt, Required, [] (* TODO: annotations *)),
                  type_expr_of_core_type loc pld_type ))
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
                        (type_expr_of_core_type loc
                           Parsetree.
                             {
                               ptyp_desc = Ptyp_tuple l;
                               ptyp_loc = pcd_name.loc;
                               ptyp_loc_stack = [ pcd_name.loc ];
                               ptyp_attributes = [];
                             }) )
              | _ ->
                  raise
                    (Invalid_argument
                       (sprintf "unsupported variant types: %s"
                       @@ string_of_type_decl type_decl)))
            l,
          [] (* TODO: annotations *) )
  | _ -> raise Not_implemented

and type_expr_of_core_type loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_var x -> Tvar (loc, x)
  | Ptyp_tuple l ->
      Tuple
        ( loc,
          List.map
            (fun core_type ->
              ( loc,
                type_expr_of_core_type loc core_type,
                [] (* TODO: annotations *) ))
            l,
          [] (* TODO: annotations *) )
  | Ptyp_constr
      ( { txt = Lident "list" (* TODO: qualified names e.g. Module.x *); loc },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      List (loc, type_expr_of_core_type loc ty, [] (* TODO: annotations *))
  | Ptyp_constr
      ( { txt = Lident "option" (* TODO: qualified names e.g. Module.x *); loc },
        [ ty ] (* only supports 1 type in list*) ) ->
      let loc = atd_loc_of_parsetree_loc loc in
      Option (loc, type_expr_of_core_type loc ty, [] (* TODO: annotations *))
  | Ptyp_constr
      ({ txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, _)
    when List.mem id [ "list"; "option" ] ->
      raise
        (Invalid_argument
           (sprintf "%s: has multiple types for %s (invalid)"
              (string_of_core_type core_type)
              id))
  | Ptyp_constr
      ({ txt = Lident id (* TODO: qualified names e.g. Module.x *); loc }, l) ->
      let loc = atd_loc_of_parsetree_loc loc in
      Name
        ( loc,
          (loc, id, List.map (type_expr_of_core_type loc) l),
          [] (* TODO: annotations *) )
  | _ ->
      (* Shard and Wrap doesn't exist anymore *)
      print_endline @@ string_of_core_type core_type;
      unsupported_derivation core_type
